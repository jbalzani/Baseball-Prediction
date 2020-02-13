####some question wording may be from HarvardX staff

#create linear model predicting runs per game based on bb/game and hr/game
#what is coeff of bb/game?
library(Lahman)
library(dslabs)
library(tidyverse)

data(Teams)
names(Teams)
Teams_filtered <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game = R/G, bb_per_game = BB / G, hr_per_game = HR / G) %>%
  select(runs_per_game, bb_per_game, hr_per_game)
model <- lm(runs_per_game ~ bb_per_game + hr_per_game, data = Teams_filtered)
summary(model)

#how many players have walk rate, singles rate above 0.2 per plate appearance
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarize(msingles = mean(singles), mbb = mean(bb))

bat_99_to_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

avg <- bat_99_to_01 %>% group_by(playerID) %>%
  summarize(msingles = mean(singles), mbb = mean(bb)) %>%
  ungroup() %>%
  select(playerID, msingles, mbb)
avg %>%  filter(msingles > 0.2) %>% nrow()
avg %>% filter(mbb > 0.2) %>% nrow()

#join 02 with 99-01
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(msingles = mean(singles), mbb = mean(bb))

t2 <- inner_join(bat_02, bat_99_01)
cor(t2$msingles, t2$mean_singles)
cor(t2$mbb, t2$mean_bb)
head(t2)

#scatterplot of mean_singles vs singles and mean_bb vs bb
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

t3 <- inner_join(bat_02, bat_99_01)
t3 %>% ggplot(aes(x = mean_singles, y = singles)) +
  geom_point()
t3 %>% ggplot(aes(x = mean_bb, y = bb)) + 
  geom_point()
which.max(t3$bb)
t3$playerID[33]
t3 %>% distinct(playerID, mean_singles, mean_bb, .keep_all = FALSE) %>% 
  arrange(desc(mean_bb))
t3 %>% distinct(playerID, mean_singles, mean_bb, .keep_all = FALSE) %>% 
  arrange(desc(mean_singles)) #distinct removes duplicates

#q12
#fit a linear model to predict 2002 singles given 99-01 mean singles
t4 <- inner_join(bat_02, bat_99_01)
t4 %>% lm(t4$singles ~ t4$mean_singles, data = .)
#fit a linear model to predict 02 bb given 99-01 mean_bb
t4 %>% lm(bb ~ mean_bb, data = .)

#run lm R~BB for strata of HR, add 3 new cols to grouped tibble: slope, se, pvalue
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

#does relationship between HR and runs vary by league?
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

library(tidyverse)
library(Lahman)
library(broom)
Teams %>% filter(yearID == 1971) %>% #lm  for 1971
  do(tidy(lm(R~HR+BB, data = .)))

Teams %>% filter(yearID == 1971) %>% #calling slope coeff
  lm(R~BB, data = .) %>%
  .$coef %>%
  .[2]

#find effect of BB & HR on R for each year in 1961 to 2018
BBHR_61to18 <- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .)))
BBHR_61to18

BB_61to18 <- BBHR_61to18 %>%
  filter(term == "BB")
BB_61to18

#make scatterplot of the effect of BB on runs over the years and add trend line
BB_61to18 %>% ggplot(aes(x = yearID, y = estimate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_abline()

#fit linear model on above to determine effect of year on impact of BB on R
b <- BB_61to18 %>%
  lm(estimate~yearID, data = .) 
summary(b)

#attendance analysis
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#use runs per game to predict average attendance
Teams_small2 <- Teams_small %>%
  mutate(runs_per_game = R/G, HR_per_game = HR/G)

lm(avg_attendance~runs_per_game, data = Teams_small2)

#use HR per game to predict avg attendance
lm(avg_attendance~HR_per_game, data = Teams_small2)

#use wins to predict avg attendance
lm(avg_attendance~W, data = Teams_small2)

#use year to predict avg attendance
lm(avg_attendance~yearID, data = Teams_small2)

#correlation coef for wins and runs per game
cor(Teams_small2$W, Teams_small2$runs_per_game)
#correlation coef for wins and HR per game
cor(Teams_small2$W, Teams_small2$HR_per_game)

#stratify Teams_small by wins, divide number of wins by 10 and round to nearest integer
#keep only strata 5 thru 10 (with values 5-10), which have 20 or more data points
df_win_strata <- Teams_small2 %>% 
  mutate(win_strata = round(W/10)) %>%
  group_by(win_strata) %>%
  filter(n() >= 20) %>%
  filter(win_strata >= 5 & win_strata <= 10)
head(df_win_strata)

# number of observation of strata value 8
df_win_strata %>% filter(win_strata == 8) %>%
  nrow()

#model avg attendance by runs per game for each strata
df_win_strata %>% group_by(win_strata) %>%
  do(tidy(lm(avg_attendance~runs_per_game, data = .))) #must have do or it ignores group_by

#model avg attendance by HR per game for each strata
df_win_strata %>% group_by(win_strata) %>%
  do(tidy(lm(avg_attendance~HR_per_game, data = .)))

#multivariate regression for effect of runs per game, HR per game, wins, and 
#year on avg attendance
model3 <- tidy(lm(avg_attendance~runs_per_game + HR_per_game + W + yearID, 
                  data = Teams_small2))
model3
#team averages 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
#avg attendance in 2002 and 1960
model3 %>% summarize(att_02 = .$estimate[1] + .$estimate[2]*5 + .$estimate[3]*1.2 +
                       .$estimate[4]*80 + .$estimate[5]*2002)
model3 %>% summarize(att_60 = .$estimate[1] + .$estimate[2]*5 + .$estimate[3]*1.2 +
                       .$estimate[4]*80 + .$estimate[5]*1960)
model4 <- lm(avg_attendance~runs_per_game + HR_per_game + W + yearID, 
             data = Teams_small2)
#predict avg attendnace in 2002 in original Teams df. correlation b/w predicted & acutal?
data(Teams)
Teams2 <- Teams %>% filter(yearID %in% 2002) %>%
  mutate(avg_attendance = attendance/G, 
         runs_per_game = R/G, 
         HR_per_game = HR/G) %>%
  mutate(pred_att_02 = predict(model4, newdata = .))
Teams2
cor(Teams2$pred_att_02, Teams2$avg_attendance)