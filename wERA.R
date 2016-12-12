library(rvest)
library(tidyverse)
library(pitchRx)
library(DBI)
library(RPostgreSQL)
library(DT)

## Create Database for all 2016 games using Postgres

## Create DB connection - answer prompts with your relevant info

pg <- dbDriver("PostgreSQL")

db <- dbConnect(pg, 
                user = .rs.askForPassword("Enter username:"), 
                password = .rs.askForPassword("Enter password:"),
                host = 'localhost', 
                port = 5432,
                dbname = .rs.askForPassword("Enter database:"))
scrape(start = '2016-04-03', end = '2016-10-02', connect = db)

## Turn into dataframes

atbat <- dbGetQuery(db, 'select * from atbat')
run <- dbGetQuery(db, 'select * from runner')
pr <- dbGetQuery(db, 'select * from action')
pitchfull <- dbGetQuery(db, 'select * from pitch')

## Close connection

dbDisconnect(db)

### Use CSV lookup table to identify player codes to player names
## Necessary when scraping player names from descriptions
## Is there a better way to do this ???? ##

setwd('~/Downloads')
players <- read.csv('master.csv', stringsAsFactors = F)
players <- players %>%
  select(mlb_id, mlb_name, espn_pos) %>%
  filter(mlb_id != 466412) ## Filter out other Luis Perdomo

## Pull out pinch runner information for run assignment
## From action db - for purposes of replacing original batter with new runner

pinch_runner <- pr %>%
  filter(grepl("Pinch-runner", des)) %>%
  select(gameday_link, player, des, event_num, num, inning) %>%
  arrange(gameday_link, num)

## String manipulation to get data in the proper format

plyaer <- strsplit(pinch_runner$des, 'replaces ')
plyaer <- unlist(plyaer)[2 * (1:length(plyaer))]
pinch_runner$replaced <- gsub("\\..*$", "", plyaer)
pinch_runner <- pinch_runner %>%
  left_join(players, players, by = c("replaced" = "mlb_name")) %>%
  mutate(id = player) %>%
  select(gameday_link, id, inning, mlb_id)

## Join created pinch runner database with actual runner database 
## To replace original batter with pinch runner

run1 <- run %>%
  mutate(run = ifelse(score == 'T' & earned == 'T', 1, 0)) %>%
  select(id, inning, run, num, gameday_link) %>%
  arrange(gameday_link, num) %>%
  left_join(pinch_runner, by = c("gameday_link" = "gameday_link", "inning" = "inning", "id" = "id")) %>%
  mutate(mlb_id = ifelse(is.na(mlb_id), id, mlb_id)) %>%
  select(gameday_link, run, num, mlb_id, id, inning)

## Join that information with batter information to assign pitcher
## Who allowed the run, and pitcher "responsible" for batter

## Probably an opportunity to optimize this portion, notably to remove excess joins
## A bit of stream of consciousness code here

atbat1 <- atbat %>%
  mutate(mlb_id = batter) %>%
  select(pitcher, mlb_id, gameday_link, inning, num) %>%
  left_join(run1, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) 

atbat1$num.y <- ifelse(is.na(atbat1$num.y), atbat1$num.x, atbat1$num.y)

## Identify situation where run scored

atbat1 <- atbat1 %>%
  filter(num.x <= num.y) %>%
  mutate(run = ifelse(is.na(run), 0, run)) %>%
  arrange(gameday_link, inning, num.x)

pitchAB <- atbat1 %>%
  select(pitcher, inning, gameday_link, num.x)

pitchScore <- atbat1 %>%
  filter(run == 1) %>%
  select(pitcher, mlb_id, inning, gameday_link, num.x, num.y) %>%
  left_join(pitchAB, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num.y' = 'num.x')) %>%
  mutate(sorter = num.y - num.x) %>%
  arrange(gameday_link, sorter) %>%
  select(inning, mlb_id, gameday_link, num.x, num.y, pitcher.y)
pitchScore$dups <- paste0(pitchScore$gameday_link, pitchScore$num.x, pitchScore$inning, pitchScore$pitcher.y)
pitchScore$dups2 <- paste0(pitchScore$gameday_link, pitchScore$mlb_id, pitchScore$num.y)
pitchScore$dupsCheck = duplicated(pitchScore$dups)
pitchScore$dupsCheck2 = duplicated(pitchScore$dups2)

## Filter out duplicates from the join - occurs if runner scores twice in the inning

pitchScore <- pitchScore %>%
  filter(dupsCheck == FALSE & dupsCheck2 == FALSE) %>%
  mutate(pitcher_score = pitcher.y) %>%
  select(inning, gameday_link, num.x, num.y, pitcher_score)

## atbatFin has the pitcher responsible & pitcher pitching for each run scored.

atbatFin <- atbat1 %>%
  left_join(pitchScore, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num.x' = 'num.x', 'num.y' = 'num.y')) %>%
  filter(run == 1, !is.na(pitcher_score))

## Pull number of outs associated with the start of each atbat

atbatOut <- atbat %>%
  select(gameday_link, num, inning, inning_side, o) %>%
  arrange(gameday_link, num, inning)

atbatOut$outs <- ifelse(is.na(lag(atbatOut$num)), 0,
                 ifelse(lag(atbatOut$gameday_link) != atbatOut$gameday_link, 0,
                 ifelse(lag(atbatOut$num) > atbatOut$num, atbatOut$o,
                 ifelse(lag(atbatOut$inning_side) != atbatOut$inning_side, 0, 
                 lag(atbatOut$o)))))
atbatOut <- atbatOut %>%
  select(gameday_link, inning, num, outs)

## Pull runner information for each at-bat & join with atbat info

pitch <- pitchfull %>%
  select(inning, num, event_num, gameday_link, on_3b, on_1b, on_2b) %>%
  arrange(gameday_link, event_num)

pitch$dups <- paste0(pitch$inning, pitch$num, pitch$gameday_link)
pitch$dupcheck <- duplicated(pitch$dups)
  
pitch <- pitch %>%
  filter(dupcheck == F) %>%
  select(inning, num, gameday_link, on_3b, on_1b, on_2b) %>%
  arrange(gameday_link, inning, num) %>%
  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num'))

## Pull the min & max AB associated with each pitcher in each game

min_ab <- atbat %>%
  select(pitcher, num, inning_side, gameday_link) %>%
  arrange(gameday_link, inning_side, num) %>%
  mutate(ab_num = row_number()) %>%
  group_by(pitcher, gameday_link, inning_side) %>%
  summarise(min_ab = min(ab_num),
            max_ab = max(ab_num)) %>%
  as.data.frame() %>%
  arrange(gameday_link, inning_side, min_ab)

## Create a lookup that converts min_ab & max_ab to relevant game num

atbat_lookup <- atbat %>%
  select(gameday_link, inning_side, inning, num, o, inning) %>%
  arrange(gameday_link, inning_side, num) %>%
  mutate(ab_num = row_number())

## Identify runners that scored from 1st that were inherited

first_runners <- min_ab %>%
  left_join(select(atbat_lookup, gameday_link, ab_num, num, o, inning), by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(select(pitch, gameday_link, num, on_3b, on_1b, on_2b, outs), by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_1b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(first_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_1b) %>%
  select(gameday_link, inning, num.x, id, outs, first_run, pitcher_scored, pitcher_leave)

## Identify runners that scored from 2nd that were inherited

second_runners <- min_ab %>%
  left_join(select(atbat_lookup, gameday_link, ab_num, num, o, inning), by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(select(pitch, gameday_link, num, on_3b, on_1b, on_2b, outs), by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_2b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(second_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_2b) %>%
  select(gameday_link, inning, num.x, id, outs, second_run, pitcher_scored, pitcher_leave)

## Identify runners that scored from 3rd that were inherited

third_runners <- min_ab %>%
  left_join(select(atbat_lookup, gameday_link, ab_num, num, o, inning), by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(select(pitch, gameday_link, num, on_3b, on_1b, on_2b, outs), by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_3b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(third_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_3b) %>%
  select(gameday_link, inning, num.x, id, outs, third_run, pitcher_scored, pitcher_leave)



## Odds of scoring runs from each out/base combination
## Convert those odds to ERA assignment

## Pull every situation from the 2016 season based on situation at start of atbat

library(reshape2)
run_odds <- pitchfull %>%
  group_by(gameday_link, inning, num, on_3b, on_1b, on_2b) %>%
  summarise(min_event_num = min(event_num)) %>%
  as.data.frame() %>%
  melt(id.vars = c('gameday_link', 'num', 'min_event_num', 'inning'), value.name = 'mlb_id')

## Remove duplicate situations

run_odds$dups <- duplicated(paste0(run_odds$gameday_link, run_odds$num, run_odds$inning, run_odds$variable))

## Join with atbat outcome information

run_odds <- run_odds %>%
  filter(dups == F) %>%
  left_join(select(atbatFin, gameday_link, inning, mlb_id, num.x, num.y, run),  by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) %>%
  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num')) %>%
  filter(num.y >= num | is.na(num.y)) %>% ## Filter out runners who scored twice in an inning
  filter(num.x <= num | is.na(num.x)) %>% ## Filter out runners who scored twice in an inning
  arrange(gameday_link, num, variable) %>%
  select(-dups)

## Identify when a run is scored, and whether it was inherited

run_odds$run <- ifelse(is.na(run_odds$run), 0, run_odds$run)
run_odds$mlb_id <- ifelse(is.na(run_odds$mlb_id), 0, 1)

## Remove weird event from end of Royals & White Sox 5/20 game
## Everything gets screwed up when someone gets ejected with 3 outs in the 9th
## Not sure what the cause is.

run_odds$dups <- duplicated(paste0(run_odds$gameday_link, run_odds$num, run_odds$inning, run_odds$variable))
run_odds <- filter(run_odds, dups == F)
run_odds <- filter(run_odds, outs <= 2)

## Calculate odds of scoring in each situation

final_odds <- run_odds %>%
  group_by(variable, mlb_id, outs) %>%
  summarise(count = n(),
            runs = sum(run)) %>%
  as.data.frame() %>%
  filter(mlb_id == 1) %>%
  mutate(score_percentage = runs / count) %>%
  select(-mlb_id) %>%
  mutate(inherit_pitcher = 1 - score_percentage, replaced_pitcher = score_percentage) %>%
  select(variable, outs, replaced_pitcher, inherit_pitcher) 

rounder <- function(var) {
  v <- round(var, 2)
  return(v)
}  
  
final_odds[,3:4] <- sapply(final_odds[,3:4], rounder)

## Use for future expected vs. actual runs calc

#batter_odds <- pitch %>%
#  group_by(gameday_link, inning, num) %>%
#  summarise(max_event_num = max(event_num)) %>%
#  as.data.frame() %>%
#  left_join(select(atbat1, gameday_link, inning, num.x, mlb_id), by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num.x')) %>%
#  left_join(select(atbatFin, gameday_link, inning, mlb_id, run), by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) %>%
#  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num'))

#batter_odds$run <- ifelse(is.na(batter_odds$run), 0, 1)

#final_batter_odds <- batter_odds %>%
#  group_by(outs) %>%
#  summarise(count = n(),
#            runs = sum(run)) %>%
#  as.data.frame() %>%
#  mutate(score_percentage = runs / count, variable = 'empty') %>%
#  select(variable, outs, count, runs, score_percentage)

#final_odds <- as.data.frame(rbind(final_odds, final_batter_odds)) %>%
#  mutate(inherit_pitcher = 1 - score_percentage, replaced_pitcher = score_percentage) %>%
#  select(variable, outs, replaced_pitcher, inherit_pitcher)

############### Run assignment #######################

## Inherited runners scored from 1st, 2nd, 3rd & traditional (no inheritance)

## Runner from first

first_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(first_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_1b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

## Assignment for pitcher who got scored on - runner on first

first_run_sc <- first_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

## Assignment for pitcher who allowed runner on base - runner on first

first_run_lv <- first_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

## Stack into one DF

first_run_fin <- as.data.frame(rbind(first_run_sc, first_run_lv))

## Runner from second

second_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(second_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_2b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

## Assignment for pitcher who allowed run to score from 2nd

second_run_sc <- second_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

## Assignment for pitcher who allowed runner to reach second

second_run_lv <- second_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

## Runner from second data stacked

second_run_fin <- as.data.frame(rbind(second_run_sc, second_run_lv))

## Runner from Third

third_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(third_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_3b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

## Assignment for pitcher who allowed runner to score from third

third_run_sc <- third_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

## Assignment for pitcher who allowed runner to get to third

third_run_lv <- third_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

## Runner on third stacked

third_run_fin <- as.data.frame(rbind(third_run_sc, third_run_lv))

## Traditional runner scored - no inheritance

trad_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(first_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(is.na(first_run)) %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x.x, id, run) %>%
  left_join(second_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(is.na(second_run)) %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x.x, id, run) %>%
  left_join(third_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(is.na(third_run)) %>%
  mutate(runs = run) %>%
  select(pitcher, runs)
  
## Stack all run scored information together

final_run <- as.data.frame(rbind(first_run_fin, rbind(second_run_fin, rbind(third_run_fin, trad_run))))

## Pull innings pitched for ERA calculation

## Calculate out/inning combination at the start & end of each pitchers appearance

ip_lookup <- atbat_lookup
ip_lookup$start <- atbat_lookup$inning + (ifelse(is.na(lag(ip_lookup$o)), 0,
                      ifelse(lag(ip_lookup$gameday_link) != ip_lookup$gameday_link, 0, 
                      ifelse(lag(ip_lookup$inning) != ip_lookup$inning, 0,
                      ifelse(lag(ip_lookup$inning_side) != ip_lookup$inning_side, 0,
                      lag(ip_lookup$o))))) /3) - 1
ip_lookup$finish <- atbat_lookup$inning + ip_lookup$o / 3 - 1
ip_lookup <- ip_lookup %>%
  select(ab_num, start, finish)

ip <- min_ab %>%
  left_join(select(ip_lookup, start, ab_num), by = c('min_ab' = 'ab_num')) %>%
  left_join(select(ip_lookup, finish, ab_num), by = c('max_ab' = 'ab_num')) %>%
  mutate(ip = finish - start) %>%
  select(pitcher, ip)

ip$mid_inning <- ifelse(as.integer(ip$ip) - ip$ip != 0, 1, 0)

ip <- ip %>%
  group_by(pitcher) %>%
  summarise(ip = sum(ip),
            appearances = n(),
            mid_inning = sum(mid_inning))%>%
  as.data.frame()

## Calculate traditional ERA

reg_calc <- atbatFin %>%
  select(pitcher, run) %>%
  group_by(pitcher) %>%
  summarise(regular_runs = sum(run)) %>%
  as.data.frame()

## Calculate inherited ERA

all_runs <- final_run %>%
  group_by(pitcher) %>%
  summarise(runs = sum(runs)) %>%
  as.data.frame() %>%
  left_join(ip, by = c('pitcher' = 'pitcher')) %>%
  left_join(reg_calc, by = c('pitcher' = 'pitcher')) %>%
  mutate(regular_era = (regular_runs / ip) * 9) %>%
  mutate(inherited_era = (runs / ip) * 9) %>%
  left_join(players, by = c('pitcher' = 'mlb_id'))

## Change missing values to 0s & calculate ER assignment change from regular

all_runs[is.na(all_runs)] <- 0
all_runs$run_change <- all_runs$runs - all_runs$regular_runs

## Sort SP by most weighted inherited runners allowed

all_runs_sp_victims <- all_runs %>%
  filter(espn_pos == 'SP') %>%
  filter(ip > 100) %>%
  filter(run_change <= 0) %>%
  mutate(era_change = inherited_era - regular_era) %>%
  arrange(run_change) %>%
  mutate(wERA_runs = runs, wERA = inherited_era) %>%
  select(mlb_name, ip, regular_runs, wERA_runs, run_change, regular_era, wERA, era_change)

## Sort RP by most weighted inherited runners allowed

all_runs_rp_lucky <- all_runs %>%
  filter(espn_pos == 'RP' & ip > 20) %>%
  filter(run_change >= 0) %>%
  mutate(era_change = inherited_era - regular_era) %>%
  arrange(-run_change) %>%
  mutate(wERA_runs = runs, wERA = inherited_era) %>%
  select(mlb_name, ip, regular_runs, wERA_runs, run_change, regular_era, wERA, era_change)

all_runs_sp_victims[,c(2,4:length(all_runs_sp_victims))] <- sapply(all_runs_sp_victims[,c(2,4:length(all_runs_sp_victims))], rounder)
all_runs_rp_lucky[,c(2,4:length(all_runs_rp_lucky))] <- sapply(all_runs_rp_lucky[,c(2,4:length(all_runs_sp_victims))], rounder)

datatable(head(all_runs_sp_victims, 10))
datatable(head(all_runs_rp_lucky, 10))

library(ggplot2)

ggplot(data = all_runs_rp_lucky, mapping = aes(x = regular_era, y = wERA)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0,10)) +
  scale_x_continuous(name = 'Regular ERA', breaks = c(1,2,3,4,5,6,7,8)) +
  scale_y_continuous(name = 'wERA', breaks = c(1,2,3,4,5,6,7,8,9,10))



############### ADDS ################

## Expected vs. actual runs allowed for RP - instead of just looking at scores for runs, 
## Can create a runs added / subtracted stat by looking at likelihood of inhereted runners getting stranded

##### Major miss - pitchers who don't allow runner on base OR runner to score. 
## Calc change in probability of scoring as assignment??

