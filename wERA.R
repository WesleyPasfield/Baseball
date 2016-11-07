library(rvest)
library(tidyverse)
library(pitchRx)

setwd('~/Downloads')
players <- read.csv('master.csv', stringsAsFactors = F)
players <- players %>%
  select(mlb_id, mlb_name)

pitchRxScraper <- function(startDate, endDate) {
  dat <- scrape(start = startDate, end = endDate)
  return(dat)
}

dat <- pitchRxScraper('2016-08-09', '2016-08-11')

atbat <- dat[[1]]
run <- dat[[5]]
pr <- dat[[2]]

pinch_runner <- pr %>%
  filter(grepl("Pinch-runner", des)) %>%
  select(gameday_link, player, des, event_num, num, inning) %>%
  arrange(gameday_link, num)

plyaer <- strsplit(pinch_runner$des, 'replaces ')
plyaer <- unlist(plyaer)[2 * (1:length(plyaer))]
pinch_runner$replaced <- gsub("\\..*$", "", plyaer)
pinch_runner <- pinch_runner %>%
  left_join(players, players, by = c("replaced" = "mlb_name")) %>%
  mutate(id = player) %>%
  select(gameday_link, id, inning, mlb_id)

run1 <- run %>%
  mutate(run = ifelse(score == 'T' & earned == 'T', 1, 0)) %>%
  select(id, inning, run, num, gameday_link) %>%
  arrange(gameday_link, num) %>%
  left_join(pinch_runner, by = c("gameday_link" = "gameday_link", "inning" = "inning", "id" = "id")) %>%
  mutate(mlb_id = ifelse(is.na(mlb_id), id, mlb_id)) %>%
  select(gameday_link, run, num, mlb_id, id, inning)

atbat1 <- atbat %>%
  mutate(mlb_id = batter) %>%
  select(pitcher, mlb_id, gameday_link, inning, num) %>%
  left_join(run1, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) 

atbat1$num.y <- ifelse(is.na(atbat1$num.y), atbat1$num.x, atbat1$num.y)

atbat1 <- atbat1 %>%
  filter(num.x <= num.y) %>%
  mutate(run = ifelse(is.na(run), 0, run)) %>%
  arrange(gameday_link, inning, num.x)

pitchAB <- atbat1 %>%
  select(pitcher, inning, gameday_link, num.x)

pitchScore <- atbat1 %>%
  filter(run == 1) %>%
  select(pitcher, inning, gameday_link, num.y) %>%
  mutate(num.x = num.y) %>%
  left_join(pitchAB, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num.x' = 'num.x')) %>%
  select(inning, gameday_link, num.y, pitcher.y)
pitchScore$dups <- paste0(pitchScore$gameday_link, pitchScore$inning, pitchScore$num.y, pitchScore$pitcher.y)
pitchScore$dupsCheck = duplicated(pitchScore$dups)

pitchScore <- pitchScore %>%
  filter(dupsCheck == FALSE) %>%
  mutate(pitcher_score = pitcher.y) %>%
  select(inning, gameday_link, num.y, pitcher_score)

atbatFin <- atbat1 %>%
  left_join(pitchScore, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num.y' = 'num.y')) %>%
  filter(run == 1)

## atbatFin has the pitcher responsible & pitcher pitching for each run scored.

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

pitch <- dat[[3]] %>%
  select(inning, num, event_num, gameday_link, on_3b, on_1b, on_2b) %>%
  arrange(gameday_link, event_num)

pitch$dups <- paste0(pitch$inning, pitch$num, pitch$gameday_link)
pitch$dupcheck <- duplicated(pitch$dups)
  
pitch <- pitch %>%
  filter(dupcheck == F) %>%
  select(inning, num, gameday_link, on_3b, on_1b, on_2b) %>%
  arrange(gameday_link, inning, num) %>%
  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num'))

min_ab <- atbat %>%
  select(pitcher, num, inning_side, gameday_link) %>%
  arrange(gameday_link, inning_side, num) %>%
  mutate(ab_num = row_number()) %>%
  group_by(pitcher, gameday_link, inning_side) %>%
  summarise(min_ab = min(ab_num),
            max_ab = max(ab_num)) %>%
  as.data.frame() %>%
  arrange(gameday_link, inning_side, min_ab)

atbat_lookup <- atbat %>%
  select(gameday_link, inning_side, num) %>%
  arrange(gameday_link, inning_side, num) %>%
  mutate(ab_num = row_number())

first_runners <- min_ab %>%
  left_join(atbat_lookup, by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(pitch, by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_1b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(first_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_1b) %>%
  select(gameday_link, inning, num.x, id, outs, first_run, pitcher_scored, pitcher_leave)

second_runners <- min_ab %>%
  left_join(atbat_lookup, by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(pitch, by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_2b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(second_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_2b) %>%
  select(gameday_link, inning, num.x, id, outs, second_run, pitcher_scored, pitcher_leave)

third_runners <- min_ab %>%
  left_join(atbat_lookup, by = c('gameday_link' = 'gameday_link', 'min_ab' = 'ab_num')) %>%
  left_join(pitch, by = c('gameday_link' = 'gameday_link', 'num' = 'num')) %>%
  left_join(atbatFin, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'on_3b' = 'id')) %>%
  filter(run == 1) %>%
  filter(pitcher_score == pitcher.x) %>%
  mutate(third_run = run, pitcher_leave = pitcher.y, pitcher_scored = pitcher.x, id = on_3b) %>%
  select(gameday_link, inning, num.x, id, outs, third_run, pitcher_scored, pitcher_leave)

## Odds of scoring runs

library(reshape2)
run_odds <- dat[[3]] %>%
  group_by(gameday_link, inning, num, on_3b, on_1b, on_2b) %>%
  summarise(min_event_num = min(event_num)) %>%
  as.data.frame() %>%
  melt(id.vars = c('gameday_link', 'num', 'min_event_num', 'inning'), value.name = 'mlb_id')

run_odds$dups = duplicated(paste0(run_odds$gameday_link, run_odds$num, run_odds$inning, run_odds$variable))

run_odds <- run_odds %>%
  filter(dups == F) %>%
  left_join(select(atbatFin, gameday_link, inning, mlb_id, num.x, num.y, run),  by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) %>%
  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num')) %>%
  arrange(gameday_link, num, variable) %>%
  select(-dups)

run_odds$run <- ifelse(is.na(run_odds$run), 0, run_odds$run)
run_odds$mlb_id <- ifelse(is.na(run_odds$mlb_id), 0, 1)

final_odds <- run_odds %>%
  group_by(variable, mlb_id, outs) %>%
  summarise(count = n(),
            runs = sum(run)) %>%
  as.data.frame() %>%
  filter(mlb_id == 1) %>%
  mutate(score_percentage = runs / count) %>%
  select(-mlb_id)

batter_odds <- dat[[3]] %>%
  group_by(gameday_link, inning, num) %>%
  summarise(max_event_num = max(event_num)) %>%
  as.data.frame() %>%
  left_join(select(atbat1, gameday_link, inning, num.x, mlb_id), by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num.x')) %>%
  left_join(select(atbatFin, gameday_link, inning, mlb_id, run), by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'mlb_id' = 'mlb_id')) %>%
  left_join(atbatOut, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'num' = 'num'))

batter_odds$run <- ifelse(is.na(batter_odds$run), 0, 1)

final_batter_odds <- batter_odds %>%
  group_by(outs) %>%
  summarise(count = n(),
            runs = sum(run)) %>%
  as.data.frame() %>%
  mutate(score_percentage = runs / count, variable = 'empty') %>%
  select(variable, outs, count, runs, score_percentage)

final_odds <- as.data.frame(rbind(final_odds, final_batter_odds)) %>%
  mutate(inherit_pitcher = 1 - score_percentage, replaced_pitcher = score_percentage) %>%
  select(variable, outs, replaced_pitcher, inherit_pitcher)

final_odds

## Inherited runners scored from 1st, 2nd, 3rd & traditional

first_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(first_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_1b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

first_run_sc <- first_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

first_run_lv <- first_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

first_run_fin <- as.data.frame(rbind(first_run_sc, first_run_lv))

second_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(second_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_2b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

second_run_sc <- second_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

second_run_lv <- second_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

second_run_fin <- as.data.frame(rbind(second_run_sc, second_run_lv))

third_run <- atbatFin %>%
  select(pitcher, gameday_link, inning, mlb_id, num.x, id, run) %>%
  left_join(third_runners, by = c('gameday_link' = 'gameday_link', 'inning' = 'inning', 'id' = 'id')) %>%
  filter(num.x.y <= num.x.x) %>%
  left_join(filter(final_odds, variable == 'on_3b'), by = c('outs' = 'outs')) %>%
  select(pitcher_scored, pitcher_leave, replaced_pitcher, inherit_pitcher)

third_run_sc <- third_run %>%
  select(pitcher_scored, inherit_pitcher) %>%
  mutate(pitcher = pitcher_scored, runs = inherit_pitcher) %>%
  select(pitcher, runs)

third_run_lv <- third_run %>%
  select(pitcher_leave, replaced_pitcher) %>%
  mutate(pitcher = pitcher_leave, runs = replaced_pitcher) %>%
  select(pitcher, runs)

third_run_fin <- as.data.frame(rbind(third_run_sc, third_run_lv))

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

final_run <- as.data.frame(rbind(first_run_fin, rbind(second_run_fin, rbind(third_run_fin, trad_run))))

######### NEXT STEPS ################

## Currently have assigment of inhereted runners scored by what base they were on, number of outs, 
## pitcher responsible, & pitcher who gave up the run & odds of scoring

## Calculate total innings pitched for each pitcher (just add outs + innings to min_ab)
## Divide runs by innings & done!

## Also need to pull for all season

############### ADDS ################

## Expected vs. actual runs allowed for RP - instead of just looking at scores for runs, 
## Can create a runs added / subtracted stat by looking at likelihood of inhereted runners getting stranded


