library(dplyr)

## Statcast ##

setwd('~/Downloads')
dn <- read.csv('savant_data.csv', stringsAsFactors = F)
dn2 <- read.csv('savant_data (1).csv', stringsAsFactors = F) 
dn3 <- read.csv('savant_data (2).csv', stringsAsFactors = F) 
dn4 <- read.csv('savant_data (3).csv', stringsAsFactors = F) 
dn5 <- read.csv('savant_data (4).csv', stringsAsFactors = F) 
dn6 <- read.csv('savant_data (5).csv', stringsAsFactors = F) 

dnTotal <- as.data.frame(rbind(dn, rbind(dn2, rbind(dn3, rbind(dn4, rbind(dn5, dn6))))))


dnSelect <- dnTotal %>%
  select(pitch_type, pitch_id, game_date, game_year, start_speed, events, description, spin_rate, 
         break_angle, break_length, zone, hit_location, bb_type, p_throws, balls, 
         strikes, outs_when_up, on_3b, on_2b, on_1b, inning_topbot, hit_distance_sc, hit_speed,
         hit_angle, release_spin_rate, release_extension) %>%
  filter(!hit_speed %in% c('0.0', '0', 'null')) %>%
  filter(!hit_distance_sc == 'null') %>%
  mutate(game_date = as.Date(game_date, '%Y-%m-%d'), 
         pitch_type = as.factor(pitch_type), 
         description = as.factor(description),
         hit_speed = as.numeric(hit_speed),
         hit_distance_sc = as.numeric(hit_distance_sc),
         hit_angle = as.numeric(hit_angle)) %>%
  arrange(game_date, pitch_id)


library(DescTools)
options(scipen = 999)

Desc(game_year ~ as.factor(hit_location), data = dnSelect)
Desc(game_year ~ hit_speed, data = dnSelect) ## nothing really  
Desc(game_year ~ hit_distance_sc, data = dnSelect) ## Hmm further
Desc(game_year ~ hit_angle, data = dnSelect) ## Bingo - pop ups

### PitchRx

dnAtBat <- dbGetQuery(db, 'select * from atbat where batter = 519083') %>%
  select(batter, num, event, event_num, gameday_link, o, p_throws)
head(atbat)
dnPitch <- dbGetQuery(db, "select * from pitch") %>%
  select(des, type, start_speed, end_speed, break_angle, break_length, pitch_type, count,
         zone, spin_dir, spin_rate, inning_side, num, on_1b, on_2b, on_3b, gameday_link, event_num, play_guid)

dnPitchFin <- dnAtBat %>%
  inner_join(dnPitch, by = c('gameday_link' = 'gameday_link', 'num' = 'num'))  %>%
  mutate(year = substr(gameday_link, 5,8))

Desc(year ~ as.factor(zone), data = dnPitchFin)
dnOutcomeFin <- filter(dnPitchFin, des %in% c('In play, out(s)', 
                                              'In play, no out',
                                              'In play, run(s)',
                                              'Hit By Pitch') |
                                   type == 'S' & 
                                   !des %in% c('Foul', 'Foul (Runner Going)') &
                                   count %in% c('0-2', '1-2', '2-2', '3-2') |
                                   type == 'B' &
                                   count %in% c('3-0', '3-1', '3-2'))

## Create variable for outcome variable for chased vs. non-chased pitches

dnOutcomeFin$chase <- ifelse(dnOutcomeFin$zone > 9, 1, 0)
unique(dnOutcomeFin$des)

allOutcomeTable <- round(prop.table(table(dnOutcomeFin$year, dnOutcomeFin$chase), 1), 3)
colnames(allOutcomeTable) <- c('Ball', 'Strike')
allOutcomeTable

## Create variable for chased vs non chased on strikeouts

dnOutcomeK <- filter(dnOutcomeFin, event %in% c('Strikeout', 'Strikeout - DP') 
                     & des %in% c('Swinging Strike (Blocked)', 'Swinging Strike'))

Desc(year ~ as.factor(zone), data = dnOutcomeK)

dnOutcomeK <- round(prop.table(table(dnOutcomeK$year, dnOutcomeK$chase), 1), 3)
colnames(dnOutcomeK) <- c('Ball', 'Strike')
dnOutcomeK

## Nothing really there above - let's look at all strikeouts by pitch type

dnOutcomeAllK <- filter(dnOutcomeFin, event %in% c('Strikeout', 'Strikeout - DP') )
colnames(dnPitchFin)
Desc(year ~ as.factor(zone), data = dnOutcomeAllK)

## Nothing really jumps out there either - let's look by pitch type

Desc(year ~ as.factor(pitch_type), data = dnOutcomeAllK)

## Really no difference there - what about on all pitches, ability to hit strikes

zone <- round(prop.table(table(dnPitchFin$year, as.factor(dnPitchFin$zone)), 1), 3)
zone <- as.data.frame(zone)
colnames(zone) <- c('Year', 'Zone', 'Perc_Frequency')

ggplot(data = zone, mapping = aes(x = Zone, y = Perc_Frequency, group = Year, color = Year)) +
  geom_point() +
  geom_line() +
  labs(title='Pitch Frequency by Zone') +
  theme(plot.title = element_text(size = rel(2), 
                                  color = 'blue', 
                                  hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

## Looks like pitchers are attacking him higher in the zone
pitchtype <- round(prop.table(table(dnPitchFin$year, as.factor(dnPitchFin$pitch_type)), 1), 3)
pitchtype <- as.data.frame(pitchtype)
colnames(pitchtype) <- c('Year', 'Pitch_Type', 'Perc_Frequency')

ggplot(data = pitchtype, mapping = aes(x = Pitch_Type, y = Perc_Frequency, group = Year, color = Year)) +
  geom_point() +
  geom_line() +
  labs(title='Pitch Frequency by Pitch Type') +
  theme(plot.title = element_text(size = rel(2), 
                                  color = 'blue', 
                                  hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

## Not a massive difference - looks like more 2 & 4 seamers, less sinkers
## Which would help explain change in zones

## Look at count by year - Nothing really there

DN2015 <- filter(dnPitchFin, year == 2015)
DN2016 <- filter(dnPitchFin, year == 2016)

## Look at success when attacked up in the zone

dnOutcomeFin$upInZone <- ifelse(dnOutcomeFin$zone %in% c(11,12), "Yes", "No")
dnOutcomeFin$success <- ifelse(dnOutcomeFin$event %in% c('Single', 'Double', 'Home Run', 'Triple'), "Success", "Failure")
dnOutcomeInPlay <- filter(dnOutcomeFin, !event %in% c('Walk', 'Hit By Pitch', 'Sac Fly'))

## Create DFs for just 2015 & 2016 outcomes

dn2016Outcome <- filter(dnOutcomeFin, year == 2016)
dn2015Outcome <- filter(dnOutcomeFin, year == 2015)

## Visualize difference in success by outcome type

dn2015Outcomept <- round(prop.table(table(dn2015Outcome$upInZone, dn2015Outcome$success), 1), 3)
dn2015Outcomept <- as.data.frame(dn2015Outcomept)
colnames(dn2015Outcomept) <- c('Up_In_Zone', 'Success', 'Perc_Frequency')
dn2015Outcomept$Year <- '2015'
dn2016Outcomept <- round(prop.table(table(dn2016Outcome$upInZone, dn2016Outcome$success), 1), 3)
dn2016Outcomept <- as.data.frame(dn2016Outcomept)
colnames(dn2016Outcomept) <- c('Up_In_Zone', 'Success', 'Perc_Frequency')
dn2016Outcomept$Year <- '2016'
dnBothOutcomept <- as.data.frame(rbind(dn2015Outcomept, dn2016Outcomept)) %>%
  filter(Success == 'Success')

## Success Rate Not Up in the Zone vs. Up In the Zone 

ggplot(data = dnBothOutcomept, mapping = aes(x = Up_In_Zone, y = Perc_Frequency, group = Year, fill = Year)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title='Success Rate - Up & Out of the Zone vs. All Other') +
  theme(plot.title = element_text(size = rel(2), 
                                  color = 'blue', 
                                  hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

## Frequencies shown in raw format

table(dn2015Outcome$upInZone, dn2015Outcome$success)
table(dn2016Outcome$upInZone, dn2016Outcome$success)

