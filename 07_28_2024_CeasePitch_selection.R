library(scales)
library(tidyverse)
library(Lahman)
library(baseballr)


#Dylan Cease no hitter 

df_cease = `savant_data.(2)`

#Pitch selection
percent_pt = df_cease %>% 
  group_by(pitch_type) %>% 
  summarise(n_of_pitches = n()/114)
#changing to % 
percent_pt = percent_pt %>% 
  mutate(Labels_pct = percent(percent_pt$n_of_pitches, 4))
  
#Pie Chart of Pitch Selection
ggplot(percent_pt, aes(x = "", y = n_of_pitches, fill = pitch_type)) +
  geom_col(color = "black")+
  geom_text(aes(label = Labels_pct),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") +
  theme_void()

#Pitch type grouped by events
event_pt = df_cease %>% 
  group_by(pitch_type, events) %>% 
  summarise(n_events_pt = n())

#Pitch type grouped by zone
zone_pt = df_cease %>% 
  group_by(pitch_type, zone) %>% 
  summarise(n_pt_zone= n())

Labels_1_3 = c("Inning1_3","Inning1_3","Inning1_3")
Labels_4_6 = c("Inning4_6","Inning4_6","Inning4_6")
Labels_7_9 = c("Inning7_9","Inning7_9","Inning7_9")

#Pitch Type grouped by inning
inning_pt = df_cease %>% 
  group_by(pitch_type) %>% 
  filter(inning >= 1 & inning <= 3) %>% 
  summarise(n_pt_inning_1 = n()/inning_pt_totals$pitch_usage[1]) %>% 
  mutate(Group = Labels_1_3)

inning_pt2 = df_cease %>% 
  group_by(pitch_type) %>% 
  filter(inning >= 4 & inning <= 6) %>% 
  summarise(n_pt_inning_1 = n()/inning_pt_totals$pitch_usage[2]) %>% 
  mutate(Group = Labels_4_6)

inning_pt3 = df_cease %>% 
  group_by(pitch_type) %>% 
  filter(inning >= 7 & inning <= 9) %>% 
  summarise(n_pt_inning_1 = n()/inning_pt_totals$pitch_usage[3]) %>% 
  mutate(Group = Labels_7_9)

#Concatenate every inning grouping of pitches
inning_pt_concat = rbind(inning_pt, inning_pt2, inning_pt3)

#Change column of pitch usage by inning into percents
inning_pt_concat = inning_pt_concat %>% 
  mutate(Labels = percent(n_pt_inning_1),4)
  

#Calculate pitch usage by inning grouping (denominator for percents)
inning_pt_totals = inning_pt_concat %>% 
  group_by(Group) %>% 
  summarise(pitch_usage = sum(n_pt_inning_1))

#Pie chart for usage by inning grouping
ggplot(inning_pt_concat, aes(x = "", y = n_pt_inning_1, fill = pitch_type)) +
  geom_col(color = "black")+
  geom_text(aes(label = Labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") +
  facet_grid(~Group) +
  theme_void()



####### Acquiring Pitch Count Data
balls_strikes = df_cease %>% 
  group_by(balls, strikes, pitch_type) %>% 
  summarise(total_BBs_Ks = n())

#Total pitches thrown by pitch count (denominator for percentages)
total_count_pt = balls_strikes %>% 
  group_by(balls, strikes) %>% 
  summarise(total_pitches = sum(total_BBs_Ks))
#Aggregate all data for pitchers counts (0-1, 0-2, 1-2)
pitchers_count_pt = balls_strikes %>% 
  filter((balls == 1 & strikes == 2)| (balls == 0 & strikes == 1)| (balls == 0 & strikes == 2))
#Aggregate all data for hitters counts (1-0, 2-0, 3-0, 2-1, 3-1)
hitters_count_pt = balls_strikes %>% 
  filter((balls == 1 & strikes == 0)| (balls == 2 & strikes == 0)| (balls == 3 & strikes == 0)|
         (balls == 2 & strikes == 1)| (balls == 3 & strikes == 1))
#Aggregate all data for neutral counts (0-0, 1-1, 2-2, 3-2)
neutral_count_pt = balls_strikes %>% 
  filter((balls == 0 & strikes == 0)| (balls == 1 & strikes == 1)| (balls == 2 & strikes == 2) | (balls == 3 & strikes== 2) )
#Calculate percentages for all pitchers counts by pitch type
pitchers_total = pitchers_count_pt %>% 
  group_by(pitch_type) %>% 
  summarise(pitches = sum(total_BBs_Ks))

#Calculate percentages for all hitters counts by pitch type
hitters_total = hitters_count_pt %>% 
  group_by(pitch_type) %>% 
  summarise(pitches = sum(total_BBs_Ks))

#Calculate percentages for all neutral counts by pitch type
neutral_total = neutral_count_pt %>% 
  group_by(pitch_type) %>% 
  summarise(pitches = sum(total_BBs_Ks)) 

#adding percentages to total counts
neutral_total = neutral_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4))

pitchers_total = pitchers_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4))

hitters_total = hitters_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4))

#Making facet labels
neutral_facet = c("neutral count","neutral count","neutral count")
hitters_facet = c("hitters count","hitters count","hitters count")
pitchers_facet = c("pitchers count","pitchers count","pitchers count")

#adding facet labels to "totals" data frames
neutral_total = neutral_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4)) %>% 
  mutate(facet = neutral_facet)

pitchers_total = pitchers_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4)) %>% 
  mutate(facet = pitchers_facet)

hitters_total = hitters_total %>% 
  mutate(values = pitches/sum(pitches)) %>% 
  mutate(labels = percent(pitches/sum(pitches), 4)) %>% 
  mutate(facet = hitters_facet)

#Aggregate all totals data frames into one concatenated 
count_concat_pt = rbind(neutral_total, pitchers_total, hitters_total)

#Pie Chart for Count data
ggplot(count_concat_pt, aes(x = "", y = values, fill = pitch_type)) +
  geom_col(color = "black")+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") +
  facet_grid(~facet) +
  theme_void()

#Count the batted ball events by outcome

events_count = df_cease %>% 
  filter(events != "") %>% 
  group_by(events) %>% 
  summarise(events_dist = n())
  

######Plate Discipline

##Contact%

#Amount of Swings
total_swings = df_cease %>% 
  filter((description == 'foul') | (description == 'foul_tip') | 
           (description == 'hit_into_play') |(description == 'swinging_strike_blocked')|
           (description == 'swinging_strike')) %>% 
  summarise(n())

#Contact % calculation
contact_pct = df_cease %>% 
  filter((description == 'foul') | (description == 'foul_tip') | 
           (description == 'hit_into_play')) %>% 
  summarise(n()/total_swings)

#Whiff%
whiff_pct = df_cease %>% 
  filter((description == 'swinging_strike') |(description == 'swinging_strike_blocked')|
           (description == 'foul_tip')) %>% 
  summarise(n()/total_swings)

## In-zone plate discipline

#In-zone swing total
total_zone_swings = df_cease %>% 
  filter((!is.na(swing_length)) & ((zone == 1) | (zone == 2) | 
            (zone == 3) | (zone == 4) | (zone == 5) | (zone == 6 ) | (zone == 7)
             | (zone == 8) | (zone == 9))) %>% 
  summarise(n())

#In-Zone whiff%
zone_whiff_pct = df_cease %>% 
  filter((description == 'swinging_strike') & ((zone == 1) | (zone == 2) | 
        (zone == 3) | (zone == 4) | (zone == 5) | (zone == 6 ) | (zone == 7) | 
        (zone == 8) | (zone == 9))) %>% 
  summarise(n()/total_zone_swings)

##Out-of-Zone Swings

#out-of-zone swing total
total_out_zone_swings = df_cease %>% 
  filter((!is.na(swing_length)) & ((zone == 10) | (zone == 12) | 
            (zone == 13) | (zone == 14) | (zone == 11))) %>% 
  summarise(n())

#Out-of-zone whiff%
out_zone_whiff_pct = df_cease %>% 
  filter((description == 'swinging_strike') & ((zone == 10) | (zone == 12) | 
  (zone == 13) | (zone == 14) | (zone == 11))) %>% 
  summarise(n()/total_out_zone_swings)

##First-Strike %

#Calculate the number of batters faced
total_batters_faced = df_cease %>% 
  filter(pitcher == 656302 & events != "") %>% 
  summarise(n())
#Calculate the First-Strike Percentage
first_strike_pct = df_cease %>% 
  filter((pitcher == 656302) & (strikes == 1) & (balls == 0)) %>% 
  summarise(n()/total_batters_faced)
##Called Strike Percentage
cs_pct = df_cease %>% 
  filter(description == 'called_strike') %>% 
  summarise(n()/114)
#####Statcast data measurements

#Effective Speed Per Pitch Type

velo_distribution = df_cease %>% 
  group_by(pitch_type, effective_speed) %>% 
  summarise(effective_velo_count = n()) 

ggplot(velo_distribution, aes(x = effective_speed)) +
  geom_histogram(color = "black", fill = "steelblue") +
  facet_wrap(~pitch_type) +
  labs(x = "Effective Speed",y = "Count") +
  ggtitle("Histogram of Pitch Velo by pitch type") +
  theme(panel.border = element_blank())+
  theme_bw()

##xwOBA and xBA by pitch types

#Estimated ba
xBA_by_pt = df_cease %>% 
  group_by(pitch_type) %>% 
  filter(description == "hit_into_play") %>% 
  summarise(average_xBA = mean(estimated_ba_using_speedangle))

ggplot(xBA_by_pt, aes(x = pitch_type, y = average_xBA, fill = pitch_type)) +
  geom_col() +
  labs(x = 'Pitch Type', y = 'xBA', title = 'xBA by Pitch Type') +
  theme_classic()

#Estimated wOBA
xwOBA_by_pt = df_cease %>% 
  group_by(pitch_type) %>% 
  filter(description == "hit_into_play") %>% 
  summarise(average_xwOBA = mean(estimated_woba_using_speedangle))

ggplot(xwOBA_by_pt, aes(x = pitch_type, y = average_xwOBA, fill = pitch_type)) +
  geom_col() +
  labs(x = 'Pitch Type', y = 'xwOBA', title = 'xwOBA by Pitch Type') +
  theme_classic()

#Barrled Balls By Definition
Barrel_LA = c()

#Launch Angle and exit velo on balls in play
 
LA_BIP = df_cease %>% 
  filter(description == 'hit_into_play') %>% 
  group_by(launch_angle, launch_speed) %>% 
  summarise(BBE_count = n())

ggplot(LA_BIP, aes(x =launch_angle, y = launch_speed))+
  geom_point() +
  annotate('rect', xmin=-Inf, xmax=+Inf, ymin=95, ymax=+Inf, alpha=.2, fill='red') +
  annotate("text", x = 0, y = 100, label ="Hard Hit Balls", color = "red") +
  annotate('rect', xmin = 8, xmax = 32, ymin = -Inf, ymax = +Inf, alpha = .2, fill = 'blue' ) +
  annotate("text", x = 22, y = 75, label = "Sweet Spot", angle = 270, color = "blue") +
  labs(title = "Batted Ball Launch Angles/Exit Velos") +
  theme_bw()
