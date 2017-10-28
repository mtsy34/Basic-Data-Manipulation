library(dplyr)
library(faraway)
data(worldcup)

head(worldcup)

# Moving first column of players into its own column

worldcup <- worldcup %>%
  mutate(player_name = rownames(worldcup))
worldcup %>% slice(1:3)

# Creating a new column with the average number of shots for a player's position 

worldcup <- worldcup %>%
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup()
worldcup

# Renaming a column

worldcup %>%
  rename(Name = player_name) %>%
  slice(1:3)

library(tidyr)
library(ggplot2)
data("VADeaths")
head(VADeaths)

# Moving age from row names to column
VADeaths <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths))
VADeaths

# Gathering all the columns into a single one called key (except age) to tidy data
VADeaths %>%
  gather(key = key, value = death_rate, -age)

# Gathering is useful e.g. when plotting the relationship between the time a player played in the world
# cup and his number of saves, tackles and shots, with a separate graph for each position. 
worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>%
  gather(key = Type, value = Number, -Position, -Time) %>%
  ggplot(aes(x = Time, y = Number)) +
  geom_point() +
  facet_grid(Type ~ Position)

# To print a summary table of the average number and range of passes by position for top 4 teams in World Cup
library(knitr)

wc_table <- worldcup %>%
  filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(ave_passes = mean(Passes),
            min_passes = min(Passes),
            max_passes = max(Passes),
            pass_summary = paste0(round(ave_passes), " (", min_passes, " ,", max_passes, ")")) %>%
  select(Team, Position, pass_summary)
wc_table

# Use spread to create a prettier format of the table - Specify the new column names, then the column
# to use for the cell values 

wc_table %>%
  spread(Position, pass_summary) %>%
  kable()

# Merging datasets 
library(readr)
team_standings <- read_csv("./team_standings.csv")
team_standings %>% slice(1:3)

# Left join includes all the rows from worldcup, whether or not the player had a team listed in team_standings
left_join(worldcup, team_standings, by = "Team")

# Right join include all the rows from team_standings, whether or not there were any players from that team in world_cup
right_join(worldcup, team_standings, by = "Team")

# Create a table of the top 5 players by shots on goal, as well as the final standing for each of these player’s teams, 
# using the worldcup and team_standings data
data(worldcup)
worldcup %>% 
  mutate(Name = rownames(worldcup),
         Team = as.character(Team)) %>% # Ensuring the field to merge is of same class for both dataframes 
  select(Name, Position, Shots, Team) %>%
  arrange(desc(Shots)) %>%  # Arrange in descending order
  slice(1:5) %>%
  left_join(team_standings, by = "Team") %>% # Merge 
  rename("Team Standing" = Standing) %>%
  kable()

