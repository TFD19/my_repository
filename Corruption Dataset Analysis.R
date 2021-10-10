# Set workind directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets")

# Examining the data files within our directory
dir()

# Loading the packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(broom)
library(readxl)

# Inspect the sheets in our datafile
excel_sheets("CorruptionStatistics_by_State_full.xlsx")

# Read datafile into workspace
corruptio_dataset <- read_excel("CorruptionStatistics_by_State_full.xlsx", 
                                sheet = "Nigeria_Statistics_by_State")

# Inspect the structure of the data
glimpse(corruptio_dataset)
str(corruptio_dataset)

# Explore research questions with visualizing aids
# Which states occupy the top 5 positions in the prevalence of bribes
corruptio_dataset %>% top_n(5, wt = `Prevalence of bribery`) %>% 
  ggplot(aes(x = reorder(STATE, -`Prevalence of bribery`), y = `Prevalence of bribery`)) + 
  geom_col() + labs(x = "State", title = "The top 5 states by bribery prevalency")

# How are the top 5 most bribery prevalent states distributed across the regions
corruptio_dataset %>% top_n(5, wt = `Prevalence of bribery`) %>% 
  ggplot(aes(x = reorder(STATE, -`Prevalence of bribery`), y = `Prevalence of bribery`, fill = Region)) + 
  geom_col() + labs(x = "State", title = "The top 5 states by bribery prevalency distributed among regions")

# Extending the last two graphs to view the top 10 states
corruptio_dataset %>% top_n(10, wt = `Prevalence of bribery`) %>% 
  ggplot(aes(x = reorder(STATE, -`Prevalence of bribery`), y = `Prevalence of bribery`)) + 
  geom_col() + labs(x = "State", title = "The top 10 states by bribery prevalency")

corruptio_dataset %>% top_n(10, wt = `Prevalence of bribery`) %>% 
  ggplot(aes(x = reorder(STATE, -`Prevalence of bribery`), y = `Prevalence of bribery`, fill = Region)) + 
  geom_col() + labs(x = "State", title = "The top 10 states by bribery prevalency distributed among regions")

# Which states has the highest amount paid for bribe and region distribution
corruptio_dataset %>% top_n(5, wt = `Average bribe size NGN_no outliers`) %>% 
  ggplot(aes(x = reorder(STATE, -`Average bribe size NGN_no outliers`), y = `Average bribe size NGN_no outliers`, fill = Region)) + 
  geom_col() + labs(x = "State", title = "States with the highest amounts paid in bribe")

# Inspecting the prevalency of bribery per region
corruptio_dataset %>% group_by(Region) %>% ggplot(aes(x = "", y = -`Prevalence of bribery`, fill = Region)) + geom_bar(width = 1, stat = "identity") + labs(title = "Prevalency of bribery in each region") + coord_polar(theta = "y", start = 0) + geom_text(aes(color = "black") + theme_void()

                                                                                                                                                                                                                                                                                                          
# Inspecting the effect of income size on bribery prevalence
corruptio_dataset %>% ggplot(aes(x = `Average income`, y = `Prevalence of bribery`, color = STATE)) + 
  geom_point(alpha = 0.7) + scale_size(range = c(.1, 24)) + geom_text(aes(label = STATE)) + theme(legend.position = "none")

# Does the size of the average income affect the size of the size of the bribe size being paid in each state
corruptio_dataset %>% ggplot(aes(x = `Average income`, y = `Average bribe size NGN`, color = STATE)) + 
  geom_point(alpha = 0.3) + geom_text(aes(label = STATE)) + theme(legend.position = "none")

# Cumulative bribe size by each region
corruptio_dataset %>% group_by(Region, `Average bribe size NGN`) %>% select(Region, `Average bribe size NGN`) %>% 
  ggplot(aes(x = reorder(Region, `Average bribe size NGN`), y = `Average bribe size NGN`)) + geom_col(fill = "blue") + 
  labs(x = "Region", title = "Average bribe size within each region")

# Ranking of states within each region by bribery proportion
corruptio_dataset %>% group_by(Region, STATE) %>% select(Region, STATE, `Prevalence of bribery`) %>% 
  ggplot(aes(x = STATE, y = `Prevalence of bribery`)) + geom_col(position = "dodge") + 
  facet_wrap(vars(Region), scales = "free")

# Effect of gender proportion on bribery prevalence
corruptio_dataset %>% ggplot(aes(x = `Share male`, y = `Prevalence of bribery`)) + 
  geom_point(alpha = 0.4, size = 2.5, color = "blue") + geom_line(color = "blue") + 
  facet_wrap(vars(Region), scales = "free") + labs(x = "Male Proportion", title = "Effect of gender population on bribery prevalence")

# What is the effect of population size on the prevalence of bribery
corruptio_dataset %>% group_by(Region) %>% select(Region, Population, `Prevalence of bribery`) %>% 
  ggplot(aes(x = Population, y = `Prevalence of bribery`, fill = Region)) + 
  geom_col() + xlim(4000000, 6500000) + labs(x = "Regional Population", title = "Effect of population on rate of bribery")

# Prevalency of police bribery within each region
corruptio_dataset %>% group_by(Region) %>% ggplot(aes(x = reorder(Region, -`Prevalence of bribery_police`), y = `Prevalence of bribery_police`), fill = Region) + 
  geom_col() + labs(x = "Region", title = "Prevalence of police bribery within each region")
