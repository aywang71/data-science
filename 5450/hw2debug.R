# clean up and setup
rm(list=ls()) # clean up any old stuff in R

library(tidyverse)
library(dplyr)

setwd('C:/Users/hyper/OneDrive/Documents/GitHub/data-science/5450')

credits_df = read.csv('credits.csv')
titles_df = read.csv('titles.csv')

new_titles_final_df = read.csv('new_titles_final_df.csv')


#2.1.2  
shows_df = new_titles_final_df %>% filter(is_movie == 0) %>% filter(imdb_votes >= 2000)
shows_df <- merge(shows_df, credits_df, by = 'id')

top_actors_df <- shows_df %>% group_by(name) %>% transmute(imdb_votes = sum(imdb_votes))
