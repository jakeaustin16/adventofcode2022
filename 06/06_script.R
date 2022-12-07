# libraries
library(readr)
library(tidyverse)
library(stringr)

# read input file
input = read_lines("06/input.txt")

# separate characters
characters = data.frame(
  character = input %>% str_split("") %>% .[[1]]) %>%
  mutate(
    lag1 = lag(character)
    ,lag2 = lag(character,2)
    ,lag3 = lag(character,3)
    ,lag4 = lag(character,4)
    ,lag5 = lag(character,5)
    ,lag6 = lag(character,6)
    ,lag7 = lag(character,7)
    ,lag8 = lag(character,8)
    ,lag9 = lag(character,9)
    ,lag10 = lag(character,10)
    ,lag11 = lag(character,11)
    ,lag12 = lag(character,12)
    ,lag13 = lag(character,13)
  ) %>%
  mutate(
    four_lags = paste(character, lag1, lag2, lag3)
    ,fourteen_lags = paste(character, lag1, lag2, lag3, lag4, lag5, lag6, lag7, lag8, lag9, lag10, lag11, lag12, lag13)
  )

# identify character markers
for(n in 1:nrow(characters)){
  characters[n,"unqiue_letters_in_previous_4"] = sum(!!str_count(characters[n,"four_lags"], letters))
  characters[n,"unqiue_letters_in_previous_14"] = sum(!!str_count(characters[n,"fourteen_lags"], letters))
}

marker_4_ends = which(characters$unqiue_letters_in_previous_4 > 3) %>% head(1)
marker_4 = characters[(marker_4_ends-3):marker_4_ends, "character"]

marker_14_ends = which(characters$unqiue_letters_in_previous_14 > 13) %>% head(1)
marker_14 = characters[(marker_14_ends-13):marker_14_ends, "character"]
