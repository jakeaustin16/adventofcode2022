library(readr)

# a = opponent rock
# b = opponent paper
# c = opponent scissors
# x = me rock
# y = me paper
# z = me scissors

# points per round
input = read_lines("02/input.txt") %>%
  data.frame() %>%
  separate(1, into = c("opponent", "me"), " ") %>%
  mutate(
    win = case_when(
      opponent == "A" & me == "Y" ~ "W"
      ,opponent == "B" & me == "Z" ~ "W"
      ,opponent == "C" & me == "X" ~ "W"
      
      ,opponent == "A" & me == "Z" ~ "L"
      ,opponent == "B" & me == "X" ~ "L"
      ,opponent == "C" & me == "Y" ~ "L"
      
      ,opponent == "A" & me == "X" ~ "D"
      ,opponent == "B" & me == "Y" ~ "D"
      ,opponent == "C" & me == "Z" ~ "D"
      )
    ) %>%
  mutate(
    win_points = case_when(
      win == "W" ~ 6
      ,win == "D" ~ 3
      ,win == "L" ~ 0
    )
  ) %>%
  mutate(
    strategy_points = case_when(
      me == "X" ~ 1
      ,me == "Y" ~ 2
      ,me == "Z" ~ 3
    )
  ) %>%
  mutate(
    points = win_points + strategy_points
    )

# total points - column two is rock/paper/scissors
input %>% .$points %>% sum()

# determine points based on need to win
# x = lose
# y = draw
# z = win
input = read_lines("02/input.txt") %>%
  data.frame() %>%
  separate(1, into = c("opponent", "me"), " ") %>%
  mutate(
    points = case_when(
      opponent == "A" & me == "X" ~ 3
      ,opponent == "B" & me == "X" ~ 1
      ,opponent == "C" & me == "X" ~ 2
      ,opponent == "A" & me == "Y" ~ 4
      ,opponent == "B" & me == "Y" ~ 5
      ,opponent == "C" & me == "Y" ~ 6
      ,opponent == "A" & me == "Z" ~ 8
      ,opponent == "B" & me == "Z" ~ 9
      ,opponent == "C" & me == "Z" ~ 7
    )
  )

# total points - column two is lose/draw/win
input %>% .$points %>% sum()