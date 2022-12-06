# libraries
library(readr)
library(tidyverse)

# read starting position
starting_position = read_fwf("05/input.txt", n_max = 8)
  # reformat stacks table to list of stacks
  stacks_list_initial = list()
  for(c in 1:ncol(starting_position)){
    stacks_list_initial[[c]] = starting_position[,c] %>% drop_na() %>% c() %>% .[[1]] %>% rev()
  }
  stacks_list = stacks_list_initial
  
# read move list
move_list = read_delim("05/input.txt", skip = 9, col_names = F) %>%
  select(crates = X2, from = X4, to = X6)

# function to move crates from one stack to another
move_crates = function(stacks, crates, from, to){
  moved_crates = stacks[[from]] %>% tail(crates) %>% rev()
  stacks[[from]] = stacks[[from]] %>% .[0:(length(.)-crates)]
  stacks[[to]] = c(stacks[[to]], moved_crates)
  return(stacks)
}

total_crates = sum(sapply(stacks_list, length))
for(r in 1:nrow(move_list)){
  print(paste0(r, ": move ", move_list[[r,1]], " from ", move_list[[r,2]], " to ", move_list[[r,3]]))
  stacks_list = move_crates(
    stacks = stacks_list
    ,crates = move_list[[r,1]]
    ,from = move_list[[r,2]]
    ,to = move_list[[r,3]]
    )
  print(sapply(stacks_list, length))
  # print(paste0("total: ", paste0(sum(sapply(stacks_list, length)))))
  if(sum(sapply(stacks_list, length)) != total_crates){stop()}
}
