library(tidyverse)
library(readr)

# read input
forest = read_fwf("08/input.txt") %>% data.frame()

# create data frame to track height of trees
forest_split = matrix(NA, nrow = 99, ncol = 99) %>% data.frame()
for(r in 1:99){
  forest_split[r,] = forest[r,1] %>% str_split(pattern = "") %>% .[[1]]
}

# create data frame to track visibility of trees
forest_visible = matrix(NA, nrow = 99, ncol = 99) %>% data.frame()

# determine visibility of trees
for(r in 1:99){
  for(c in 1:99){
    if(r == 1 | c == 1 | r == 99 | c == 99){
      forest_visible[r,c] = T
    }else if(
      all(forest_split[1:r-1,c] < forest_split[r,c]) | all(forest_split[r,1:c-1] < forest_split[r,c]) | all(forest_split[(r+1):99,c] < forest_split[r,c]) | all(forest_split[r,(c+1):99] < forest_split[r,c])
    ){
      forest_visible[r,c] = T
    }else{
      forest_visible[r,c] = F
    }
  }
}

# count visible trees
sapply(forest_visible, sum) %>% sum()

# create data frame to track visibility_distance score
forest_score = matrix(NA, nrow = 99, ncol = 99) %>% data.frame()

for(r in 1:99){
  for(c in 1:99){
    
    # if tree is on border of forest score is zero
    if(any(r == 1, c == 1, r == 99, c == 99)){
      score = 0
      }else{
        
        # if tree has no taller trees north of it, north distance is row number minus one
        # if tree does have a tree at least as tall to the north, north distance is row number minus the row number of the closest tree to the north
        if(which(forest_split[1:(r-1),c] >= forest_split[r,c]) %>% length() == 0){
          n = r-1
          }else{
            n = r-(which(forest_split[1:(r-1),c] >= forest_split[r,c]) %>% tail(1))
            }
        
        # if tree has no taller trees west of it, west distance is column number minus one
        # if tree does have a tree at least as tall to the west, west distance is column number minus the column number of the closest tree to the west
        if(which(forest_split[r,1:(c-1)] >= forest_split[r,c]) %>% length() == 0){
          w = c-1
          }else{
            w = c-(which(forest_split[r,1:(c-1)] >= forest_split[r,c]) %>% tail(1))
            }
        
        # if tree has no taller trees south of it, south distance is ninetynine minus the row number
        # if tree does have a tree at least as tall to the south, south distance is row number minus the row number of the closest tree to the south
        if(which(forest_split[(r+1):99,c] >= forest_split[r,c]) %>% length() == 0){
          s = 99-r
          }else{
            s = which(forest_split[(r+1):99,c] >= forest_split[r,c]) %>% head(1)
            }
        
        # if tree has no taller trees east of it, east distance is ninetynine minus the row number
        # if tree does have a tree at least as tall to the east, east distance is row number minus the row number of the closest tree to the east
        if(which(forest_split[r,(c+1):99] >= forest_split[r,c]) %>% length() == 0){
          e = 99-c
          }else{
            e = which(forest_split[r,(c+1):99] >= forest_split[r,c]) %>% head(1)
            }
      
        score = n*w*s*e
        
      }
    
    forest_score[r,c] = score
    
  }
}

# identify max visibility score
sapply(forest_score, max) %>% max()
