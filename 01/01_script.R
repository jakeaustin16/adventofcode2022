library(readr)

# read input data
input = read_lines("01/input.txt")

# identify food each elf is carrying
elf_food = data.frame(elf = c(1), food = c(0))
elf = 1
for(line in 1:length(input)){
  # print(line)
  # print(elf)
  if(str_length(input[line]) > 0){
    elf_food[elf, "food"] = elf_food[elf, "food"] + as.numeric(input[line])
  }else{
    elf = elf + 1
    elf_food[elf, "elf"] = elf
    elf_food[elf, "food"] = 0
    }
  
}

# most food carried by an elf
max(elf_food$food)

# total food carried by top three elves
elf_food %>% arrange(-food) %>% head(3) %>% .$food %>% sum()
