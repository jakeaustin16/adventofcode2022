rm(list = ls())
library(stringr)
FilePath <- setRefClass("FilePath", 
                        fields = c("path", "children", "files"),
                        methods = list(
                          getChildren = function(){
                            return(children)
                          },
                          checkForChild = function(path){
                            if (length(children) > 0){
                              for (i in children){
                                if (i$path == path){
                                  return(TRUE)
                                }
                              }
                            }
                            return(FALSE)
                          },
                          addFilePath = function(path){
                            newFP <- createFP(path = path, children = list(), files = list())
                            children <<- append(children, newFP)
                          },
                          getFilePath = function(path){
                            for (item in children){
                              if (item$path == path){
                                return(item)
                              }
                            }
                          },
                          addFile = function(size, name){
                            newF <- createF(size = size, name = name)
                            files <<- append(files, newF)
                          },
                          directorySize = function(){
                            totalSize = 0
                            for (file in files){
                              totalSize = totalSize + file$size
                            }
                            for (child in children){
                              totalSize = totalSize + child$directorySize()
                            }
                            return(totalSize)
                          }
                        ))
File <- setRefClass("File", fields = c("size", "name"))

createFP <- function(path, children, files){
  newFP <- FilePath$new(path = path, children = children, files = files)
  return(newFP)
}

createF <- function(size, name){
  newF <- File$new(size = size, name = name)
  return(newF)
}

findParent <- function(searchObj, queue = list(homeFP)){
  
  while (length(queue) > 0){
    
    current_search <- queue[[1]]
    queue <- queue[-1]
    
    if (length(current_search$children) > 0){
      for (child in current_search$children){
        if (identical(child, searchObj)){
          return(current_search)
        }
      }
      for (i in current_search$children){
        queue <- append(queue, i)
      }
    }
  }
}

traverseEntireTree <- function(queue = list(homeFP)){
  
  all_nodes <- list()
  
  while (length(queue) > 0){
    
    current_search <- queue[[1]]
    queue <- queue[-1]
    
    last <- length(all_nodes)
    all_nodes[[last+1]] <- list()
    all_nodes[[last+1]][[1]] <- current_search$path
    all_nodes[[last+1]][[2]] <- current_search$directorySize()
    
    if (length(current_search$children) > 0){
      for (child in current_search$children){
        queue <- append(queue, child)
      }
    }
  }
  
  return(all_nodes)
}

lines <- read.table("07/input.txt", header = FALSE, sep = "\n")$V1

homeFP <- createFP(path = "/", children = list(), files = list())
current_dir <- homeFP

for (line in lines){
  if (str_detect(line, fixed("$ cd"))){
    folder <- str_split(line, " ")[[1]][3]
    if (folder == ".."){
      current_dir <- findParent(current_dir)
    } else if (folder == "/"){
      current_dir <- homeFP
    } else {
      if (current_dir$checkForChild(folder)){
        current_dir <- current_dir$getFilePath(folder)
      } else {
        current_dir$addFilePath(folder)
        current_dir <- current_dir$getFilePath(folder)
      }
    }
  } else if (str_detect(line, fixed("$ ls"))){
    # Do nothing.
  } else if (str_detect(line, fixed("dir"))){
    folder = str_split(line, " ")[[1]][2]
    if (!current_dir$checkForChild(folder)){
      current_dir$addFilePath(folder)
    }
  } else {
    file_size <- as.integer(str_split(line, " ")[[1]][1])
    file_name <- str_split(line, " ")[[1]][2]
    current_dir$addFile(file_size, file_name)
  }
}

all_nodes <- traverseEntireTree()

sum(unlist(sapply(all_nodes[unlist(lapply(all_nodes, function(x){x[[2]] <= 100000}))], function(x) x[2]))) # Answer to Part 1.
desired_free_disk_space <- homeFP$directorySize() - (70000000 - 30000000)
sort(unlist(sapply(all_nodes[unlist(lapply(all_nodes, function(x){x[[2]] >= desired_free_disk_space}))], function(x) x[2])))[1] # Answer to Part 2.
