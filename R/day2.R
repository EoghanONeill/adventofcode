

data2a <- read.csv("data/day2data_a.csv", header = FALSE)


day2func_a <- function(data2a){
  hor_count <- 0
  dep_count <- 0
  for(i in 1:(nrow(data2a))){
    if(substr(data2a[i,], start = 1, stop = 2) == "fo"){
      tempnum <- as.numeric(substr(data2a[i,], start = 9, stop = nchar(as.character(data2a[i,]))))
      hor_count <- hor_count + tempnum
    }
    if(substr(data2a[i,], start = 1, stop = 2) == "do"){
      tempnum <- as.numeric(substr(data2a[i,], start = 6, stop = nchar(as.character(data2a[i,]))))
      dep_count <- dep_count + tempnum
    }
    if(substr(data2a[i,], start = 1, stop = 2) == "up"){
      tempnum <- as.numeric(substr(data2a[i,], start = 4, stop = nchar(as.character(data2a[i,]))))
      dep_count <- dep_count - tempnum
    }
  }
  return(hor_count*dep_count)
}


exampledata <- matrix(data = c("forward 5",
                               "down 5",
                               "forward 8",
                               "up 3",
                               "down 8",
                               "forward 2"),
                      nrow = 6,
                      ncol = 1)

day2func_a(exampledata)

#answer
day2func_a(data2a)

######################
#part b



day2func_b <- function(data2a){
  aim_count <- 0
  hor_count <- 0
  dep_count <- 0
  for(i in 1:(nrow(data2a))){
    if(substr(data2a[i,], start = 1, stop = 2) == "fo"){
      tempnum <- as.numeric(substr(data2a[i,], start = 9, stop = nchar(as.character(data2a[i,]))))
      hor_count <- hor_count + tempnum
      dep_count <- dep_count + tempnum*aim_count
    }
    if(substr(data2a[i,], start = 1, stop = 2) == "do"){
      tempnum <- as.numeric(substr(data2a[i,], start = 6, stop = nchar(as.character(data2a[i,]))))
      aim_count <- aim_count + tempnum
    }
    if(substr(data2a[i,], start = 1, stop = 2) == "up"){
      tempnum <- as.numeric(substr(data2a[i,], start = 4, stop = nchar(as.character(data2a[i,]))))
      aim_count <- aim_count - tempnum
    }
  }
  return(hor_count*dep_count)
}


day2func_b(exampledata)

#answer
day2func_b(data2a)






