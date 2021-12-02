

data1 <- read.csv("data/day1data.csv", header = FALSE,
                  fileEncoding="UTF-8-BOM")

day1func <- function(data1){
  count <- 0
  for(i in 2:(nrow(data1))){
    if(data1[i,1]>data1[i-1,1]){
      count <- count +1
    }
  }
  return(count)
}

example_data <- matrix(data = c(199,200,208,210,200,207,240,269,260,263),
                       nrow=10,
                       ncol=1)

day1func(example_data) ==7

#answer:
day1func(data1)


#part B

#just compare the first observation of the previous three day average
#to the last observation of the current three day average
#and add to the count
day1funcb <- function(data1){
  count <- 0
  for(i in 2:(nrow(data1)-2)){
    if(data1[i+2,1]>data1[i-1,1]){
      count <- count +1
    }
  }
  return(count)
}

day1funcb(example_data) == 5

#answer for part B
day1funcb(data1)
