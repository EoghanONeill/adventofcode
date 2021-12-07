
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data7a <- read.csv("data/day7data_a.csv", header = FALSE,sep = ",",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day7example.csv", header = FALSE, sep = ",",
                        colClasses = "character"
                   )

exampledata_table <- as.numeric(strsplit(exampledata$V1[1],split=c(','), fixed=TRUE)[[1]])


data7a_table <- as.numeric(strsplit(data7a$V1[1],split=c(','), fixed=TRUE)[[1]])

# median(exampledata_table)

day7func_a <- function(data7a){

  res <- sum(abs(data7a - median(data7a)))

# print(result_mat)
  return(res)

}

# day7func_a(exampledata_table)

# day7func_a(data7a_table)



day7func_b <- function(data7a){

  #this seems to give the nearest integer that minimizes the sum of distances
  #I obtained the correct answer, but I have not verifies this
  temp_c <- round((mean(data7a)+ (median(data7a))/(length(data7a)))/(1+(1/(length(data7a)))))

  res <- 0
  for(i in 1:length(data7a)){
    res <- res + (abs(data7a[i]- temp_c))*(abs(data7a[i]- temp_c)+1)/2
  }

  # print(result_mat)
  return(res)

}

# day7func_b(exampledata_table)

# day7func_b(data7a_table)


