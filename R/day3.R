

data3a <- read.csv("data/day3data_a.csv", header = FALSE,
                   colClasses = "character")


day3func_a <- function(data3a){
  num_digits <- nchar(as.character(data3a[1,]))
  epsilon_rate <- 0
  gamma_rate <- 0
  for(dig_index in 1:num_digits){
    temp_1count <- 0
    temp_0count <- 0

    for(i in 1:(nrow(data3a))){
      if(substr(data3a[i,], start = dig_index, stop = dig_index) == "1"){
        temp_1count <- temp_1count + 1
      }else{
        temp_0count <- temp_0count + 1
      }

    }
    if(temp_1count == temp_0count){
      stop("Counts of 1s and 0s are equal")
    }
    if(temp_1count > temp_0count){
      #most common bit 1, so relevant element of gamma binary representation is 1
      gamma_rate <- gamma_rate + (2^(num_digits-dig_index))
    }
    if(temp_1count < temp_0count){
      #least common bit 1, so relevant element of gamma binary representation is 1

      epsilon_rate <- epsilon_rate + (2^(num_digits-dig_index))

    }


  }
  return(gamma_rate*epsilon_rate)
}


exampledata <- matrix(data = c("00100",
                               "11110",
                               "10110",
                               "10111",
                               "10101",
                               "01111",
                               "00111",
                               "11100",
                               "10000",
                               "11001",
                               "00010",
                               "01010"),
                      nrow = 12,
                      ncol = 1)

# day3func_a(exampledata)

#answer
# day3func_a(data3a)

######################
#part b



day3func_b <- function(data3a){
  num_digits <- nchar(as.character(data3a[1,]))
  oxy_rate <- "0"
  co2_rate <- "0"

  oxy_val <- 0
  co2_val <- 0

  keep_oxy_inds <- rep(1, nrow(data3a))
  keep_co2_inds <- rep(1, nrow(data3a))

  end_oxy_search <- 0
  end_co2_search <- 0

  for(dig_index in 1:num_digits){
    temp_1count_oxy <- 0
    temp_0count_oxy <- 0

    temp_1count_co2 <- 0
    temp_0count_co2 <- 0

    # print("dig_index = "  )
    # print(dig_index)

    for(i in 1:(nrow(data3a))){
      if(keep_oxy_inds[i] ==1){
        if(substr(data3a[i,], start = dig_index, stop = dig_index) == "1"){
          temp_1count_oxy <- temp_1count_oxy + 1
        }else{
          temp_0count_oxy <- temp_0count_oxy + 1
        }
      }
      if(keep_co2_inds[i] ==1){
        if(substr(data3a[i,], start = dig_index, stop = dig_index) == "1"){
          temp_1count_co2 <- temp_1count_co2 + 1
        }else{
          temp_0count_co2 <- temp_0count_co2 + 1
        }
      }

    }
    # if(temp_1count == temp_0count){
    #   stop("Counts of 1s and 0s are equal")
    # }
    if(temp_1count_oxy >= temp_0count_oxy){
      #most common bit 1, least common 0
      oxy_val <- "1"
      # co2_val <- "0"
    }
    if(temp_1count_oxy < temp_0count_oxy){
      #most common bit 0, least common 1
      oxy_val <- "0"
      # co2_val <- "1"
    }
    if(temp_1count_co2 >= temp_0count_co2){
      #most common bit 1, least common 0
      # oxy_val <- "1"
      co2_val <- "0"
    }
    if(temp_1count_co2 < temp_0count_co2){
      #most common bit 0, least common 1
      # oxy_val <- "0"
      co2_val <- "1"
    }

    for(i in 1:(nrow(data3a))){
      if(substr(data3a[i,], start = dig_index, stop = dig_index) == oxy_val){
        if(end_oxy_search==0){
          keep_oxy_inds[i] <- keep_oxy_inds[i]*1
        }
      }else{
        if(end_oxy_search==0){
          keep_oxy_inds[i] <- keep_oxy_inds[i]*0
        }
      }

      if(substr(data3a[i,], start = dig_index, stop = dig_index) == co2_val){
        #note oxy val and co2 val never equal
        if(end_co2_search==0){
          keep_co2_inds[i] <- keep_co2_inds[i]*1
        }
      }else{
        if(end_co2_search==0){
          keep_co2_inds[i] <- keep_co2_inds[i]*0
        }
      }

    }

    # print("keep_co2_inds = ")
    # print(keep_co2_inds)

    if(end_oxy_search==0){
      if(sum(keep_oxy_inds)==1){
        oxy_rate <- data3a[which(keep_oxy_inds ==  1) ,1]
        end_oxy_search <- 1
      }
    }

    if(end_co2_search==0){
      if(sum(keep_co2_inds)==1){
        co2_rate <- data3a[which(keep_co2_inds ==  1) ,1]
        end_co2_search <- 1
      }
    }

    if((sum(keep_oxy_inds)==1) & (sum(keep_co2_inds)==1) ) {
      break
    }

    if( (dig_index ==num_digits)&((sum(keep_oxy_inds)*sum(keep_co2_inds)) !=1) ) {
      stop("did not obtain single numbers for both oxygen and co2")
    }


  } #end loop over digi index


# print("end of function")
#
# print("oxy_rate = ")
# print(oxy_rate)
#
# print("co2_rate = ")
# print(co2_rate)

  return(strtoi(oxy_rate, base = 2)*strtoi(co2_rate, base = 2))
}


# day3func_b(exampledata)

#answer
# day3func_b(data3a)






