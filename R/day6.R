

data6a <- read.csv("data/day6data_a.csv", header = FALSE,sep = ",",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day6example.csv", header = FALSE, sep = ",",
                        colClasses = "character"
                   )

exampledata_table <- as.numeric(strsplit(exampledata$V1[1],split=c(','), fixed=TRUE)[[1]])


data6a_table <- as.numeric(strsplit(data6a$V1[1],split=c(','), fixed=TRUE)[[1]])



day6func_a <- function(data6a, #a vector
                       num_days = 18){

  result_mat <- matrix(-1, nrow = num_days+1, ncol = length(data6a))
  result_mat[1,] <- data6a


  for(i in 2:(num_days+1)){
    result_mat[i,] <- result_mat[i-1,]-1

    tempncols <- ncol(result_mat)
    for(j in 1:tempncols){
      if(result_mat[i,j]==-1){
        result_mat[i,j] <- 6
        result_mat <- cbind(result_mat, c(rep(-1, i-1),8, rep(-1,num_days-i+1) ))
      }

    }#end loop pver j


  }#end loop over i


# print(result_mat)
  return(ncol(result_mat))

}




day6func_a_fast <- function(data6a, #a vector
                       num_days = 18){

  # result_mat <- matrix(-1, nrow = num_days+1, ncol = length(data6a))
  result_vec <- data6a


  for(i in 2:(num_days+1)){
    result_vec <- result_vec-1

    tempncols <- length(result_vec)
    for(j in 1:tempncols){
      if(result_vec[j]==-1){
        result_vec[j] <- 6
        result_vec <- c(result_vec, 8)
      }

    }#end loop pver j


  }#end loop over i


  # print(result_mat)
  return(length(result_vec))

}


#
# day6func_a_faster <- function(data6a, #a vector
#                             num_days = 18){
#
#   # result_mat <- matrix(-1, nrow = num_days+1, ncol = length(data6a))
#   result_vec <- data6a
#
#   totalnum <- 0
#   for(i in 1:length(data6a)){
#
#     num_extra <- floor((num_days - (data6a[i]+1))/7)
#     running_counts <- rep(0,num_extra)
#
#     for(j in 1:num_extra){
#       day_created <- (j-1)*7 + (num_days - (data6a[i]+1))
#       new_num_extra <- floor((num_days - (day_created+2))/7)
#
#       running_counts[j] <- new_num_extra
#
#       while(day_created < num_days -1){
#         day_created <- 7 + day_created
#         new_num_extra_jk <- floor((num_days - (day_created+2))/7)
#
#         running_counts[j] <- running_counts[j]+new_num_extra_jk
#
#
#
#
#       }
#
#
#     }
#
#     totalnum <- totalnum + 1+ num_extra + sum(running_counts)
#
#   }#end loop over i
#
#
#   # print(result_mat)
#   return(length(result_vec))
#
# }


day6func_recursive <- function(init_num = 3, #a vector
                              num_days = 18){

  if((num_days - (init_num+1))<=-1){
    return(0)
  }else{
    # num_extra <- floor((num_days - (init_num+1))/7)+1

    num_extra <- 0
    if(init_num == 5 ){
      num_extra <- floor(num_days/7)
    }
    if(init_num < 5 ){
      num_extra <- floor((num_days- init_num -1 )/7)+1

    }
    if(init_num ==7 ){
      num_extra <- floor((num_days -1 )/7)

    }


    running_counts <- rep(0,num_extra)

    for(j in 1:num_extra){
            day_created <- (j-1)*7 + ((init_num+1))
            #new_num_extra <- floor((num_days - (day_created+2))/7)

            running_counts[j] <- day6func_recursive(7, num_days - day_created -1)


    }

    return(num_extra+ sum(running_counts))

  }

}

day6func_apply_rec<- function(data6a, num_days1 = 18){

  total <- 0

  for(i in 1:length(data6a)){
    total <- total + day6func_recursive(init_num = data6a[i], num_days = num_days1)
  }

  return(total + length(data6a))
}

# adventofcode:::day6func_recursive_cpp(7, 15)

# adventofcode:::day6func_recursive_cpp(init_num = 3, num_days = 18)
# adventofcode:::day6func_recursive_cpp(init_num = 4, num_days = 18)
# adventofcode:::day6func_recursive_cpp(init_num = 3, num_days = 18)
# adventofcode:::day6func_recursive_cpp(init_num = 1, num_days = 18)
# adventofcode:::day6func_recursive_cpp(init_num = 2, num_days = 18)
#
# library(parallel)
# ncore <- detectCores()
#
# adventofcode:::day6func_apply_rec_cpp(data6a = exampledata_table,
#                    num_days = 18, ncore = ncore-1)
#
#
# adventofcode:::day6func_apply_rec_cpp(data6a = exampledata_table,
#                    num_days = 80, ncore = ncore-1)
#
# #answer to part a
#
# adventofcode:::day6func_apply_rec_cpp(data6a = data6a_table,
#                    num_days = 80, ncore = ncore-1)
#
#
# adventofcode:::day6func_apply_rec_cpp(data6a = exampledata_table,
#                                       num_days = 256, ncore = ncore-1)
#
# #answer to part b
#

# start.time <- Sys.time()
# res_b <- adventofcode:::day6func_apply_rec_cpp(data6a = data6a_table,
#                                       num_days = 256, ncore = ncore-1)
#
# end.time <- Sys.time()
#
# end.time - start.time
#
# format(res_b,scientific=FALSE)



#5509621059
#26984457539
# 26984457539 - 5509621059

#
# day6func_apply_rec(data6a = exampledata_table,
#            num_days = 18)
#
# day6func_recursive(init_num = 7, num_days = 15)
#
# day6func_recursive(init_num = 3, num_days = 18)
# day6func_recursive(init_num = 4, num_days = 18)
# day6func_recursive(init_num = 3, num_days = 18)
# day6func_recursive(init_num = 1, num_days = 18)
# day6func_recursive(init_num = 2, num_days = 18)
#
#
#
# day6func_apply_rec(data6a = exampledata_table,
#                    num_days = 18)
#
#
# day6func_apply_rec(data6a = exampledata_table,
#                    num_days = 80)
#
#
#
#
#
# day6func_apply_rec(data6a = data6a_table,
#                    num_days = 80)
#
#
#
#
# ######################
# #part b
#
#
# day6func_apply_rec(data6a = data6a_table,
#                    num_days = 256)


#
#
# day6func_a(data6a = exampledata_table,
#            num_days = 18)
#
#
# day6func_a(data6a = exampledata_table,
#            num_days = 80)
#
#
#
# day6func_a_fast(data6a = exampledata_table,
#            num_days = 18)
#
#
# day6func_a_fast(data6a = exampledata_table,
#            num_days = 80)
#
# #answer
#
# # day6func_a(data6a = data6a_table,
# #            num_days = 80)
#
# day6func_a_fast(data6a = data6a_table,
#            num_days = 80)
#








