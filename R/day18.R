
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")



data18a <- read.csv("data/day18data.txt", header = FALSE,sep = "=",
                   colClasses = "character")
#
# exampledata <- read.csv("data/day18example.txt", header = FALSE, sep = "=",
#                         colClasses = "character")

exampledata2_vec <- strsplit("[1,1]",
                             split=c(''), fixed=TRUE)[[1]]


snail_add <- function(exampledata1_vec, exampledata2_vec){

  #create a pair
  whole_pair <- c("[", exampledata1_vec,",",exampledata2_vec, "]")

  #reduce the pair

  action_last_round <- 1
  while(action_last_round ==1){
    action_last_round <- 0
    #check if can explode
    nest_count <- 0
    explode_this_round <- 0
    split_this_round <- 0
    explode_at <- -1
    split_at <- -1

    new_whole_pair_explode <- whole_pair
    new_whole_pair_split <- whole_pair


    for(i in 1:length(whole_pair)){
      if(whole_pair[i]=="["){
        nest_count <- nest_count + 1
      }
      if(whole_pair[i]=="]"){
        nest_count <- nest_count - 1
      }

      if(nest_count == 5){
        #explode the pair at i

        # print("line 45, explode at whole_pair[i] = ")
        # print(whole_pair[i])
        #obtain the left value
        left_valvec <- vector(length = 0)
        #length of loop unimportant here
        #could have used while loop
        #will break from loop
        comma_ind <- -1
        for(j in (i+1):(length(whole_pair))){
          # print("whole_pair[j] = ")
          # print(whole_pair[j])
          if(whole_pair[j] == ","){
            comma_ind <- j
            break
          }else{
            left_valvec <- c(left_valvec, whole_pair[j])
          }
        }
        left_valstr <- paste(left_valvec, collapse = "")
        #obtain the right value
        right_valvec <- vector(length = 0)

        end_ind <- -1

        for(j in (comma_ind+1):(length(whole_pair))){
          if(whole_pair[j] == "]"){
            end_ind <- j
            break
          }else{
            right_valvec <- c(right_valvec, whole_pair[j])
          }
        }
        right_valstr <- paste(right_valvec, collapse = "")


        #find next value to left and add to it
        leftnew <- -1
        leftnew_str <- NA

        k <- -1
        last_left <- -1

        # print("i = ")
        # print(i)
        for(j in (i-1):(1)){
          if(!(whole_pair[j] %in% c(",","[","]")  )){
            # left_next_ind <- j
            leftnew_vec <- vector(length = 0)
            k <- j#-1
            # while((whole_pair[k] %in% c(",","[","]")  )){
            #   k <- k-1
            # }
            last_left <- -1
            # print("line 92, last_left = ")
            # print(last_left)
            # print("k = ")
            # print(k)

            k1 <- k
            # for(k1 in k:1){
            while(!(whole_pair[k1] %in% c(",","[","]")  )){
              leftnew_vec <- c(whole_pair[k1],leftnew_vec )
              k1 <- k1-1
              last_left <- k1
              # print("line 100, last_left = ")
              # print(last_left)
              #
              # print("k1 = ")
              # print(k1)
            }

            # print("line 98, last_left = ")
            # print(last_left)
            #
            # print("line 98, leftnew_vec = ")
            # print(leftnew_vec)
            #
            # print("line 98, whole_pair = ")
            # print(whole_pair)
            #
            # print("line 98, left_valstr = ")
            # print(left_valstr)
            #
            # print("line 98, left_valvec = ")
            # print(left_valvec)


            # }
            leftnew_str <- paste(leftnew_vec, collapse = "")

            leftnew_str <- as.character(as.numeric(leftnew_str)+
                                          as.numeric(left_valstr) )

            if(is.na(leftnew_str)){
              print("whole_pair = ")
              print(whole_pair )
              print("leftnew_str = ")
              print(leftnew_str)

              stop("NA value")
            }

            break
          }
        }


        # print("line 108, last_left = ")
        # print(last_left)
        # print("whole_pair[i-1] = ")
        # print(whole_pair[i-1])

        rightnew <- -1
        rightnew_str <- NA

        kr <- -1
        last_right <- -1
        #find next value to right and add to it
        for(j in (end_ind+1):length(whole_pair)){
          if(!(whole_pair[j] %in% c(",","[","]")  )){
            # right_next_ind <- j
            rightnew_vec <- vector(length = 0)
            kr <- j
            # while((whole_pair[kr] %in% c(",","[","]")  )){
            #   kr <- kr-1
            # }
            last_right <- -1
            kr1 <- j
            # for(kr1 in kr:length(whole_pair)){
            while(!(whole_pair[kr1] %in% c(",","[","]")  )){
              rightnew_vec <- c(rightnew_vec, whole_pair[kr1])
              kr1 <- kr1+1
              last_right <- kr1
            }
            # }
            rightnew_str <- paste(rightnew_vec, collapse = "")

            rightnew_str <- as.character(as.numeric(rightnew_str)+
                                           as.numeric(right_valstr) )


            if(is.na(rightnew_str)){
              print("rightnew_str = ")
              print(rightnew_str)

              stop("NA value")
            }

            break
          }

        }

        explode_at <- i
        explode_this_round <- 1


        if((last_left == -1) & (last_right == -1)){
          new_whole_pair_explode <- c( whole_pair[1:(i-1)],
                           "0"  , #replace exploded pair with zero
                           whole_pair[(end_ind+1):length(whole_pair)])
        }
        if((last_left == -1) & (last_right != -1)){
          new_whole_pair_explode <- c( whole_pair[1:(i-1)],
                           "0"  , #replace exploded pair with zero
                           whole_pair[(end_ind+1):(kr-1)],
                           rightnew_str,
                           whole_pair[(last_right):length(whole_pair)])
        }
        if((last_left != -1) & (last_right == -1)){
          new_whole_pair_explode <- c( whole_pair[1:(last_left)],
                           leftnew_str,
                           whole_pair[(k+1):(i-1)],
                           "0"  , #replace exploded pair with zero
                           whole_pair[(end_ind+1):length(whole_pair)])
        }
        if((last_left != -1) & (last_right != -1)){
          new_whole_pair_explode <- c( whole_pair[1:(last_left)],
                           leftnew_str,
                           whole_pair[(k+1):(i-1)],
                           "0"  , #replace exploded pair with zero
                           whole_pair[(end_ind+1):(kr-1)],
                           rightnew_str,
                           whole_pair[(last_right):length(whole_pair)])

        }

        if(sum(whole_pair =="[")!= sum(whole_pair =="]")){
          print("num [")
          print(sum(whole_pair =="["))
          print("num ]")
          print(sum(whole_pair =="]"))
          stop("unequal brackets after explode")
        }

        action_last_round <- 1

        #next iteration
        break
      }#end if num left brackets equals 5


      if(explode_this_round==1){
        stop("bug explode_this_round==1")
      }
      # print("i")
      # print(i)
      # print("action_last_round")
      # print(action_last_round)

    }#end loop over i (part of explode)

    if(action_last_round ==0){
      # print("did not explode in this round")
      # action_last_round <- 0
    }

    #check if can split

    i <- 1
    while(i <= length(whole_pair)){

      if(!(whole_pair[i] %in% c(",","[","]")  )){
        check_split_vec <- vector(length = 0)
        k <- i
        while(!(whole_pair[k] %in% c(",","[","]")  )){
          check_split_vec <- c(check_split_vec, whole_pair[k])
          k <- k + 1
        }

        check_split_str <- paste(check_split_vec, collapse = "")


        check_split_num <- as.numeric(check_split_str)

        # print("check_split_num = ")
        # print(check_split_num)
        # print("whole_pair[i:k] = ")
        # print(whole_pair[i:k])


        if(check_split_num >= 10){
          split_this_round <- 1
          split_at <- i
          action_last_round <- 1
          #split
          new_left <- as.character(floor(check_split_num/2))
          new_right <- as.character(ceiling(check_split_num/2))

          if(is.na(new_left)|is.na(new_right)){
            print("new_left = ")
            print(new_left)

            print("new_right = ")
            print(new_right)

            stop("NA value")
          }

          new_whole_pair_split <- c( whole_pair[1:(i-1)],
                           "[",
                           new_left,
                           ",",
                           new_right,
                           "]",
                           whole_pair[k:length(whole_pair)])




          if(sum(whole_pair =="[")!= sum(whole_pair =="]")){
            print("num [")
            print(sum(whole_pair =="["))
            print("num ]")
            print(sum(whole_pair =="]"))
            stop("unequal brackets after split")

          }


          #finished split
          #break from while loop
          break
        }

        i <- k

      }else{
        i <- i +1
      }
    }


    if((explode_this_round ==1)&(split_this_round==1)){
      whole_pair <- new_whole_pair_explode
      #
      # if(explode_at < split_at){
      #   whole_pair <- new_whole_pair_explode
      # }
      # if(split_at < explode_at){
      #   whole_pair <- new_whole_pair_split
      # }
      # if(explode_at == split_at){
      #   stop("explode_at == split_at")
      # }
    }

    if((explode_this_round ==1)&(split_this_round==0)){
      whole_pair <- new_whole_pair_explode
    }
    if((explode_this_round ==0)&(split_this_round==1)){
      whole_pair <- new_whole_pair_split
    }




  }#end while loop, no more actions

  return(whole_pair)
}



restemp <- snail_add(exampledata1_vec, exampledata2_vec)

paste(restemp, collapse = "")




sum1_exampledata <- read.csv("data/day18sum_ex1.txt", header = FALSE, sep = " ",
                        colClasses = "character")

sum2_exampledata <- read.csv("data/day18sum_ex2.txt", header = FALSE, sep = " ",
                             colClasses = "character")

sum3_exampledata <- read.csv("data/day18sum_ex3.txt", header = FALSE, sep = " ",
                             colClasses = "character")

sum4_exampledata <- read.csv("data/day18sum_ex4.txt", header = FALSE, sep = " ",
                             colClasses = "character")


exampledata1 <- read.csv("data/day18_example.txt", header = FALSE, sep = " ",
                             colClasses = "character")

day18data1 <- read.csv("data/day18data.txt", header = FALSE, sep = " ",
                         colClasses = "character")

sum1_exampledata_vec <- strsplit(sum1_exampledata$V1,split=c(''), fixed=TRUE)
sum2_exampledata_vec <- strsplit(sum2_exampledata$V1,split=c(''), fixed=TRUE)
sum3_exampledata_vec <- strsplit(sum3_exampledata$V1,split=c(''), fixed=TRUE)
sum4_exampledata_vec <- strsplit(sum4_exampledata$V1,split=c(''), fixed=TRUE)
exampledata_vec1 <- strsplit(exampledata1$V1,split=c(''), fixed=TRUE)

day18data_vec1 <- strsplit(day18data1$V1,split=c(''), fixed=TRUE)


magnitude <- function(exampledata_vec){

    #find left values
    #find right values

    # print("exampledata_vec =")
    # print(exampledata_vec)

    # print("exampledata_vec[2] =")
    # print(exampledata_vec[2])
    if((exampledata_vec[2] != "[")&(exampledata_vec[5] == "]")){
      #obtain left and right numbers, evaluate and return

      leftnum <- as.numeric(exampledata_vec[2])
      rightnum <- as.numeric(exampledata_vec[4])

      # print("3*leftnum + 2* rightnum")
      # print(3*leftnum + 2* rightnum)
      return(3*leftnum + 2* rightnum)

    }

      comma_ind <- -1
      left_str <- NA
      mag_left <- 0
      nest_count <- 0

    for(i in 2:length(exampledata_vec)){

      if(exampledata_vec[i]=="["){
          nest_count <- nest_count + 1
          # print("nest_count = ")
          # print(nest_count)

      }
      if(exampledata_vec[i]=="]"){
        nest_count <- nest_count - 1
        # print("nest_count = ")
        # print(nest_count)
      }



    if(nest_count==0){
      comma_ind <- i+1
      left_str <- paste(exampledata_vec[2:i], collapse = "")
      # print("exampledata_vec = ")
      # print(exampledata_vec)
      # print("exampledata_vec[2:i] = ")
      # print(exampledata_vec[2:i])


      if(!(exampledata_vec[2] %in% c("[", "]") )){
        mag_left <- as.numeric(exampledata_vec[2])
      }else{
        mag_left <- magnitude(exampledata_vec[2:i])
      }

      # mag_left <- magnitude(exampledata_vec[2:i])

      # print("left_str")
      break
    }


  }# end for loop

  right_str <- NA
  mag_right <- 0
  nest_count <- 0

  for(i in (comma_ind+1):(length(exampledata_vec)-1)){

    # nest_count <- 0
    if(exampledata_vec[i]=="["){
      nest_count <- nest_count + 1
      # print("nest_count = ")
      # print(nest_count)
    }
    if(exampledata_vec[i]=="]"){
      nest_count <- nest_count - 1
      # print("nest_count = ")
      # print(nest_count)
    }

    if(nest_count==0){
      # comma_ind <- i+1
      if(i != length(exampledata_vec)-1){
        stop("bug in right half")
      }
      right_str <- paste(exampledata_vec[(comma_ind+1):i], collapse = "")

      # print("exampledata_vec = ")
      # print(exampledata_vec)
      # print("exampledata_vec[2:i] = ")
      # print(exampledata_vec[(comma_ind+1):i])

      if(!(exampledata_vec[comma_ind+1] %in% c("[", "]") )){
        mag_right <- as.numeric(exampledata_vec[comma_ind+1])
      }else{
        mag_right <- magnitude(exampledata_vec[(comma_ind+1):i])
      }

      break
    }


  }# end for loop
#
#   print("3*mag_left + 2* mag_right")
#   print(3*mag_left + 2* mag_right)

  return(3*mag_left + 2*mag_right)

}




day18func_a <- function(exampledata_vec){

  res_vec <- exampledata_vec[[1]]
  for(i in 2:length(exampledata_vec)){
    res_vec <- snail_add(res_vec,exampledata_vec[[i]] )
    # if(i ==2){
    #     return(res_vec)
    # }
  }

  # print(res_vec)

  return(magnitude(res_vec))

}


# paste(day18func_a(sum1_exampledata_vec), collapse = "")
# paste(day18func_a(sum2_exampledata_vec), collapse = "")
# paste(day18func_a(sum3_exampledata_vec), collapse = "")
# paste(day18func_a(sum4_exampledata_vec), collapse = "")

# paste(day18func_a(exampledata_vec1), collapse = "")
#
# paste(day18func_a(day18data_vec1), collapse = "")
#
# day18data_vec1
#
# exampledata_vec1
#
#
# day18func_a(sum2_exampledata_vec)
# day18func_a(sum3_exampledata_vec)
# day18func_a(sum4_exampledata_vec)



#
# day18func_a(exampledata)
#
# day18func_a(data18a)







day18func_b <- function(exampledata){

  mag_vec <- rep(NA, length(exampledata)*(length(exampledata)-1))

  for(i in 1:length(exampledata)){

    j0 <- 1
    for(j in setdiff(1:length(exampledata), i)){

      # print("i = ")
      # print(i)
      # print("j = ")
      # print(j)

      temp_list <- list(exampledata[[i]], exampledata[[j]])
      mag_vec[(i-1)*(length(exampledata)-1)+j0] <- day18func_a(temp_list)
      j0 <- j0+1
    }

  }


  return(max(mag_vec))

}


# day18func_b(exampledata_vec1)
#
# day18func_b(day18data_vec1)



#
# day18func_b(exampledata)
#
# day18func_b(data18a)









