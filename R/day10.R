
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data10a <- read.table("data/day10data.txt", header = FALSE,sep = ",",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day10example.txt", header = FALSE, sep = ",",
                        colClasses = "character"
                   )


data10a_list <- strsplit(data10a$V1,split=c(''), fixed=TRUE)

exampledata_list <- strsplit(exampledata$V1,split=c(''), fixed=TRUE)

day10func_a <- function(exampledata_list){

  rightchars <- c(")","]","}",">" )
  leftchars <- c("(","[","{","<" )


  rightouttercount <- rep(0,4)
  rightpenalties <- c(3,57,1197,25137)


  for(i in 1:length(exampledata_list)){
    temp_string <- exampledata_list[[i]]

    corrupt_found <- 0
    corrupt_char_ind <- NA

    for(j in 1:length(temp_string)){

      if(temp_string[j] %in% rightchars){
        closing_char <- temp_string[j]
        rightcounts <- rep(0,4)
        char_ind <- which(rightchars == closing_char)

        k<- j-1
        closed <- 0

        while(k>0){
          for(ind in 1:4){
            if(temp_string[k]==rightchars[ind]){
              rightcounts[ind] <- rightcounts[ind]+1
            }else{
              if(temp_string[k]==leftchars[ind]){
                rightcounts[ind] <- rightcounts[ind]-1
                # if(rightcounts[ind]==0){
                #
                # }
                if(rightcounts[ind]<0){
                  if(ind==char_ind){
                    closed <- 1
                    break
                  }
                  corrupt_found <- 1
                  corrupt_char_ind <- char_ind
                  break
                }
              }
            }
            if(closed==1){break}
            if(corrupt_found==1){break}

          }#end loop over ind
          if((closed==1)|(corrupt_found==1)){
            break
          }else{
            k <- k-1
          }

        }#end while loop over k

      }#end if rightchar
      if(corrupt_found==1){break}

    }#end loop over j

    if(corrupt_found==1){
      rightouttercount[corrupt_char_ind] <- 1+rightouttercount[corrupt_char_ind]

    }


  }#end loop over i

  # print("rightouttercount = ")
  # print(rightouttercount)
  #
  # print("answer = ")


  res <- 0
  for(i in 1:4){
    res <- res + rightouttercount[i]*rightpenalties[i]
  }

  return(res)

}

# day10func_a(exampledata_list)
# day10func_a(data10a_list)







day10func_b <- function(exampledata_list){

  rightchars <- c(")","]","}",">" )
  leftchars <- c("(","[","{","<" )

  rightouttercount <- rep(0,4)
  rightpenalties <- c(3,57,1197,25137)

  corrupt_lines <- rep(0, length(length(exampledata_list)))

  for(i in 1:length(exampledata_list)){
    temp_string <- exampledata_list[[i]]

    corrupt_found <- 0
    corrupt_char_ind <- NA

    for(j in 1:length(temp_string)){


      if(temp_string[j] %in% rightchars){
        closing_char <- temp_string[j]
        rightcounts <- rep(0,4)
        char_ind <- which(rightchars == closing_char)

        k<- j-1
        closed <- 0

        while(k>0){
          for(ind in 1:4){
            if(temp_string[k]==rightchars[ind]){
              rightcounts[ind] <- rightcounts[ind]+1
            }else{
              if(temp_string[k]==leftchars[ind]){
                rightcounts[ind] <- rightcounts[ind]-1
                # if(rightcounts[ind]==0){
                #
                # }
                if(rightcounts[ind]<0){
                  if(ind==char_ind){
                    closed <- 1
                    break
                  }
                  corrupt_found <- 1
                  corrupt_char_ind <- char_ind
                  break
                }
              }
            }
            if(closed==1){break}
            if(corrupt_found==1){break}

          }#end loop over ind
          if((closed==1)|(corrupt_found==1)){
            break
          }else{
            k <- k-1
          }

        }#end while loop over k

      }#end if rightchar
      if(corrupt_found==1){break}

    }#end loop over j

    if(corrupt_found==1){
      rightouttercount[corrupt_char_ind] <- 1+rightouttercount[corrupt_char_ind]
      corrupt_lines[i] <- 1
    }


  }#end loop over i


  corrupt_inds <- which(corrupt_lines==1)

  # print("corrupt_inds = ")

  # print(corrupt_inds)


  rightchars <- c(")","]","}",">" )
  leftchars <- c("(","[","{","<" )

  rightouttercount <- rep(0,4)
  rightpenalties <- 1:4

  tot_scores_vec <- rep(0,length(setdiff(1:length(exampledata_list),corrupt_inds) ))
  loopcount<- 1

  for(i in setdiff(1:length(exampledata_list),corrupt_inds) ){
    temp_string <- exampledata_list[[i]]

    loopcount <- which(setdiff(1:length(exampledata_list),corrupt_inds) ==i)
    total_score <- 0
    # corrupt_found <- 0
    # corrupt_char_ind <- NA
    completion_vec <-  vector(length = 0)
    rightcounts <- rep(0,4)

    for(j in length(temp_string):1){
      temp_char <- temp_string[j]

      for(ind in 1:4){
        if(temp_string[j]==rightchars[ind]){
          rightcounts[ind] <- rightcounts[ind]+1

        }else{
          if(temp_string[j]==leftchars[ind]){
            rightcounts[ind] <- rightcounts[ind]-1
            # if(rightcounts[ind]==0){
            #
            # }
            if(rightcounts[ind]<0){
              completion_vec <- c( completion_vec, rightchars[ind])
              rightcounts[ind] <- rightcounts[ind]+1


              # print("j =")
              # print(j)
              #
              # print("i =")
              # print(i)
              #
              # print("completion_vec =")
              # print(completion_vec)
              #
              # print("ind =")
              # print(ind)



            }
          }

        }


      }#end loop over ind

    }#end loop over j



    for(j in 1:length(completion_vec)){
      char_ind <- which(rightchars == completion_vec[j])

      tot_scores_vec[loopcount] <- tot_scores_vec[loopcount]*5 + rightpenalties[char_ind]

    }

    loopcount<- loopcount+1
    # print("tot_scores_vec =")
    # print(tot_scores_vec)

  } # end loop over i


  # print("tot_scores_vec =")
  # print(tot_scores_vec)



  return(median(tot_scores_vec))

}

# day10func_b(exampledata_list)
# day10func_b(data10a_list)








