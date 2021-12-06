

data4a <- read.csv("data/day4data_a.csv", header = FALSE,
                   colClasses = "character"
                   )


exampledata <- read.csv("data/day4example.csv", header = FALSE,
                   colClasses = "character"
)

# unlist(strsplit(data4a[1,], ","))

#strsplit leaves a space at the start if the line begins with a space
# unlist(strsplit(data4a[35,], "\\s+"))
# texttemp <- scan(text = data4a[35,], what = "")



day4func_a <- function(data4a){
  called_nums <- unlist(strsplit(data4a[1,], ","))
  num_rounds <- length(called_nums)

  num_boards <- (nrow(data4a)-1)/5

  marked_boards <- array(data = rep(0, 5*5*num_boards),
                         dim = c(5,5,num_boards))

  bingo_found <- 0
  winner_board <- -1
  last_called_num <- -1

  for(round_ind in 1:num_rounds){
    round_num <- called_nums[round_ind]

    for(board_ind in 1:num_boards){

      bingo_this_board <- 0
      for(i in 1:5){
        texttemp <- scan(text = data4a[1+(board_ind-1)*5+i,],
                         what = "", quiet = TRUE)
        for(j in 1:5){
          if(texttemp[j] == round_num){
            marked_boards[i,j,board_ind] <- 1

            if((sum(marked_boards[i,,board_ind])==5)|(sum(marked_boards[,j,board_ind])==5)){
              if(bingo_found==1 & bingo_this_board!=1 ){
                stop("already found a bingo in this round")
              }

              bingo_found <- 1
              bingo_this_board <- 1
              winner_board <- board_ind
              last_called_num <- as.numeric(round_num)

            }


          }

        }#end j loop

        # if(bingo_this_board==1){
        #   break
        # }

      }#end i loop

      if(bingo_found==1){
        print("last board  = ")
        print(board_ind)
        break
      }

      # if( (board_ind==num_boards) & (bingo_found==1)   ){
      #   break
      # }

    }#end loop over boards

    if(bingo_found==1){
      print("last round = ")
      print(round_ind)
      break
    }

  }#end loop over rounds

  #sum all unmarked numbers in winning board
  running_sum <- 0
  for (i in 1:5){
    for(j in 1:5){
      if(marked_boards[i,j,winner_board] == 0){
        running_sum <- running_sum +
          as.numeric(scan(text = data4a[1+(board_ind-1)*5+i,],
             what = "", quiet = TRUE)[j])

      }
    }
  }

  print("running_sum = ")
  print(running_sum)
  print("last_called_num = ")
  print(last_called_num)

  print("Answer = ")
  return(running_sum*last_called_num)
}


# day4func_a(exampledata)

#answer
# day4func_a(data4a)

######################
#part b



day4func_b <- function(data4a){
  called_nums <- unlist(strsplit(data4a[1,], ","))
  num_rounds <- length(called_nums)

  num_boards <- (nrow(data4a)-1)/5

  bingo_boardvec <- rep(0, num_boards)

  marked_boards <- array(data = rep(0, 5*5*num_boards),
                         dim = c(5,5,num_boards))

  bingo_found <- 0
  winner_board <- -1
  last_called_num <- -1

  for(round_ind in 1:num_rounds){
    round_num <- called_nums[round_ind]

    for(board_ind in 1:num_boards){

      if(bingo_boardvec[board_ind]==1){
        next
      }

      # bingo_this_board <- 0
      for(i in 1:5){
        # if(sum(bingo_boardvec) = num_boards){
        #   break
        # }
        texttemp <- scan(text = data4a[1+(board_ind-1)*5+i,],
                         what = "", quiet = TRUE)
        for(j in 1:5){
          if(texttemp[j] == round_num){
            marked_boards[i,j,board_ind] <- 1

            if((sum(marked_boards[i,,board_ind])==5)|(sum(marked_boards[,j,board_ind])==5)){
              # if(bingo_found==1 & bingo_this_board!=1 ){
              #   stop("already found a bingo in this round")
              # }



              # bingo_this_board <- 1
              winner_board <- board_ind
              last_called_num <- as.numeric(round_num)

              bingo_boardvec[board_ind] <- 1

              if(sum(bingo_boardvec) == num_boards){
                #indicates last bingo found
                print("last bingo found")
                bingo_found <- 1
              }

            }

            # if(sum(bingo_boardvec) = num_boards){
            #   break
            # }

          }

        }#end j loop

        # if(bingo_this_board==1){
        #   break
        # }

      }#end i loop

      if(bingo_found==1){
        print("last board  = ")
        print(board_ind)
        break
      }

      # if( (board_ind==num_boards) & (bingo_found==1)   ){
      #   break
      # }

    }#end loop over boards

    if(bingo_found==1){
      print("last round = ")
      print(round_ind)
      break
    }

  }#end loop over rounds

  #sum all unmarked numbers in winning board
  running_sum <- 0
  for (i in 1:5){
    for(j in 1:5){
      if(marked_boards[i,j,winner_board] == 0){
        running_sum <- running_sum +
          as.numeric(scan(text = data4a[1+(board_ind-1)*5+i,],
                          what = "", quiet = TRUE)[j])

      }
    }
  }
  bingo_boardvec

  # print("bingo_boardvec = ")
  # print(bingo_boardvec)

  print("winner_board = ")
  print(winner_board)
  print("running_sum = ")
  print(running_sum)
  print("last_called_num = ")
  print(last_called_num)

  print("Answer = ")
  return(running_sum*last_called_num)
}


# day4func_b(exampledata)

#answer
# day4func_b(data4a)


