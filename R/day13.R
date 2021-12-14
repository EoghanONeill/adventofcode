
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data13a <- read.table("data/day13data.txt", header = FALSE,sep = "-",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day13example.txt", header = FALSE, sep = "-",
                        colClasses = "character"
                   )

exampledata2 <- read.csv("data/day13example2.txt", header = FALSE, sep = "-",
                        colClasses = "character"
)

exampledata3 <- read.csv("data/day13example3.txt", header = FALSE, sep = "-",
                        colClasses = "character"
)

# data13a_list <- strsplit(data13a$V1,split=c(' '), fixed=TRUE)
#
# exampledata_list <- strsplit(exampledata$V1,split=c(' '), fixed=TRUE)





day13func_a <- function(exampledata){




  fold_index <- 0
  for(i in 1:nrow(exampledata)){
    if(substr(exampledata[i,1],1,4)=="fold"){
      fold_index <- i
      break
    }
  }

  input_nums <- strsplit(exampledata$V1[1:(fold_index-1)],split=c(','), fixed=TRUE)

  input_matrix <- matrix(NA,
                         nrow = (fold_index - 1),
                         ncol = 2)

  for(i in 1:(fold_index-1)){
    for(j in 1:2){
      input_matrix[i,j] <- as.numeric(input_nums[[i]][j])
    }
  }

  nrows_init <- max(input_matrix[,2])+1
  ncols_init <- max(input_matrix[,1])+1

  paper_matrix <- matrix(0,
                         nrow = nrows_init,
                         ncol = ncols_init)

  for(i in 1:(fold_index-1)){

    paper_matrix[(input_matrix[i,2])+1, (input_matrix[i,1])+1] <- 1

  }


  print("paper_matrix = ")
  print(paper_matrix)

  # instruct_ind <- 1

  res <- 0

  for(instruct_ind in (fold_index):(fold_index)){
  # for(instruct_ind in (fold_index):(nrow(exampledata))){
      #(nrow(exampledata)){

    instructions <- exampledata[instruct_ind, 1]
    print("instructions = ")
    print(instructions)

    fold_line <- as.numeric(substr(instructions,14,nchar(instructions)))

    print("substr(instructions,12,12) = ")
    print(substr(instructions,12,12))

    if(substr(instructions,12,12)=="x"){
      #line numbers start at zero

      for(i in 1:nrow(paper_matrix)){
        for(j in 1:(ncol(paper_matrix)- (fold_line+1))){

          if(paper_matrix[i,fold_line+1+j]==1){
            paper_matrix[i,fold_line+1-j] <- 1
          }

        }
      }
      # res <- sum(paper_matrix[,1:(nrow(paper_matrix)- (fold_line+1))]>0)
      res <- sum(paper_matrix[,1:fold_line]>0)

    }

    if(substr(instructions,12,12)=="y"){

      for(i in 1:(nrow(paper_matrix)- (fold_line+1))){
        for(j in 1:ncol(paper_matrix)){

          if(paper_matrix[fold_line+1+i,j]==1){
            paper_matrix[fold_line+1-i,j] <- 1
          }

        }
      }
      # res <- sum(paper_matrix[1:(nrow(paper_matrix)- (fold_line+1)),]>0)
      res <- sum(paper_matrix[1:(fold_line),]>0)

    }


  }



  print("paper_matrix = ")
  print(paper_matrix)
  return(res)

}

# day13func_a(exampledata)
#
# day13func_a(data13a)



day13func_b <- function(exampledata){




  fold_index <- 0
  for(i in 1:nrow(exampledata)){
    if(substr(exampledata[i,1],1,4)=="fold"){
      fold_index <- i
      break
    }
  }

  input_nums <- strsplit(exampledata$V1[1:(fold_index-1)],split=c(','), fixed=TRUE)

  input_matrix <- matrix(NA,
                         nrow = (fold_index - 1),
                         ncol = 2)

  for(i in 1:(fold_index-1)){
    for(j in 1:2){
      input_matrix[i,j] <- as.numeric(input_nums[[i]][j])
    }
  }

  nrows_init <- max(input_matrix[,2])+1
  ncols_init <- max(input_matrix[,1])+1

  paper_matrix <- matrix(0,
                         nrow = nrows_init,
                         ncol = ncols_init)

  for(i in 1:(fold_index-1)){

    paper_matrix[(input_matrix[i,2])+1, (input_matrix[i,1])+1] <- 1

  }


  print("paper_matrix = ")
  print(paper_matrix)

  # instruct_ind <- 1

  res <- 0

  # for(instruct_ind in (fold_index):(fold_index)){
  for(instruct_ind in (fold_index):(nrow(exampledata))){
    #(nrow(exampledata)){

    instructions <- exampledata[instruct_ind, 1]
    print("instructions = ")
    print(instructions)

    fold_line <- as.numeric(substr(instructions,14,nchar(instructions)))

    print("substr(instructions,12,12) = ")
    print(substr(instructions,12,12))

    if(substr(instructions,12,12)=="x"){
      #line numbers start at zero

      for(i in 1:nrow(paper_matrix)){
        for(j in 1:(ncol(paper_matrix)- (fold_line+1))){

          if(paper_matrix[i,fold_line+1+j]==1){
            paper_matrix[i,fold_line+1-j] <- 1
          }

        }
      }
      # res <- sum(paper_matrix[,1:(nrow(paper_matrix)- (fold_line+1))]>0)
      res <- sum(paper_matrix[,1:fold_line]>0)
      paper_matrix <- paper_matrix[,1:fold_line]
    }

    if(substr(instructions,12,12)=="y"){

      for(i in 1:(nrow(paper_matrix)- (fold_line+1))){
        for(j in 1:ncol(paper_matrix)){

          if(paper_matrix[fold_line+1+i,j]==1){
            paper_matrix[fold_line+1-i,j] <- 1
          }

        }
      }
      # res <- sum(paper_matrix[1:(nrow(paper_matrix)- (fold_line+1)),]>0)
      res <- sum(paper_matrix[1:(fold_line),]>0)
      paper_matrix <- paper_matrix[1:(fold_line),]
    }


  }



  print("paper_matrix = ")
  print(paper_matrix)

  print("res = ")
  print(res)

  return(paper_matrix)

}

# day13func_b(exampledata)
#
#
# result13b <- day13func_b(data13a)
# save(result13b, file = "data/day13result_b.txt")

#
