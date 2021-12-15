
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data14a <- read.csv("data/day14data.txt", header = FALSE,sep = " ",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day14example.txt", header = FALSE, sep = " ",
                        colClasses = "character"
                   )



day14func_a <- function(exampledata, num_steps){

  orig_polymer <- exampledata$V1[1]

  from_vec <- exampledata$V1[2:length(exampledata$V1)]
  to_vec <- exampledata$V3[2:length(exampledata$V3)]

  fold_index <- 0


  orig_polymer_vec <- strsplit(orig_polymer, split = "")[[1]]

  for(step in 1:num_steps){

    new_polymer_vec <- rep(NA, length(orig_polymer_vec)-1)


    for(i in 1:(length(orig_polymer_vec)-1)){
      ref_val <- paste0(orig_polymer_vec[i],orig_polymer_vec[i+1])#  substr(orig_polymer,i,i+1)

      new_val <- to_vec[which(from_vec==ref_val)]

      new_polymer_vec[i] <- new_val
    }

    result_vec <- rep(NA, 2*length(orig_polymer_vec)-1)

    result_vec[2*(1:(length(orig_polymer_vec)-1))] <- new_polymer_vec
    result_vec[2*(1:(length(orig_polymer_vec)))-1] <- orig_polymer_vec

    orig_polymer_vec <- result_vec

  }

  elem_counts <- table(orig_polymer_vec)

  res <- max(elem_counts) - min(elem_counts)

  # res <- paste(orig_polymer_vec, collapse = "")
  # print("paper_matrix = ")
  # print(paper_matrix)
  return(res)

}

  # day14func_a(exampledata,10)
  #
  # day14func_a(data14a,10)



day14func_b_temp <- function(orig_polymer,exampledata, num_steps){

  # orig_polymer <- exampledata$V1[1]

  from_vec <- exampledata$V1[2:length(exampledata$V1)]
  to_vec <- exampledata$V3[2:length(exampledata$V3)]

  fold_index <- 0


  orig_polymer_vec <- strsplit(orig_polymer, split = "")[[1]]

  for(step in 1:num_steps){

    new_polymer_vec <- rep(NA, length(orig_polymer_vec)-1)


    for(i in 1:(length(orig_polymer_vec)-1)){
      ref_val <- paste0(orig_polymer_vec[i],orig_polymer_vec[i+1])#  substr(orig_polymer,i,i+1)

      new_val <- to_vec[which(from_vec==ref_val)]

      new_polymer_vec[i] <- new_val
    }

    result_vec <- rep(NA, 2*length(orig_polymer_vec)-1)

    result_vec[2*(1:(length(orig_polymer_vec)-1))] <- new_polymer_vec
    result_vec[2*(1:(length(orig_polymer_vec)))-1] <- orig_polymer_vec

    orig_polymer_vec <- result_vec

  }

  # elem_counts <- table(orig_polymer_vec)
  #
  # res <- max(elem_counts) - min(elem_counts)
  #
 res <- paste(orig_polymer_vec, collapse = "")
  # print("paper_matrix = ")
  # print(paper_matrix)
  return(res)

}

# res <- day14func_b_temp(exampledata, 1)






day14func_b <- function(exampledata#, num_steps
                        ){

  orig_polymer <- exampledata$V1[1]

  from_vec <- exampledata$V1[2:length(exampledata$V1)]
  to_vec <- exampledata$V3[2:length(exampledata$V3)]

  # print("126")
  fold_index <- 0


  orig_polymer_vec <- strsplit(orig_polymer, split = "")[[1]]

  new_from_vec <- from_vec
  new_to_vec <- to_vec

  letters_vec <- unique(to_vec)
  # print("136")

  # for(step in 1:num_steps){

    # new_polymer_vec <- rep(NA, length(orig_polymer_vec)-1)

    # old_from_vec <- new_from_vec
    # old_to_vec <- new_to_vec

    # temp_vec <- vector(length= 0)
    # for(i in 1:length(letters_vec)){
    #   temp_vec <- c(temp_vec, paste0(new_from_vec, letters_vec[i]))
    # }
    # new_from_vec <- temp_vec
    new_to_vec <- rep(NA, length(new_from_vec))
    # print("148")

    new_val_counts <- matrix(0,
                             nrow = length(new_to_vec),
                            ncol = length(letters_vec))

    for(i in 1:length(new_to_vec)){
      # print("table, i == ")
      # print(i)
      # print("new_from_vec[i] ")
      # print(new_from_vec[i])

      new_to_vec[i] <- day14func_b_temp(new_from_vec[i],exampledata, 20)

      temp_polymer_str <- substr(new_to_vec[i], 1, nchar(new_to_vec[i])-1)
      temp_polymer_vec <- strsplit(temp_polymer_str, split = "")[[1]]

      #update counts
      for(j in 1:length(letters_vec)){
        # print("table, j == ")
        # print(j)

        new_val_counts[i,j] <- sum(temp_polymer_vec == letters_vec[j])
      }
    }
    # print("153")

  # }

  # print("157")

  table_10step <- cbind(new_from_vec, new_to_vec)


  new_polymer_vec <- rep(NA, length(orig_polymer_vec)-1)





  for(step in 1:1){

    # print("step == ")
    # print(step)

    new_polymer_vec <- rep(NA, length(orig_polymer_vec)-1)


    for(i in 1:(length(orig_polymer_vec)-1)){

      # print("i == ")
      # print(i)
      ref_val <- paste0(orig_polymer_vec[i],orig_polymer_vec[i+1])#  substr(orig_polymer,i,i+1)

      # print("which(table_10step[,1]==ref_val) == ")
      # print(which(table_10step[,1]==ref_val))
      # print("ref_val == ")
      # print(ref_val)

      new_val <- table_10step[,2][which(table_10step[,1]==ref_val)]

      # print("new_val == ")
      # print(new_val)
      new_polymer_vec[i] <- substr(new_val, 1, nchar(new_val))

    }

    # if(step!=3){
    #   orig_polymer <- paste(new_polymer_vec, collapse = "")
    #   orig_polymer <- paste0(orig_polymer,orig_polymer_vec[length(orig_polymer_vec)] )
    #
    #   orig_polymer_vec <- strsplit(orig_polymer, split = "")[[1]]
    # }


  }

  #expand each element of new_polymer_vec and obtain counts
  #subtracting 1 from the count of the last character in the element
  #then add 1 to the count of orig_polymer_vec[length(orig_polymer_vec)]

  rescount_vec <- rep(0, length(letters_vec))

  rescount_vec[which(letters_vec == orig_polymer_vec[length(orig_polymer_vec)])] <- 1

  #apply to
  for(k in 1:length(new_polymer_vec)){
    # print("k == ")
    # print(k)
    temp_origvec <- strsplit(new_polymer_vec[k], split = "")[[1]]
    # temp_newpolymer_vec <- rep(NA, length(temp_origvec)-1)
    # print("length(temp_origvec) == ")
    # print(length(temp_origvec))

    for(i in 1:(length(temp_origvec)-1)){
      # print("i == ")
      # print(i)
      ref_val <- paste0(temp_origvec[i],temp_origvec[i+1])#  substr(orig_polymer,i,i+1)

      # new_val <- table_10step[,2][which(table_10step[,1]==ref_val)]
      ref_ins <- which(table_10step[,1]==ref_val)

      # temp_polymer_vec <- substr(new_val, 1, nchar(new_val)-1)
      #update counts
      for(j in 1:length(letters_vec)){
        rescount_vec[j] <- rescount_vec[j] + new_val_counts[ref_ins,j] #sum(temp_polymer_vec == letters_vec[j])
      }
      # rescount_vec[which(letters_vec == temp_polymer_vec[length(temp_polymer_vec)])] <- -1 +
      #   rescount_vec[which(letters_vec == temp_polymer_vec[length(temp_polymer_vec)])]

    }

    #pair of elements defined by end of vector and start of next
    # if(k!= length(new_polymer_vec)){
    #   ref_val <- paste0(temp_origvec[length(temp_origvec)],
    #                     substr(new_polymer_vec[k+1],1,1))#  substr(orig_polymer,i,i+1)
    #   # new_val <- table_10step[,2][which(table_10step[,1]==ref_val)]
    #
    #   # new_val_counts[i,j]
    #   ref_ins <- which(table_10step[,1]==ref_val)
    #
    #   # temp_polymer_vec <- substr(new_val, 1, nchar(new_val)-1)
    #   #update counts
    #   for(j in 1:length(letters_vec)){
    #     rescount_vec[j] <- rescount_vec[j] + new_val_counts[ref_ins,j] #sum(temp_polymer_vec == letters_vec[j])
    #   }
    #
    # }

  }


  #expand the


  # elem_counts <- table(orig_polymer_vec)

  res <- max(rescount_vec) - min(rescount_vec)

  # res <- paste(orig_polymer_vec, collapse = "")
  # print("rescount_vec = ")
  # print(rescount_vec)
  return(res)

}


# res <- day14func_b(exampledata)
#
# sprintf("%.1f",res)

# res <- day14func_b(data14a)
#
# sprintf("%.1f",res)
#
# res[,2]

#
#
# result13b <- day14func_b(data14a)
# save(result13b, file = "data/day14result_b.txt")

#
