
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data15a <- read.csv("data/day15data.txt", header = FALSE,sep = "",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day15example.txt", header = FALSE, sep = "",
                        colClasses = "character"
                   )

exampledata_list <- strsplit(exampledata$V1,split=c(''), fixed=TRUE)

data15a_list <- strsplit(data15a$V1,split=c(''), fixed=TRUE)



day15func_a <- function(exampledata_list){

  input_mat <- matrix(NA, nrow = length(exampledata_list),
                    ncol = length(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    # temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- exampledata_list[[i]] #strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      input_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  min_dists <- matrix(NA, nrow = length(exampledata_list),
                    ncol = length(exampledata_list[[1]]))

  min_dists[1,1] <- 0

  endloop <- 0
  num_filled <- 1

  edge_mat <- matrix(0, nrow = length(exampledata_list),
                      ncol = length(exampledata_list[[1]]))

  edge_mat[1,1] <- 1


  # which(edge_inds==1,arr.ind = TRUE)

  while(num_filled < nrow(min_dists)*ncol(min_dists) ){

    min_val <- min(min_dists[edge_mat==1])

    edge_indices <- which((edge_mat==1)&(min_val == min_dists),arr.ind = TRUE)

    # edge_indices <- which(edge_mat==1,arr.ind = TRUE)


    # print("edge_mat = ")
    # print(edge_mat)
    # print("min_dists = ")
    # print(min_dists)

    #minimum of edge values

    # print("min_val = ")
    # print(min_val)

    min_val_i <- edge_indices[1,1]
    min_val_j <- edge_indices[1,2]



    # for(i in 1:nrow(edge_indices)){
    #   if(min_val == min_dists[edge_indices[i,1], edge_indices[i,2] ]){
    #     min_val_i <- edge_indices[i,1]
    #     min_val_j <- edge_indices[i,2]
    #     break
    #   }
    # }

# print("nrow(edge_indices) = ")
# print(nrow(edge_indices))
#
# print("edge_indices = ")
# print(edge_indices)
#
# print("edge_mat = ")
# print(edge_mat)
#
#
# print("min_val = ")
# print(min_val)

    for(i in setdiff(c(min_val_i-1, min_val_i+1), c(0, nrow(edge_mat)+1))){
      # print("i = ")
      # print(i)
      # print("min_val_i = ")
      # print(min_val_i)
      # print("min_val_j = ")
      # print(min_val_j)
      # print("min_dists[i, min_val_j] = ")
      # print(min_dists[i, min_val_j])

      if(is.na(min_dists[i, min_val_j])){
        min_dists[i, min_val_j] <- min_dists[min_val_i, min_val_j] + input_mat[i, min_val_j]
        num_filled <- num_filled+1
        # edge_mat[i, min_val_j] <- 1

      }
    }
    # print("end loop")

    for(j in setdiff(c(min_val_j-1, min_val_j+1), c(0, ncol(edge_mat)+1))){
      if(is.na(min_dists[min_val_i, j])){
        min_dists[min_val_i, j] <- min_dists[min_val_i, min_val_j] + input_mat[min_val_i, j]
        num_filled <- num_filled+1
        # edge_mat[min_val_i, j] <- 1

      }
    }


    edge_mat[min_val_ii, min_val_j] <- 0

    edge_mat <- matrix(0, nrow = length(exampledata_list),
                       ncol = length(exampledata_list[[1]]))


    for(i in 1:nrow(edge_mat) ){
      for(j in 1:ncol(edge_mat) ){
        if(is.na(min_dists[i, j]) == FALSE){
          for(i0 in setdiff(c(i-1, i+1), c(0, nrow(edge_mat)+1))){
            if(is.na(min_dists[i0, j])){
              edge_mat[i, j] <- 1
            }
          }
          for(j0 in setdiff(c(j-1, j+1), c(0, ncol(edge_mat)+1))){
            if(is.na(min_dists[i, j0])){
              edge_mat[i, j] <- 1
            }
          }
        }
      }
    }





    #if find solution, just return answer and skip filling in the rest
    if(is.na(min_dists[nrow(min_dists), ncol(min_dists)]) == FALSE){
      return(min_dists[nrow(min_dists), ncol(min_dists)])
    }


    if(num_filled%%500==0){
      print("num_filled = ")
      print(num_filled)
    }

  }



# print(min_dists)

  return(min_dists[nrow(min_dists), ncol(min_dists)])

}


# day15func_a(exampledata_list)
#
# day15func_a(data15a_list)








day15func_b <- function(exampledata_list){

  input_mat <- matrix(NA, nrow = length(exampledata_list),
                      ncol = length(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    # temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- exampledata_list[[i]] #strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      input_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }


  big_input_mat <- matrix(NA,
                          nrow = 5*nrow(input_mat),
                          ncol = 5*ncol(input_mat))

  for(i in 1:5){
    for(j in 1:5){
      add_amount <- i + j -2
      # new_input_mat <- (input_mat +add_amount - 1) %%9 + 1

      big_input_mat[(i-1)*nrow(input_mat) + 1:nrow(input_mat),
                    (j-1)*ncol(input_mat) + 1:ncol(input_mat)] <- (input_mat +add_amount - 1) %%9 + 1
    }
  }

  min_dists <- matrix(NA,
                      nrow = nrow(big_input_mat),
                      ncol = ncol(big_input_mat))

  min_dists[1,1] <- 0

  endloop <- 0
  num_filled <- 1

  edge_mat <- matrix(0,
                     nrow = nrow(big_input_mat),
                     ncol = ncol(big_input_mat))

  edge_mat[1,1] <- 1




  # which(edge_inds==1,arr.ind = TRUE)

  while(num_filled < nrow(big_input_mat)*ncol(big_input_mat) ){

    min_val <- min(min_dists[edge_mat==1])

    edge_indices <- which((edge_mat==1)&(min_val == min_dists),arr.ind = TRUE)

    # edge_indices <- which(edge_mat==1,arr.ind = TRUE)


    # print("edge_mat = ")
    # print(edge_mat)
    # print("min_dists = ")
    # print(min_dists)

    #minimum of edge values

    # print("min_val = ")
    # print(min_val)

    min_val_i <- edge_indices[1,1]
    min_val_j <- edge_indices[1,2]



    # for(i in 1:nrow(edge_indices)){
    #   if(min_val == min_dists[edge_indices[i,1], edge_indices[i,2] ]){
    #     min_val_i <- edge_indices[i,1]
    #     min_val_j <- edge_indices[i,2]
    #     break
    #   }
    # }

    # print("nrow(edge_indices) = ")
    # print(nrow(edge_indices))
    #
    # print("edge_indices = ")
    # print(edge_indices)
    #
    # print("edge_mat = ")
    # print(edge_mat)
    #
    #
    # print("min_val = ")
    # print(min_val)

    for(i in setdiff(c(min_val_i-1, min_val_i+1), c(0, nrow(edge_mat)+1))){
      # print("i = ")
      # print(i)
      # print("min_val_i = ")
      # print(min_val_i)
      # print("min_val_j = ")
      # print(min_val_j)
      # print("min_dists[i, min_val_j] = ")
      # print(min_dists[i, min_val_j])

      if(is.na(min_dists[i, min_val_j])){
        min_dists[i, min_val_j] <- min_dists[min_val_i, min_val_j] + big_input_mat[i, min_val_j]
        num_filled <- num_filled+1
        edge_mat[i, min_val_j] <- 1

      }
    }
    # print("end loop")

    for(j in setdiff(c(min_val_j-1, min_val_j+1), c(0, ncol(edge_mat)+1))){
      if(is.na(min_dists[min_val_i, j])){
        min_dists[min_val_i, j] <- min_dists[min_val_i, min_val_j] + big_input_mat[min_val_i, j]
        num_filled <- num_filled+1
        edge_mat[min_val_i, j] <- 1

      }
    }


    edge_mat[min_val_i, min_val_j] <- 0

    # edge_mat <- matrix(0, nrow = length(exampledata_list),
    #                    ncol = length(exampledata_list[[1]]))
    #
    #
    # for(i in 1:nrow(edge_mat) ){
    #   for(j in 1:ncol(edge_mat) ){
    #     if(is.na(min_dists[i, j]) == FALSE){
    #       for(i0 in setdiff(c(i-1, i+1), c(0, nrow(edge_mat)+1))){
    #         if(is.na(min_dists[i0, j])){
    #           edge_mat[i, j] <- 1
    #         }
    #       }
    #       for(j0 in setdiff(c(j-1, j+1), c(0, ncol(edge_mat)+1))){
    #         if(is.na(min_dists[i, j0])){
    #           edge_mat[i, j] <- 1
    #         }
    #       }
    #     }
    #   }
    # }





    #if find solution, just return answer and skip filling in the rest
    # if(is.na(min_dists[nrow(min_dists), ncol(min_dists)]) == FALSE){
    #   return(min_dists[nrow(min_dists), ncol(min_dists)])
    # }


    if(num_filled%%500==0){
      print("num_filled = ")
      print(num_filled)
    }

  }

  # print(min_dists)
  return(min_dists[nrow(min_dists), ncol(min_dists)])

}



#
# day15func_b(exampledata_list)
#
# day15func_b(data15a_list)








#

day15func_get_dists <- function(input_mat, start_i, start_j, dist_so_far){

  min_dists <- matrix(NA,
                      nrow = nrow(input_mat),
                      ncol = ncol(input_mat))

  min_dists[start_i,start_j] <- dist_so_far + input_mat[start_i,start_j]

  endloop <- 0
  num_filled <- 1

  edge_mat <- matrix(0, nrow = nrow(input_mat),
                     ncol = ncol(input_mat))

  edge_mat[start_i,start_j] <- 1


  # which(edge_inds==1,arr.ind = TRUE)

  while(num_filled < nrow(min_dists)*ncol(min_dists) ){

    edge_indices <- which(edge_mat==1,arr.ind = TRUE)

    # print("edge_mat = ")
    # print(edge_mat)
    # print("min_dists = ")
    # print(min_dists)

    #minimum of edge values
    min_val <- min(min_dists[edge_mat==1])

    # print("min_val = ")
    # print(min_val)

    min_val_i <- 0
    min_val_j <- 0



    for(i in 1:nrow(edge_indices)){
      if(min_val == min_dists[edge_indices[i,1], edge_indices[i,2] ]){
        min_val_i <- edge_indices[i,1]
        min_val_j <- edge_indices[i,2]
        break
      }
    }

    # print("nrow(edge_indices) = ")
    # print(nrow(edge_indices))
    #
    # print("edge_indices = ")
    # print(edge_indices)
    #
    # print("edge_mat = ")
    # print(edge_mat)
    #
    #
    # print("min_val = ")
    # print(min_val)

    for(i in setdiff(c(min_val_i-1, min_val_i+1), c(0, nrow(edge_mat)+1))){
      # print("i = ")
      # print(i)
      # print("min_val_i = ")
      # print(min_val_i)
      # print("min_val_j = ")
      # print(min_val_j)
      # print("min_dists[i, min_val_j] = ")
      # print(min_dists[i, min_val_j])

      if(is.na(min_dists[i, min_val_j])){
        min_dists[i, min_val_j] <- min_dists[min_val_i, min_val_j] + input_mat[i, min_val_j]
        num_filled <- num_filled+1
      }
    }
    # print("end loop")

    for(j in setdiff(c(min_val_j-1, min_val_j+1), c(0, ncol(edge_mat)+1))){
      if(is.na(min_dists[min_val_i, j])){
        min_dists[min_val_i, j] <- min_dists[min_val_i, min_val_j] + input_mat[min_val_i, j]
        num_filled <- num_filled+1
      }
    }


    edge_mat <- matrix(0, nrow = nrow(input_mat),
                       ncol = ncol(input_mat))


    for(i in 1:nrow(edge_mat) ){
      for(j in 1:ncol(edge_mat) ){
        if(is.na(min_dists[i, j]) == FALSE){
          for(i0 in setdiff(c(i-1, i+1), c(0, nrow(edge_mat)+1))){
            if(is.na(min_dists[i0, j])){
              edge_mat[i, j] <- 1
            }
          }
          for(j0 in setdiff(c(j-1, j+1), c(0, ncol(edge_mat)+1))){
            if(is.na(min_dists[i, j0])){
              edge_mat[i, j] <- 1
            }
          }
        }
      }
    }





    #if find solution, just return answer and skip filling in the rest
    if(is.na(min_dists[nrow(min_dists), ncol(min_dists)]) == FALSE){
      # return(min_dists)

      return(min_dists[nrow(min_dists), ncol(min_dists)])
    }


    if(num_filled%%500==0){
      print("num_filled = ")
      print(num_filled)
    }

  }


  #
  # print(min_dists)

  # return(min_dists[nrow(min_dists), ncol(min_dists)])

  return(min_dists)

}






day15func_b <- function(exampledata_list#, num_steps
                        ){

  input_mat <- matrix(NA, nrow = length(exampledata_list),
                      ncol = length(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    # temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- exampledata_list[[i]] #strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      input_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }


  dist_so_far <- 0

  first_out_mat <- day15func_get_dists(input_mat, 1, 1, dist_so_far)

  tile_i <- 1
  tile_j <- 1

  new_input_mat <- input_mat

  end_loop <- 0
  while(end_loop==0){

    min_below <- min(first_out_mat[nrow(first_out_mat),])
    min_right <- min(first_out_mat[,ncol(first_out_mat)])

    print("min_below = ")
    print(min_below)
    print("min_right = ")
    print(min_right)

    # if(min_below==min_right){
    #   stop("equal minimums below and right, must pick one")
    # }

    min_i <- 1
    min_j <- 1

    if(((min_below <=min_right)&tile_i !=5)| tile_j==5){
      min_j <- which.min(first_out_mat[nrow(first_out_mat),])
      if(length(min_j)>1){
        stop("more than one minimum, must pick one")
      }

      tile_i <- tile_i + 1
      dist_so_far <- first_out_mat[nrow(first_out_mat), min_j]

    }
    # if(((min_below > min_right)&tile_j!=5)|tile_i==5){
    else{
      min_i <- which.min(first_out_mat[,ncol(first_out_mat)])
      if(length(min_i)>1){
        stop("more than one minimum, must pick one")
      }

      tile_j <- tile_j + 1
      dist_so_far <- first_out_mat[min_i, ncol(first_out_mat)]

    }


    #create input

    print("tile_i = ")
    print(tile_i)

    print("tile_j = ")
    print(tile_j)

    add_amount <- tile_i + tile_j -2
    new_input_mat <- (input_mat +add_amount - 1) %%9 + 1


    first_out_mat <- day15func_get_dists(new_input_mat,
                                         min_i,
                                         min_j,
                                         dist_so_far)


    if(tile_i==5 & tile_j==5){
      print(new_input_mat)
      return(first_out_mat[nrow(first_out_mat), ncol(first_out_mat)])
    }

  }


  # return(res)

}


# res <- day15func_b(exampledata_list)
#
# res <- day15func_b(data15a_list)



day15_dijkstra_a <- function(exampledata_list, start_i, start_j, dist_so_far){


  input_mat <- matrix(NA, nrow = length(exampledata_list),
                      ncol = length(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    # temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- exampledata_list[[i]] #strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      input_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }



  #1 if still in Q
  Qmat <- matrix(1,
                     nrow = nrow(input_mat),
                     ncol = ncol(input_mat))

  distmat <- matrix(Inf,
                 nrow = nrow(input_mat),
                 ncol = ncol(input_mat))

  # prevmat_i <- matrix(NA,
  #                   nrow = nrow(input_mat),
  #                   ncol = ncol(input_mat))
  #
  # prevmat_j <- matrix(NA,
  #                     nrow = nrow(input_mat),
  #                     ncol = ncol(input_mat))


  distmat[start_i, start_j] <- 0

  while(sum(Qmat)!=0){
    #first element is i ind, second element is j ind
    # print(distmat)
    u_inds <- which((Qmat==1)&(distmat==min(distmat[Qmat==1])),arr.ind = TRUE)

    # print(u_inds)

    u_inds <- u_inds[1,]

    Qmat[u_inds[1], u_inds[2]] <- 0

    for(i in setdiff( c(u_inds[1]-1, u_inds[1]+1), c(0, nrow(Qmat)+1))){
      if(Qmat[i, u_inds[2]]==1){
        alt <- distmat[u_inds[1], u_inds[2]] + input_mat[i, u_inds[2]]
        if(alt< distmat[i, u_inds[2]]){
          distmat[i, u_inds[2]] <- alt
          # prevmat_i[i, u_inds[2]] <- u_inds[1]
          # prevmat_j[i, u_inds[2]] <- u_inds[2]
        }
      }
    }
    for(j in setdiff( c(u_inds[2]-1, u_inds[2]+1), c(0, ncol(Qmat)+1))){
      if(Qmat[u_inds[1], j]==1){
        alt <- distmat[u_inds[1], u_inds[2]] + input_mat[u_inds[1], j]
        if(alt< distmat[u_inds[1], j]){
          distmat[u_inds[1], j] <- alt
          # prevmat_i[u_inds[1], j] <- u_inds[1]
          # prevmat_j[u_inds[1], j] <- u_inds[2]
        }
      }
    }


  }

  # print("distmat = ")
  # print(distmat)

  return(distmat[nrow(distmat), ncol(distmat)])

}


# day15_dijkstra_a(exampledata_list, 1, 1, 0)
#
# day15_dijkstra_a(data15a_list, 1, 1, 0)





day15_dijkstra_b <- function(exampledata_list, start_i, start_j, dist_so_far){


  input_mat <- matrix(NA, nrow = length(exampledata_list),
                      ncol = length(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    # temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- exampledata_list[[i]] #strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      input_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  big_input_mat <- matrix(NA,
                          nrow = 5*nrow(input_mat),
                          ncol = 5*ncol(input_mat))

  for(i in 1:5){
    for(j in 1:5){
      add_amount <- i + j -2
      # new_input_mat <- (input_mat +add_amount - 1) %%9 + 1

      big_input_mat[(i-1)*nrow(input_mat) + 1:nrow(input_mat),
                    (j-1)*ncol(input_mat) + 1:ncol(input_mat)] <- (input_mat +add_amount - 1) %%9 + 1
    }
  }

  #1 if still in Q
  Qmat <- matrix(1,
                 nrow = nrow(big_input_mat),
                 ncol = ncol(big_input_mat))

  distmat <- matrix(Inf,
                    nrow = nrow(big_input_mat),
                    ncol = ncol(big_input_mat))

  # prevmat_i <- matrix(NA,
  #                   nrow = nrow(input_mat),
  #                   ncol = ncol(input_mat))
  #
  # prevmat_j <- matrix(NA,
  #                     nrow = nrow(input_mat),
  #                     ncol = ncol(input_mat))


  distmat[start_i, start_j] <- 0

  while(sum(Qmat)!=0){
    #first element is i ind, second element is j ind
    # print(distmat)
    u_inds <- which((Qmat==1)&(distmat==min(distmat[Qmat==1])),arr.ind = TRUE)

    # print(u_inds)

    u_inds <- u_inds[1,]

    Qmat[u_inds[1], u_inds[2]] <- 0

    for(i in setdiff( c(u_inds[1]-1, u_inds[1]+1), c(0, nrow(Qmat)+1))){
      if(Qmat[i, u_inds[2]]==1){
        alt <- distmat[u_inds[1], u_inds[2]] + big_input_mat[i, u_inds[2]]
        if(alt< distmat[i, u_inds[2]]){
          distmat[i, u_inds[2]] <- alt
          # prevmat_i[i, u_inds[2]] <- u_inds[1]
          # prevmat_j[i, u_inds[2]] <- u_inds[2]
        }
      }
    }
    for(j in setdiff( c(u_inds[2]-1, u_inds[2]+1), c(0, ncol(Qmat)+1))){
      if(Qmat[u_inds[1], j]==1){
        alt <- distmat[u_inds[1], u_inds[2]] + big_input_mat[u_inds[1], j]
        if(alt< distmat[u_inds[1], j]){
          distmat[u_inds[1], j] <- alt
          # prevmat_i[u_inds[1], j] <- u_inds[1]
          # prevmat_j[u_inds[1], j] <- u_inds[2]
        }
      }
    }


  }

  # print("distmat = ")
  # print(distmat)

  return(distmat[nrow(distmat), ncol(distmat)])

}


# day15_dijkstra_b(exampledata_list, 1, 1, 0)
#
# day15_dijkstra_b(data15a_list, 1, 1, 0)




#
# sprintf("%.1f",res)

# res <- day15func_b(data15a)
#
# sprintf("%.1f",res)
#
# res[,2]

#
#
# result13b <- day15func_b(data15a)
# save(result13b, file = "data/day15result_b.txt")

#
