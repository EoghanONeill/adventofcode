
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data9a <- read.table("data/day9data.txt", header = FALSE,sep = ",",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day9example.csv", header = FALSE, sep = ",",
                        colClasses = "character"
                   )


data9a_list <- strsplit(data9a$V1,split=c(' '), fixed=TRUE)

exampledata_list <- strsplit(exampledata$V1,split=c(' '), fixed=TRUE)

day9func_a <- function(exampledata_list){


  # median(exampledata_table)
  res_mat <- matrix(NA, nrow = length(exampledata_list),
                    ncol = nchar(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      res_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  count_running <- 0

  for(i in 1:nrow(res_mat)){
    for(j in 1:ncol(res_mat)){

      i_inds <- setdiff(c(i-1,i,i+1), c(0,nrow(res_mat)+1))
      j_inds <- setdiff(c(j-1,j,j+1), c(0,ncol(res_mat)+1))

      is_lowest <-1
      for(i0 in i_inds){
        for(j0 in j_inds){
          if(!((i==i0)&(j==j0))){
            if(res_mat[i,j ] > res_mat[i0,j0 ]){
              is_lowest <-0
            }
          }
        }
      }

      if(is_lowest==1){
        count_running <- count_running + res_mat[i,j ] + 1
      }


    }
  }

  return(count_running)

}

day9func_a(exampledata_list)
day9func_a(data9a_list)








basincount_recur <- function(res_mat, i, j, basinsearch_mat){

  i_inds <- setdiff(c(i-1,i,i+1), c(0,nrow(res_mat)+1))
  j_inds <- setdiff(c(j-1,j,j+1), c(0,ncol(res_mat)+1))

  for(i0 in i_inds){
    for(j0 in j_inds){
      if((i0 %in% c(i-1,i+1) )&(j0 %in% c(j-1,j+1))){
        next
      }

      if(!((i==i0)&(j==j0))){
        if((res_mat[i0,j0]!=9)&(res_mat[i,j]<res_mat[i0,j0])){
          basinsearch_mat[i0,j0]<- 1
          basinsearch_mat <- basinsearch_mat + basincount_recur(res_mat, i0, j0, basinsearch_mat)

        }else{
          # basinsearch_mat[i0,j0]<- 1

          # basinsearch_mat[i0,j0]<- 1

          # return(basinsearch_mat)
        }

      }else{
        basinsearch_mat[i0,j0]<- 1

        # return(basinsearch_mat)
      }

    }
  }
  return(basinsearch_mat)

}





day9func_b <- function(exampledata_list){


  # median(exampledata_table)
  res_mat <- matrix(NA, nrow = length(exampledata_list),
                    ncol = nchar(exampledata_list[[1]]))

  lowest_mat <- matrix(0, nrow = length(exampledata_list),
                    ncol = nchar(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      res_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  # count_running <- 0

  for(i in 1:nrow(res_mat)){
    for(j in 1:ncol(res_mat)){

      i_inds <- setdiff(c(i-1,i,i+1), c(0,nrow(res_mat)+1))
      j_inds <- setdiff(c(j-1,j,j+1), c(0,ncol(res_mat)+1))

      is_lowest <-1
      for(i0 in i_inds){
        for(j0 in j_inds){
          if(!((i==i0)&(j==j0))){
            if(res_mat[i,j ] > res_mat[i0,j0 ]){
              is_lowest <-0
            }
          }
        }
      }

      if(is_lowest==1){
        # count_running <- count_running + res_mat[i,j ] + 1
        lowest_mat[i,j] <- 1

      }


    }
  }

  basinsizes_mat <- matrix(0, nrow = length(exampledata_list),
                       ncol = nchar(exampledata_list[[1]]))

  for(i in 1:nrow(res_mat)){
    for(j in 1:ncol(res_mat)){
        if(lowest_mat[i,j]==1){



          basinsearch_mat <- matrix(0, nrow = length(exampledata_list),
                                   ncol = nchar(exampledata_list[[1]]))


          basinsearch_mat <- basincount_recur(res_mat, i, j, basinsearch_mat)

          basinsizes_mat[i,j] <- sum(basinsearch_mat>0)




        }
    }
  }

  res <- prod(sort(as.vector(basinsizes_mat), decreasing = TRUE)[1:3])

  return(res)

}

# day9func_b(exampledata_list)
# day9func_b(data9a_list)









