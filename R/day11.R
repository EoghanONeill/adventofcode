
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data11a <- read.table("data/day11data.txt", header = FALSE,sep = ",",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day11example.txt", header = FALSE, sep = ",",
                        colClasses = "character"
                   )


data11a_list <- strsplit(data11a$V1,split=c(' '), fixed=TRUE)

exampledata_list <- strsplit(exampledata$V1,split=c(' '), fixed=TRUE)

day11func_a <- function(exampledata_list, num_steps){

  res_mat <- matrix(NA, nrow = length(exampledata_list),
                    ncol = nchar(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      res_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  total_flashes <- 0

  for(stepind in 1:num_steps){

    res_mat <- res_mat+1

    temp_res<- res_mat
    has_flashed <- matrix(0,
                          nrow = nrow(res_mat),
                          ncol = ncol(res_mat))

    while( sum((res_mat>9)- (has_flashed==1))>0){
      prev_res<- res_mat

      for(i in 1:nrow(res_mat)){
        for(j in 1:ncol(res_mat)){
          if((prev_res[i,j]>9)&(has_flashed[i,j]==0)){
            has_flashed[i,j]<-1
            i_inds <- setdiff(c(i-1,i,i+1), c(0,nrow(res_mat)+1))
            j_inds <- setdiff(c(j-1,j,j+1), c(0,ncol(res_mat)+1))

            for(i0 in i_inds){
              for(j0 in j_inds){
                if(!((i==i0)&(j==j0))){
                  res_mat[i0,j0] <- res_mat[i0,j0]+1
                }
              }
            }


          }


        }#end loop over j
      }
    }
    total_flashes <- total_flashes + sum(res_mat>9)

    for(i in 1:nrow(res_mat)){
      for(j in 1:ncol(res_mat)){
        if(res_mat[i,j]>9){

          res_mat[i,j] <- 0

        }

      }#end loop over j
    }#end loop over i




  }#end loop over steps

  print("res_mat = ")
  print(res_mat)
  return(total_flashes)

}

# day11func_a(exampledata_list,100)
# day11func_a(data11a_list,100)






day11func_b <- function(exampledata_list){

  res_mat <- matrix(NA, nrow = length(exampledata_list),
                    ncol = nchar(exampledata_list[[1]]))

  for(i in 1:length(exampledata_list)){
    temp_list_elem <- exampledata_list[[i]]
    sep_temp_vec <- strsplit(temp_list_elem,split=c(''), fixed=TRUE)[[1]]
    for(j in 1:length(sep_temp_vec)){
      res_mat[i,j ] <- as.numeric(sep_temp_vec[j])

    }
  }

  total_flashes <- 0

  step_ind <-1

  no_sync <- 1


  while(no_sync==1){

    res_mat <- res_mat+1

    temp_res<- res_mat
    has_flashed <- matrix(0,
                          nrow = nrow(res_mat),
                          ncol = ncol(res_mat))

    while( sum((res_mat>9)- (has_flashed==1))>0){
      prev_res<- res_mat

      for(i in 1:nrow(res_mat)){
        for(j in 1:ncol(res_mat)){
          if((prev_res[i,j]>9)&(has_flashed[i,j]==0)){
            has_flashed[i,j]<-1
            i_inds <- setdiff(c(i-1,i,i+1), c(0,nrow(res_mat)+1))
            j_inds <- setdiff(c(j-1,j,j+1), c(0,ncol(res_mat)+1))

            for(i0 in i_inds){
              for(j0 in j_inds){
                if(!((i==i0)&(j==j0))){
                  res_mat[i0,j0] <- res_mat[i0,j0]+1
                }
              }
            }


          }


        }#end loop over j
      }
    }
    total_flashes <- total_flashes + sum(res_mat>9)

    for(i in 1:nrow(res_mat)){
      for(j in 1:ncol(res_mat)){
        if(res_mat[i,j]>9){

          res_mat[i,j] <- 0

        }

      }#end loop over j
    }#end loop over i


    if(all(res_mat==res_mat[1,1])){
      res_step <- step_ind
      no_sync <-0
    }else{
      step_ind<- step_ind+1
    }

  }#end loop over steps

  # print("res_mat = ")
  # print(res_mat)
  return(res_step)

}

# day11func_b(exampledata_list)
# day11func_b(data11a_list)

