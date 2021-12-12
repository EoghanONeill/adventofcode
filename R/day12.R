
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data12a <- read.table("data/day12data.txt", header = FALSE,sep = "-",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day12example.txt", header = FALSE, sep = "-",
                        colClasses = "character"
                   )

exampledata2 <- read.csv("data/day12example2.txt", header = FALSE, sep = "-",
                        colClasses = "character"
)

exampledata3 <- read.csv("data/day12example3.txt", header = FALSE, sep = "-",
                        colClasses = "character"
)

# data12a_list <- strsplit(data12a$V1,split=c(' '), fixed=TRUE)
#
# exampledata_list <- strsplit(exampledata$V1,split=c(' '), fixed=TRUE)



day12_recur <- function(exampledata, begin_val){

  if(nrow(exampledata)==0){return(0)}

  num_paths <- 0
  for(i in 1:nrow(exampledata)){
    if(exampledata[i,1]==begin_val){

      if(exampledata[i,2]=="start"){
        next
      }else{
        if(exampledata[i,2]=="end"){
          num_paths <- num_paths+1
        }else{
          new_val <- exampledata[i,2]
          if(begin_val != toupper(begin_val)){

            remove_inds <- vector(length=0)
            for(i0 in 1:nrow(exampledata)){
              if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                remove_inds <- c(remove_inds, i0)
              }
            }

            num_paths <- num_paths + day12_recur(exampledata[-remove_inds,], new_val)
          }else{
            num_paths <- num_paths + day12_recur(exampledata, new_val)
          }

        }
      }

    }

  }

  #add right to left paths

  for(i in 1:nrow(exampledata)){
    if(exampledata[i,2]==begin_val){

      if(exampledata[i,1]=="start"){
        next
      }else{
        if(exampledata[i,1]=="end"){
          num_paths <- num_paths+1
        }else{
          new_val <- exampledata[i,1]
          if(begin_val != toupper(begin_val)){
            remove_inds <- vector(length=0)
            for(i0 in 1:nrow(exampledata)){
              if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                remove_inds <- c(remove_inds, i0)
              }
            }

            num_paths <- num_paths + day12_recur(exampledata[-remove_inds,], new_val)
          }else{
            num_paths <- num_paths + day12_recur(exampledata, new_val)
          }

        }
      }

    }

  }

  # print("num_paths = " )
  #
  # print(num_paths)
  #
  # print("exampledata = " )
  #
  # print(exampledata)

  return(num_paths)
}



day12func_a <- function(exampledata){


  total_num_paths <- 0

  begin_val <- "start"


  # remove_inds <- vector(length=0)
  # #remove small caves only visited by one small cave
  # for(i in 1:nrow(exampledata)){
  #   for(j in 1:2){
  #     if(!(exampledata[i,j] %in% c("start", "end"))){
  #       if((sum(exampledata==exampledata[i,j])==1)&
  #          (toupper(exampledata[i,j]) != exampledata[i,j])&
  #          (toupper(exampledata[i,3-j]) != exampledata[i,3-j])){
  #         remove_inds <- c(remove_inds, i)
  #       }
  #     }
  #   }
  # }
  #
  # if(length(remove_inds)==0){
  #
  # }else{
  #   exampledata <- exampledata[-remove_inds, ]
  # }


  # print(exampledata)


  total_num_paths <- total_num_paths + day12_recur(exampledata, begin_val)


  # print("res_mat = ")
  # print(res_mat)
  return(total_num_paths)

}

# day12func_a(exampledata)
# day12func_a(exampledata2)
# day12func_a(exampledata3)
#
# day12func_a(data12a)





day12_recur_b <- function(exampledata, begin_val,
                          cave_names, visit_countvec, count_lim){

  if(nrow(exampledata)==0){
    # print("dead end")
    return(0)
  }

  # if(begin_val=="start"){
  #   print("new path")
  # }

  num_paths <- 0
  for(i in 1:nrow(exampledata)){
    if(exampledata[i,1]==begin_val){

      if(exampledata[i,2]=="start"){
        next
      }else{
        if(exampledata[i,2]=="end"){
          num_paths <- num_paths+1
          # print("end path")
          # print("num_paths = ")
          # print(num_paths)

        }else{
          new_val <- exampledata[i,2]
          if(begin_val != toupper(begin_val)){

            # print("begin_val = ")
            # print(begin_val)
            # print("new_val = ")
            # print(new_val)

            new_countvec <- visit_countvec

            new_countvec[which(cave_names==begin_val)] <- 1+ new_countvec[which(cave_names==begin_val)]


            if( (count_lim ==1) |(begin_val == "start") ){
              remove_inds <- vector(length=0)
              for(i0 in 1:nrow(exampledata)){
                if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                  remove_inds <- c(remove_inds, i0)
                }
              }

              num_paths <- num_paths + day12_recur_b(exampledata[-remove_inds,], new_val,
                                                   cave_names, new_countvec,
                                                   count_lim)
            }else{
              if(new_countvec[which(cave_names==begin_val)] >=2){
                #reduce number of possible visits for all other small caves to 1

                #multiplication here is a trick
                #to ensure do not increase count for caves that have not been visited at all
                new_countvec <- new_countvec*(1+ new_countvec)

                # new_countvec <- new_countvec + 1

                #remove current cave (that has been visited twice)
                remove_inds <- vector(length=0)

                #and remove other small caves that have been visited once
                for(name_ind in 1:length(cave_names)){
                  if(new_countvec[name_ind] >=2){
                    for(i0 in 1:nrow(exampledata)){
                      if((exampledata[i0,1] == cave_names[name_ind])|(exampledata[i0,2]== cave_names[name_ind])){
                        remove_inds <- c(remove_inds, i0)
                      }
                    }
                  }
                }

                #if new cave is small and has already been visited once, cannot visit
                if(new_val != toupper(new_val)){
                  if(new_countvec[which(cave_names==new_val)] >= 2){
                    next
                  }
                }

                for(i0 in 1:nrow(exampledata)){
                  if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                    remove_inds <- c(remove_inds, i0)
                  }
                }

                new_count_lim <- 1
                num_paths <- num_paths + day12_recur_b(exampledata[-remove_inds,], new_val,
                                                     cave_names, new_countvec, new_count_lim)

              }else{
                num_paths <- num_paths + day12_recur_b(exampledata, new_val,
                                                     cave_names, new_countvec, count_lim)
              }
            }
          }else{
            num_paths <- num_paths + day12_recur_b(exampledata, new_val,
                                                 cave_names, visit_countvec, count_lim)
          }

        }
      }

    }

  }

  #add right to left paths

  for(i in 1:nrow(exampledata)){
    if(exampledata[i,2]==begin_val){

      if(exampledata[i,1]=="start"){
        next
      }else{
        if(exampledata[i,1]=="end"){
          num_paths <- num_paths+1
          # print("end path")
          # print("num_paths = ")
          # print(num_paths)
        }else{
          new_val <- exampledata[i,1]
          if(begin_val != toupper(begin_val)){
            # print("begin_val = ")
            # print(begin_val)
            # print("new_val = ")
            # print(new_val)

            new_countvec <- visit_countvec

            new_countvec[which(cave_names==begin_val)] <- 1+ new_countvec[which(cave_names==begin_val)]


            if( (count_lim ==1) |(begin_val == "start") ){
              remove_inds <- vector(length=0)
              for(i0 in 1:nrow(exampledata)){
                if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                  remove_inds <- c(remove_inds, i0)
                }
              }

              num_paths <- num_paths + day12_recur_b(exampledata[-remove_inds,], new_val,
                                                   cave_names, new_countvec,
                                                   count_lim)
            }else{
              if(new_countvec[which(cave_names==begin_val)] >=2){
                #reduce number of possible visits for all other small caves to 1

                #multiplication here is a trick
                #to ensure do not increase count for caves that have not been visited at all
                new_countvec <- new_countvec*(1+ new_countvec)

                # new_countvec <- new_countvec + 1

                #remove current cave (that has been visited twice)
                remove_inds <- vector(length=0)

                #and remove other small caves that have been visited once
                for(name_ind in 1:length(cave_names)){
                  if(new_countvec[name_ind] >=2){
                    for(i0 in 1:nrow(exampledata)){
                      if((exampledata[i0,1] == cave_names[name_ind])|(exampledata[i0,2]== cave_names[name_ind])){
                        remove_inds <- c(remove_inds, i0)
                      }
                    }
                  }
                }

                #if new cave is small and has already been visited once, cannot visit
                if(new_val != toupper(new_val)){
                  if(new_countvec[which(cave_names==new_val)] >= 2){
                    next
                  }
                }

                for(i0 in 1:nrow(exampledata)){
                  if((exampledata[i0,1] == begin_val)|(exampledata[i0,2]== begin_val)){
                    remove_inds <- c(remove_inds, i0)
                  }
                }

                new_count_lim <- 1
                num_paths <- num_paths + day12_recur_b(exampledata[-remove_inds,], new_val,
                                                       cave_names, new_countvec, new_count_lim)

              }else{
                num_paths <- num_paths + day12_recur_b(exampledata, new_val,
                                                       cave_names, new_countvec, count_lim)
              }
            }
          }else{
            num_paths <- num_paths + day12_recur_b(exampledata, new_val,
                                                   cave_names, visit_countvec, count_lim)
          }

        }
      }

    }

  }

  # print("num_paths = " )
  #
  # print(num_paths)
  #
  # print("exampledata = " )
  #
  # print(exampledata)

  return(num_paths)
}



day12func_b <- function(exampledata){


  total_num_paths <- 0

  begin_val <- "start"

  cave_names0 <- unique(as.vector(as.matrix(exampledata)))

  cave_names0 <- setdiff(cave_names0, c("start", "end"))
  cave_names <- cave_names0

  for(i in 1:length(cave_names0)){
    if(cave_names0[i]==toupper(cave_names0[i])){
      cave_names <- setdiff(cave_names,cave_names0[i])
    }

  }
  print(cave_names)


  visit_countvec <- rep(0, length(cave_names))


  count_lim <- 2
  total_num_paths <- total_num_paths + day12_recur_b(exampledata, begin_val,
                                                   cave_names, visit_countvec, count_lim)


  # print("res_mat = ")
  # print(res_mat)
  return(total_num_paths)

}

# day12func_b(exampledata)
# day12func_b(exampledata2)
# day12func_b(exampledata3)
#
# day12func_b(data12a)


