

data5a <- read.csv("data/day5data_a.csv", header = FALSE,
                   colClasses = "character"
                   )


# strsplit(string,split='->', fixed=TRUE)


exampledata <- read.csv("data/day5example.csv", header = FALSE,
                   colClasses = "character"
                   )

exampledata_table <- matrix(NA, nrow = nrow(exampledata), ncol = 4)

for(i in 1:nrow(exampledata)){
  tempstring <- strsplit(exampledata[i,1],split=c('->'), fixed=TRUE)
  tempstring <- strsplit(tempstring[[1]],split=c(','), fixed=TRUE)

  exampledata_table[i,1] <-  as.numeric(tempstring[[1]][1])
  exampledata_table[i,2] <-  as.numeric(tempstring[[1]][2])
  exampledata_table[i,3] <-  as.numeric(tempstring[[2]][1])
  exampledata_table[i,4] <-  as.numeric(tempstring[[2]][2])
}

data5a_table <- matrix(NA, nrow = nrow(data5a), ncol = 4)

for(i in 1:nrow(data5a)){
  tempstring <- strsplit(data5a[i,1],split=c('->'), fixed=TRUE)
  tempstring <- strsplit(tempstring[[1]],split=c(','), fixed=TRUE)

  data5a_table[i,1] <-  as.numeric(tempstring[[1]][1])
  data5a_table[i,2] <-  as.numeric(tempstring[[1]][2])
  data5a_table[i,3] <-  as.numeric(tempstring[[2]][1])
  data5a_table[i,4] <-  as.numeric(tempstring[[2]][2])
}


# unlist(strsplit(data4a[1,], ","))

#strsplit leaves a space at the start if the line begins with a space
# unlist(strsplit(data4a[35,], "\\s+"))
# texttemp <- scan(text = data4a[35,], what = "")


#' @title day5func_a
#'
#' @description
#' @param data5a data
#' @param
#' @return A number
#' @useDynLib adventofcode, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @examples
#'
#'
#'
#' @export
day5func_a <- function(data5a){

  xmax <- max(cbind(data5a[,1], data5a[,3]))
  ymax <- max(cbind(data5a[,2], data5a[,4]))

  #matrix starts at zero, so add 1
  linemat <- matrix(0,
                    nrow = ymax+1,
                    ncol = xmax+1)


  for (i in 1:nrow(data5a)){

    if((data5a[i,1]== data5a[i,3]) & (data5a[i,2]== data5a[i,4])){
      linemat[data5a[i,2]+1, data5a[i,1]+1] <- 1+ linemat[data5a[i,2]+1, data5a[i,1]+1]
    }else{
      if(data5a[i,1]== data5a[i,3]){
        linemat[(data5a[i,2]+1):(data5a[i,4]+1), data5a[i,1]+1] <- 1+ linemat[(data5a[i,2]+1):(data5a[i,4]+1), data5a[i,1]+1]
      }
      if(data5a[i,2]== data5a[i,4]){
        linemat[data5a[i,2]+1, (data5a[i,1]+1):(data5a[i,3]+1)] <- 1+ linemat[data5a[i,2]+1, (data5a[i,1]+1):(data5a[i,3]+1)]
      }
    }


  }#end loop over i

  return(sum(linemat>=2))

}

#
# day5func_a(exampledata_table)

#answer
# day5func_a(data5a_table)










######################
#part b



day5func_b <- function(data5a){

  xmax <- max(cbind(data5a[,1], data5a[,3]))
  ymax <- max(cbind(data5a[,2], data5a[,4]))

  #matrix starts at zero, so add 1
  linemat <- matrix(0,
                    nrow = ymax+1,
                    ncol = xmax+1)


  for (i in 1:nrow(data5a)){

    if((data5a[i,1]== data5a[i,3]) & (data5a[i,2]== data5a[i,4])){
      linemat[data5a[i,2]+1, data5a[i,1]+1] <- 1+ linemat[data5a[i,2]+1, data5a[i,1]+1]
    }else{
      if(data5a[i,1]== data5a[i,3]){
        linemat[(data5a[i,2]+1):(data5a[i,4]+1), data5a[i,1]+1] <- 1+ linemat[(data5a[i,2]+1):(data5a[i,4]+1), data5a[i,1]+1]
      }else{
        if(data5a[i,2]== data5a[i,4]){
          linemat[data5a[i,2]+1, (data5a[i,1]+1):(data5a[i,3]+1)] <- 1+ linemat[data5a[i,2]+1, (data5a[i,1]+1):(data5a[i,3]+1)]
        }else{
          if(abs(data5a[i,1] - data5a[i,3]) == abs(data5a[i,2] - data5a[i,4])){
            xcoords <- (data5a[i,2]+1):(data5a[i,4]+1)
            ycoords <- (data5a[i,1]+1):(data5a[i,3]+1)

            for(j in 1:length(xcoords)){
              linemat[xcoords[j], ycoords[j]] <- 1+ linemat[xcoords[j], ycoords[j]]
            }
          }
        }
      }
    }


  }#end loop over i

  # print(linemat)
  # print("answer = ")

  return(sum(linemat>=2))

}


# day5func_b(exampledata_table)

#answer
# day5func_b(data5a_table)




