
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")



data20a <- read.csv("data/day20data.txt", header = FALSE,sep = "=",
                   colClasses = "character")

exampledata <- read.csv("data/day20example.txt", header = FALSE, sep = "=",
                        colClasses = "character")

# exampledata2_vec <- strsplit("[1,1]",
#                              split=c(''), fixed=TRUE)[[1]]
#
# nrow((exampledata))
#
# substr(exampledata$V1[1], 1,3)
#

# exampledata <- data20a

day20func_a <- function(exampledata, num_enhance){

  numcols_input <- nchar(exampledata$V1[nrow(exampledata)])

  inputbegins <- NA
  for(i in nrow(exampledata):1){
    if(nchar(exampledata$V1[i])>numcols_input ){
      inputbegins <- i+1
      break
    }
  }

  inputdata <- exampledata$V1[inputbegins:nrow(exampledata)]
  imagealgtemp <- strsplit(as.vector(exampledata$V1[1:(inputbegins-1)]), split = "")

  imagealgo <- vector(length = 0)
  for(i in 1:length(imagealgtemp)){
    imagealgo <- c(imagealgo, imagealgtemp[[i]])
  }

  inputmat <- matrix(NA, nrow = length(inputdata),
                     ncol = nchar(inputdata[1]))

  for(i in 1:length(inputdata)){
    # for(j in 1:nchar(inputdata[1])){
      inputmat[i, ] <- strsplit(inputdata[i], split = "")[[1]]
    # }
  }

  biginput <- matrix(".", nrow = 2*(2*num_enhance +1)+ nrow(inputmat),
                     ncol = 2*(2*num_enhance +1) + ncol(inputmat))

  biginput[(2*num_enhance +2):((2*num_enhance +1)+nrow(inputmat)),(2*num_enhance +2):((2*num_enhance +1)+ncol(inputmat))] <- inputmat

  bigbinmat <- 1*(biginput == "#")

  bigoutput <- matrix(".", nrow = nrow(bigbinmat),
                      ncol = ncol(bigbinmat))

  outsideall <- "."

  for(k in 1:num_enhance){
    for(i in 2:(nrow(bigbinmat)-1)){
      for(j in 2:(ncol(bigbinmat)-1)){
        tempstr <- paste(as.character(as.vector(t(bigbinmat[(i-1):(i+1), (j-1):(j+1)]))), collapse = "")
        binind <- strtoi(tempstr, base = 2)+1

        bigoutput[i,j] <- imagealgo[binind]
      }
    }

    tempstr <- paste(as.character(rep(1*(outsideall == "#"), 9)), collapse = "")
    binind <- strtoi(tempstr, base = 2)+1

    outsideall <- imagealgo[binind]

    bigoutput[1,] <- rep(outsideall, ncol(bigoutput))
    bigoutput[nrow(bigoutput),] <- rep(outsideall, ncol(bigoutput))

    bigoutput[,1] <- rep(outsideall, nrow(bigoutput))
    bigoutput[,ncol(bigoutput)] <- rep(outsideall, nrow(bigoutput))


    bigbinmat <- 1*(bigoutput == "#")

  }

  res <- sum(bigbinmat)
  #
  # reslist <- list()
  # reslist[[1]] <- res
  # reslist[[2]] <- max(dists)


  return(res)

}




###
###
#
# day20func_a(exampledata, 50)
#
# day20func_a(data20a, 50)

