
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")


decoder <- read.csv("data/day16decoder.txt", header = FALSE,sep = " ",
                    colClasses = "character")

data16a <- read.csv("data/day16data.txt", header = FALSE,sep = "",
                   colClasses = "character")

exampledata <- read.csv("data/day16example.txt", header = FALSE, sep = "",
                        colClasses = "character")

example_op0 <- read.csv("data/day16example_optype0.txt", header = FALSE, sep = "",
                        colClasses = "character")

example_op1 <- read.csv("data/day16example_optype1.txt", header = FALSE, sep = "",
                        colClasses = "character")

exampledata1 <- read.csv("data/day16example1.txt", header = FALSE, sep = "",
                        colClasses = "character")

exampledata2 <- read.csv("data/day16example2.txt", header = FALSE, sep = "",
                        colClasses = "character")

exampledata3 <- read.csv("data/day16example3.txt", header = FALSE, sep = "",
                        colClasses = "character")

exampledata4 <- read.csv("data/day16example4.txt", header = FALSE, sep = "",
                        colClasses = "character")

data16a_vec <- strsplit(data16a$V1,split=c(''), fixed=TRUE)[[1]]

exampledata_vec <- strsplit(exampledata$V1,split=c(''), fixed=TRUE)[[1]]
example_op0_vec <- strsplit(example_op0$V1,split=c(''), fixed=TRUE)[[1]]
example_op1_vec <- strsplit(example_op1$V1,split=c(''), fixed=TRUE)[[1]]
exampledata1_vec <- strsplit(exampledata1$V1,split=c(''), fixed=TRUE)[[1]]
exampledata2_vec <- strsplit(exampledata2$V1,split=c(''), fixed=TRUE)[[1]]
exampledata3_vec <- strsplit(exampledata3$V1,split=c(''), fixed=TRUE)[[1]]
exampledata4_vec <- strsplit(exampledata4$V1,split=c(''), fixed=TRUE)[[1]]



convert <- function(x) {
  y <- as.numeric(x)
  sum(y * 2^rev((seq_along(y)-1)))
}

day16_recur <- function(orig_decoded_vec, sum_vnums){

  version_vec <- orig_decoded_vec[1:3]
  typeID_vec <- orig_decoded_vec[4:6]
  typeID_str <- paste(typeID_vec, collapse = "")

  typeID_int <- strtoi(typeID_str, base = 2)

  vnum_str <- paste(version_vec, collapse = "")
  vnum_int <- strtoi(vnum_str, base = 2)
  sum_vnums <- sum_vnums + vnum_int

  end_lit <- 0

  outval <- NA

  if(all(typeID_vec == c("1","0", "0"))){
    #literal



    # numn_coded_digits <- floor((length(orig_decoded_vec)-6)/5)

    #alternatively go to 1, 6, 11, etc until a zero
    i <- 1
    lit_binrep <- vector(length = 0)
    while(orig_decoded_vec[7+5*(i-1)]==1){
      lit_binrep <- c(lit_binrep, orig_decoded_vec[(8+5*(i-1)):(11+5*(i-1))])
      i <- i + 1
    }
    #add final 4 numbers
    lit_binrep <- c(lit_binrep, orig_decoded_vec[(8+5*(i-1)):(11+5*(i-1))])

    # binrep_str <- paste(lit_binrep, collapse = "")
    # binrep_int <- strtoi(binrep_str, base = 2)

    binrep_int <- convert(lit_binrep)

    if(is.na(binrep_int)){
      print("lit_binrep = ")
      print(lit_binrep)
    }

    outval <- binrep_int

    end_lit <- 7+5*(i) #orig_decoded_vec[7+5*(i)]

  }else{
    #operator
    #check length (type) ID
    lencount <- 0

    packvals <- vector(length=0)

    if(orig_decoded_vec[7] =="0"){
      #next 15 give total (i.e. sum of) length of subpackets

      len_binrep <- orig_decoded_vec[8:22]
      len_str <- paste(len_binrep, collapse = "")
      len_int <- strtoi(len_str, base = 2)

      #assuming all literals?
      lencount <- 23

      while(lencount <= 22+len_int){
        restemp <- day16_recur(orig_decoded_vec[lencount:length(orig_decoded_vec)] , sum_vnums)

        sum_vnums <- restemp[[1]]
        lencount <- lencount+restemp[[2]]-1

        packvals <- c(packvals, restemp[[3]])
      }

      #assuming no extra zeros if within other packets?


    }else{
      #next 11 give number of sub-packets immediately contained
      numsub_binrep <- orig_decoded_vec[8:18]
      numsub_str <- paste(numsub_binrep, collapse = "")
      numsub_int <- strtoi(numsub_str, base = 2)

      subpackcount <- 0
      # lencount <- 0

      lencount <- 19

      while(subpackcount < numsub_int){
        restemp <- day16_recur(orig_decoded_vec[lencount:length(orig_decoded_vec)] , sum_vnums)

        sum_vnums <- restemp[[1]]
        lencount <- lencount+restemp[[2]]-1
        subpackcount <- subpackcount+1
        packvals <- c(packvals, restemp[[3]])

      }

    }


    if(length(packvals)!=0){

      if(typeID_int==0){
        outval <- sum(packvals)
      }
      if(typeID_int==1){
        outval <- prod(packvals)
      }
      if(typeID_int==2){
        outval <- min(packvals)
      }
      if(typeID_int==3){
        outval <- max(packvals)
      }
      if(typeID_int==5){
        outval <- 1*(packvals[1]>packvals[2])
      }
      if(typeID_int==6){
        outval <- 1*(packvals[1]<packvals[2])
      }
      if(typeID_int==7){
        outval <- 1*(packvals[1]==packvals[2])
      }



    }

    # version_vec
    #MUST update END LIT
    end_lit <- lencount


  }#end operator part


  if(is.na(outval)){
    print("typeID_int = ")
    print(typeID_int)
  }

  ret_list <- list()
  ret_list[[1]] <- sum_vnums
  ret_list[[2]] <- end_lit
  ret_list[[3]] <- outval

  # all(c("1","0", "0")  == c("1","0", "0"))
  return(ret_list)


}


day16func_a <- function(exampledata_vec){

  orig_decoded_vec <- rep(NA, length = 4*length(exampledata_vec))

  for(i in 1:length(exampledata_vec)){
    orig_decoded_vec[(i-1)*4 + 1:4 ] <- strsplit(decoder$V3[which(decoder$V1 == exampledata_vec[i])],
                                                 split=c(''), fixed=TRUE)[[1]]
  }

  res_all <- day16_recur(orig_decoded_vec, 0)

  return(res_all)

}


#
#  day16func_a(exampledata_vec )
#
#
#  day16func_a(example_op0_vec  )
#
#  day16func_a(example_op1_vec  )
#
#
#
#  day16func_a(exampledata1_vec  )
#
#  day16func_a(exampledata2_vec  )
#
#  day16func_a(exampledata3_vec  )
#
#  day16func_a(exampledata4_vec  )



#
#  exampledata1_vec <- strsplit("C200B40A82",split=c(''), fixed=TRUE)[[1]]
#  exampledata2_vec <- strsplit("04005AC33890",split=c(''), fixed=TRUE)[[1]]
#  exampledata3_vec <- strsplit("880086C3E88112",split=c(''), fixed=TRUE)[[1]]
#  exampledata4_vec <- strsplit("CE00C43D881120",split=c(''), fixed=TRUE)[[1]]
#  exampledata5_vec <- strsplit("D8005AC2A8F0",split=c(''), fixed=TRUE)[[1]]
#  exampledata6_vec <- strsplit("F600BC2D8F",split=c(''), fixed=TRUE)[[1]]
#  exampledata7_vec <- strsplit("9C005AC2F8F0",split=c(''), fixed=TRUE)[[1]]
#  exampledata8_vec <- strsplit("9C0141080250320F1802104A08",split=c(''), fixed=TRUE)[[1]]
#
#  day16func_a(exampledata1_vec  )
#  day16func_a(exampledata2_vec  )
#  day16func_a(exampledata3_vec  )
#  day16func_a(exampledata4_vec  )
#  day16func_a(exampledata5_vec  )
#  day16func_a(exampledata6_vec  )
#  day16func_a(exampledata7_vec  )
#  day16func_a(exampledata8_vec  )


#output contains answers to parts a and b

 # res <- day16func_a(data16a_vec  )


 # sprintf("%.1f",res[[3]])





#
# day16func_a(data16a_list)





