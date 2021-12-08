
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")

data8a <- read.csv("data/day8data_a.csv", header = FALSE,sep = "|",
                   colClasses = "character"
                   )



exampledata <- read.csv("data/day8example.csv", header = FALSE, sep = "|",
                        colClasses = "character"
                   )


data8a_10dig_list <- strsplit(data8a$V1,split=c(' '), fixed=TRUE)[[1]]
data8a_4dig_list <- strsplit(data8a$V2,split=c(' '), fixed=TRUE)[[1]]

exampledata_10dig_list <- strsplit(exampledata$V1,split=c(' '), fixed=TRUE)[[1]]
exampledata_4dig_list <- strsplit(exampledata$V2,split=c(' '), fixed=TRUE)[[1]]

# median(exampledata_table)

day8func_a <- function(data8a_10dig_list, data8a_4dig_list){

  count <- 0

  for (j in 1: length(data8a_4dig_list)){
    temp_list_elem <- data8a_4dig_list[[j]]
      for(i in 1:4){
        tempval <- temp_list_elem[i+1]

        if(nchar(tempval) %in% c(2,3,4,7)){
          count <- count + 1
        }

      }
  }
  return(count)

}

# day8func_a(exampledata_10dig_list, exampledata_4dig_list)

# day8func_a(data8a_10dig_list, data8a_4dig_list)



day8func_b <- function(data8a_10dig_list, data8a_4dig_list){

  count <- 0

# print("line 51")

  for (j in 1: length(data8a_4dig_list)){
    temp_list_10dig <- data8a_10dig_list[[j]]

    temp_list_4dig <- data8a_4dig_list[[j]]

    out4dig <- rep(NA,4)

    temp6dig_0 <- rep(NA,6)
    temp2dig_1 <- rep(NA,2)
    temp5dig_2 <- rep(NA,5)
    temp5dig_3 <- rep(NA,5)
    temp4dig_4 <- rep(NA,4)
    temp5dig_5 <- rep(NA,5)
    temp6dig_6 <- rep(NA,6)
    temp3dig_7 <- rep(NA,3)

    temp6dig_9 <- rep(NA,6)

    a_char <- NA
    b_char <- NA
    c_char <- NA
    d_char <- NA
    e_char <- NA
    f_char <- NA
    g_char <- NA

    for(i in 1:length(temp_list_10dig)){
      tempval <- temp_list_10dig[i]
      if(nchar(tempval)==3){
        temp3dig_7 <- strsplit(tempval, split = c(''))[[1]]
      }
      if(nchar(tempval)==2){
        temp2dig_1 <- strsplit(tempval, split = c(''))[[1]]
      }
      if(nchar(tempval)==4){
        temp4dig_4 <- strsplit(tempval, split = c(''))[[1]]
      }
    }

    for (k in 1:3){
      if(!(temp3dig_7[k] %in% temp2dig_1)){
        a_char <- temp3dig_7[k]
      }
    }

    for(i in 1:length(temp_list_10dig)){
      tempval <- temp_list_10dig[i]
      tempstring <- strsplit(tempval, split = c(''))[[1]]
      if((nchar(tempval)==5)  ){
        if(( length(intersect(temp4dig_4, tempstring))==2 )){
          temp6dig_2 <- tempstring
          # f_char <- intersect(setdiff(tempstring, a_char), temp3dig_7 )
          f_char <- setdiff(temp3dig_7,intersect(temp3dig_7, tempstring) )
        }
        if(( length(intersect(temp2dig_1, tempstring))==2 )){
          temp5dig_3 <- tempstring
        }



      }

      if((nchar(tempval)==6)  ){
        if(( length(intersect(temp2dig_1, tempstring))==1 )){
          temp6dig_6 <- tempstring
        }else{
          if(( length(intersect(temp4dig_4, tempstring))==4 )){
            temp6dig_9 <- tempstring
            e_char <- setdiff(c('a','b','c','d','e','f','g'), tempstring)
          }else{
            temp6dig_0 <- tempstring
            d_char <- setdiff(c('a','b','c','d','e','f','g'), tempstring)
          }
        }
      }

    }#end loop over j

    # print("line 130")

    for(k in 1:2){
      if(temp2dig_1[k] != f_char){
        c_char <- temp2dig_1[k]
      }
    }

    # print("line 130")

    b_char <- setdiff(temp4dig_4, c(c_char, d_char, f_char))

    g_char <- setdiff(c('a','b','c','d','e','f','g'),
                      c(a_char, b_char, c_char, d_char, e_char, f_char))

    # print("line 144")


    for(i in 1:4){
      tempval <- temp_list_4dig[i+1]
      tempstring <- strsplit(tempval, split = c(''))[[1]]

      # print("i ==")
      # print(i)
      # print("j ==")
      # print(j)

      if(nchar(tempval) ==2 ){
        out4dig[i] <- 1
      }
      if(nchar(tempval) ==3 ){
        out4dig[i] <- 7
      }
      if(nchar(tempval) ==4 ){
        out4dig[i] <- 4
      }
      if(nchar(tempval) ==7 ){
        out4dig[i] <- 8
      }
      # print("line 167")
      # print("d_char ==")
      # print(d_char)
      # print("c_char ==")
      # print(c_char)
      # print("e_char ==")
      # print(e_char)
      #
      # print("tempstring ==")
      # print(tempstring)

      if(nchar(tempval) ==6 ){
        if(setdiff(c('a','b','c','d','e','f','g'),tempstring ) == d_char){
        out4dig[i] <- 0
        }
        if(setdiff(c('a','b','c','d','e','f','g'),tempstring ) == c_char){
          out4dig[i] <- 6
        }
        if(setdiff(c('a','b','c','d','e','f','g'),tempstring ) == e_char){
          out4dig[i] <- 9
        }
      }

      if(nchar(tempval) ==5 ){
        if(b_char %in% tempstring){
          out4dig[i] <- 5
        }else{
          if(e_char %in% tempstring){
            out4dig[i] <- 2
          }else{
            out4dig[i] <- 3
          }
        }

      }

    }

    # print("line 191")
    # print("j = ")
    # print(j)
    # print("output = ")
    # print(1000*out4dig[1] + 100*out4dig[2] + 10*out4dig[3] + 1*out4dig[4])


    count <- count + 1000*out4dig[1] + 100*out4dig[2] + 10*out4dig[3] + 1*out4dig[4]

  }
  return(count)

}


# day8func_b(exampledata_10dig_list, exampledata_4dig_list)

# day8func_b(data8a_10dig_list, data8a_4dig_list)


