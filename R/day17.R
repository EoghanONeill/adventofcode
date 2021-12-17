
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")


# decoder <- read.csv("data/day17decoder.txt", header = FALSE,sep = " ",
#                     colClasses = "character")

data17a <- read.csv("data/day17data.txt", header = FALSE,sep = "=",
                   colClasses = "character")

exampledata <- read.csv("data/day17example.txt", header = FALSE, sep = "=",
                        colClasses = "character")



day17func_a <- function(exampledata){

  temp_x <- strsplit(exampledata$V2,split = c(','), fixed=TRUE)[[1]]
  xtarget <- as.numeric(strsplit(temp_x,split = c('..'), fixed=TRUE)[[1]])
  ytarget <- as.numeric(strsplit(exampledata$V3,split = c('..'), fixed=TRUE)[[1]])

  if(sign(xtarget[1])!=sign(xtarget[2])){
    stop("directly below")
  }

  xsign <- sign(xtarget[1])

  ymax_vec <- vector(length = 0)


  #try all possible x initial values
  for(i in  sign(xtarget[1]):xtarget[2]){

    #record which steps are in range
    steps_in_range_x <- vector(length =0)

    # i <- 6
    step <- 1
    xtemp <- 0
    vertical_in_target <- 0
    while(xsign*xtemp < xsign*(xtarget[2]+xsign)){
      xtemp <- step*i - xsign*(0.5)*(min(xsign*i,step)-1)*min(xsign*i,step)

      if((xtarget[1] <= xtemp ) & (xtemp <= xtarget[2])){
        steps_in_range_x <- c(steps_in_range_x, step)

      }

      if(step == xsign*i){
        if((xtarget[1] <= xtemp ) & (xtemp <= xtarget[2])){
          vertical_in_target <- 1
        }


        # print("step == xsign*i")
        # print("i ==")
        # print(i)
        # print("step ==")
        # print(step)
        # print("vertical_in_target ==")
        # print(vertical_in_target)

        break
      }

      step <- step+1
    }

    if(length(steps_in_range_x)==0){
      next
    }

    #now find y values for which the
    #steps steps_in_range_x
    #are also in the y range

    #first must obtain limit
    #for y initial values

    #assuming y limits are negative
    #first step below zero has y value -(y1+1)
    #where y1 is initial value
    #thereforew require -(y1+1) >= ytarget[1]

    # print("x1 = ")
    # print(i)
    #
    # print("steps_in_range_x = ")
    # print(steps_in_range_x)

    for(y1 in 1:(-1*(ytarget[1]+1) )){

      if((vertical_in_target==0)&all(steps_in_range_x  < 2*y1 + 1)){
        next
      }

      ytemp <- 0
      step_ind <- 1


      #require step_ind >= 2y1+1 (first step with negative y value)
      #and can also derive limit step but this is unnecessary
      if(vertical_in_target==1){

        step_y <- min(steps_in_range_x)
        while(ytemp >= ytarget[1]){
          ytemp <- step_y*y1-(0.5)*(step_y-1)*step_y

          if(ytemp < ytarget[1]){
            break
          }
          if((ytemp >= ytarget[1])&(ytemp <= ytarget[2])){
            #in y and x range
            #calculate maximum and add to list


            ymax_vec <- c(ymax_vec, (0.5)*y1*(y1+1))
            #then do not need to check any more steps
            break

          }
          step_y <- step_y+1
        }


      }else{
        for(step_y in steps_in_range_x[steps_in_range_x  >= 2*y1 + 1]){


          ytemp <- step_y*y1-(0.5)*(step_y-1)*step_y

          if(ytemp < ytarget[1]){
            break
          }
          if((ytemp >= ytarget[1])&(ytemp <= ytarget[2])){
            #in y and x range
            #calculate maximum and add to list


            ymax_vec <- c(ymax_vec, (0.5)*y1*(y1+1))
            #then do not need to check any more steps
            break

          }


        }
      }

    }# end of loop over y1 (initial y values)


  }#end of for loop over i (initial x values)

  return(max(ymax_vec))

}


#
# day17func_a(exampledata)
#
# day17func_a(data17a)







day17func_b <- function(exampledata){

  temp_x <- strsplit(exampledata$V2,split = c(','), fixed=TRUE)[[1]]
  xtarget <- as.numeric(strsplit(temp_x,split = c('..'), fixed=TRUE)[[1]])
  ytarget <- as.numeric(strsplit(exampledata$V3,split = c('..'), fixed=TRUE)[[1]])

  if(sign(xtarget[1])!=sign(xtarget[2])){
    stop("directly below")
  }

  xsign <- sign(xtarget[1])

  ymax_vec <- vector(length = 0)


  #try all possible x initial values
  for(i in  sign(xtarget[1]):xtarget[2]){

    #record which steps are in range
    steps_in_range_x <- vector(length =0)

    # i <- 6
    step <- 1
    xtemp <- 0
    vertical_in_target <- 0
    while(xsign*xtemp < xsign*(xtarget[2]+xsign)){
      xtemp <- step*i - xsign*(0.5)*(min(xsign*i,step)-1)*min(xsign*i,step)

      if((xtarget[1] <= xtemp ) & (xtemp <= xtarget[2])){
        steps_in_range_x <- c(steps_in_range_x, step)

      }

      if(step == xsign*i){
        if((xtarget[1] <= xtemp ) & (xtemp <= xtarget[2])){
          vertical_in_target <- 1
        }


        # print("step == xsign*i")
        # print("i ==")
        # print(i)
        # print("step ==")
        # print(step)
        # print("vertical_in_target ==")
        # print(vertical_in_target)

        break
      }

      step <- step+1
    }

    if(length(steps_in_range_x)==0){
      next
    }

    #now find y values for which the
    #steps steps_in_range_x
    #are also in the y range

    #first must obtain limit
    #for y initial values

    #assuming y limits are negative
    #first step below zero has y value -(y1+1)
    #where y1 is initial value
    #thereforew require -(y1+1) >= ytarget[1]

    # print("x1 = ")
    # print(i)
    #
    # print("steps_in_range_x = ")
    # print(steps_in_range_x)

    #positive y1 values
    for(y1 in ytarget[1]:(-1*(ytarget[1]+1) )){

      if((vertical_in_target==0)&all(steps_in_range_x  < 2*y1 + 1)){
        next
      }

      ytemp <- 0
      # step_ind <- 1


      #require step_ind >= 2y1+1 (first step with negative y value)
      #and can also derive limit step but this is unnecessary
      if(vertical_in_target==1){

        step_y <- min(steps_in_range_x)
        while(ytemp >= ytarget[1]){
          ytemp <- step_y*y1-(0.5)*(step_y-1)*step_y

          if(ytemp < ytarget[1]){
            break
          }
          if((ytemp >= ytarget[1])&(ytemp <= ytarget[2])){
            #in y and x range
            #calculate maximum and add to list


            ymax_vec <- c(ymax_vec, (0.5)*y1*(y1+1))
            #then do not need to check any more steps
            break

          }
          step_y <- step_y+1
        }


      }else{
        for(step_y in steps_in_range_x[steps_in_range_x  >= 2*y1 + 1]){


          ytemp <- step_y*y1-(0.5)*(step_y-1)*step_y

          if(ytemp < ytarget[1]){
            break
          }
          if((ytemp >= ytarget[1])&(ytemp <= ytarget[2])){
            #in y and x range
            #calculate maximum and add to list


            ymax_vec <- c(ymax_vec, (0.5)*y1*(y1+1))
            #then do not need to check any more steps
            break

          }


        }
      }

    }# end of loop over y1 (initial y values)


  }#end of for loop over i (initial x values)

  return(length(ymax_vec))

}



#
# day17func_b(exampledata)
#
# day17func_b(data17a)









