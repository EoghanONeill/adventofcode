
setwd("C:/Users/eonei/Dropbox/adventofcode/attempt2/adventofcode")



data19a <- read.csv("data/day19data.txt", header = FALSE,sep = "=",
                   colClasses = "character")

exampledata <- read.csv("data/day19example.txt", header = FALSE, sep = "=",
                        colClasses = "character")

# exampledata2_vec <- strsplit("[1,1]",
#                              split=c(''), fixed=TRUE)[[1]]
#
# nrow((exampledata))
#
# substr(exampledata$V1[1], 1,3)
#

#create A transformations
At_list <- list()

#identity, even number of negatives
At_list[[1]] <- diag(1,3,3)
for(i in 1:3){
  tempmat <- -1*diag(1,3,3)
  tempmat[i,i] <- 1
  At_list[[i+1]] <- tempmat
}

rotatemat <- cbind(c(0,0,1),
                   c(0,1,0),
                   c(1,0,0))

At_list[[5]] <- -1*rotatemat

for(i in 1:3){
  tempmat <- rotatemat
  tempmat[4-i,i] <- -1
  At_list[[i+5]] <- tempmat
}

tempmat0 <- cbind(c(0,0,1),
                 c(1,0,0),
                 c(0,1,0))

At_list[[9]] <- tempmat0
for(i in 1:3){
  tempmat <- -1*tempmat0
  tempmat[ ((i+1) %% 3)+1 , i] <- 1
  At_list[[i+9]] <- tempmat
}

At_list[[13]] <- -1*rotatemat%*%tempmat0
for(i in 1:3){
  tempmat <- rotatemat%*%tempmat0
  tempmat[ i , 3 - ((i-2) %% 3 ) ] <- -1
  At_list[[i+13]] <- tempmat
}

tempmat0 <- cbind(c(0,1,0),
                 c(0,0,1),
                 c(1,0,0))

At_list[[17]] <- tempmat0
for(i in 1:3){
  tempmat <- -1*tempmat0
  tempmat[(i %% 3 ) + 1 ,i] <- 1
  At_list[[i+17]] <- tempmat
}

At_list[[21]] <- -1*rotatemat%*%tempmat0
for(i in 1:3){
  tempmat <- rotatemat%*%tempmat0
  tempmat[ 3 - (i %% 3 ),i ] <- -1
  At_list[[i+21]] <- tempmat
}





day19func_a <- function(exampledata){

  scanner_count <- 1
  scanner_list <- list()

  scanner_mat <- matrix(NA, nrow = 0, ncol = 3)

  for(i in 1:nrow(exampledata)){

    if( ( substr(exampledata$V1[i], 1,3) == "---")){
      if(i!=1){
        scanner_list[[scanner_count]] <- scanner_mat

        scanner_mat <- matrix(NA, nrow = 0, ncol = 3)

        scanner_count <- scanner_count +1
      }
    }else{
      temp_row <- as.numeric(strsplit(exampledata$V1[i], split = ",")[[1]])

      scanner_mat <- rbind(scanner_mat, temp_row)
      # if(is.na(scanner_mat[1,1])){
      #   print("i = ")
      #   print(i)
      #   stop("NA in scanner mat")
      # }
      if(i ==nrow(exampledata)){
        scanner_list[[scanner_count]] <- scanner_mat
      }

    }
  }  #end i for loop


  # nrow(scanner_mat)


  #array for saving relative coordinates

  #vectors from scanner x (rows) to scanner y (columns)
  #from perspective of scanner x
  #depth 3 because 3 coordinates

  scanner_coords_arr <- array(NA, dim = c(length(scanner_list),length(scanner_list),3))

  #4d array for transformations
  #scanner x to y perspective
  #dims 3 and 4 give 3 by 3 trandformation matrix
  scanner_trans_arr <- array(NA,
                              dim = c(length(scanner_list),length(scanner_list),3,3))




  for(a in 1:(length(scanner_list)-1)){
    for(b in (a+1):length(scanner_list)){
      a_list <- scanner_list[[a]]
      b_list <- scanner_list[[b]]
      found_overlap_break <- 0

      overlap_count <- 0
      for(i in 1:nrow(a_list)){
        for(j in 1:nrow(b_list)){

          # print("overlap_count = ")
          # print(overlap_count)
          # print("i")
          # print(i)
          # print("j")
          # print(j)

          overlap_count <- 0

          #distances from beacon i
          temp_dists_i <- sweep(a_list, 2, a_list[i,])
          #distances from beacon j
          temp_dists_j <- sweep(b_list , 2, b_list[j,] )

          overlap_ind_mat <- matrix(NA, nrow = 0, ncol = 2)
          for(i0 in 1:nrow(a_list)){
            for(j0 in 1:nrow(b_list)){
              if(identical( sort(abs(temp_dists_i[i0,])) ,
                            sort(abs(temp_dists_j[j0,]))) ){
                overlap_count <- overlap_count + 1
                overlap_ind_mat <- rbind(overlap_ind_mat, c(i0,j0))

                #might have to check for equal vectors or below full rank matrices
                if(overlap_count >= 12){
                  #there are 12 overlapping beacons for scanners a and b

                  # stop("found overlap")
                  #now find relative positions of scanners a and b
                  #transformation to scanner b beacon coordinates that gives theor scanner a coordinates

                  # for position of scanner b relative to scanner a in
                  # scanner a's coordinate system

                  # add vector from scanner a to some beacon x in overlap ind mat
                  # and negative of vector from beacon x to scanner b
                  # but first need to re-express scanner b vector to beacon x
                  # in the coordinate system of scanner a


                  # Note: scanners facing different directions
                  # cannot just compare coordinates of beacons directly
                  # must compare relative positions of beacons
                  # i, j is one of the shared beacons, so might as well use this

                  # find the depth dimension that is unchanged or negative
                  # across all relative beacons positions for both scanners
                  relmat_i <- temp_dists_i[overlap_ind_mat[, 1],]
                  relmat_j <- temp_dists_j[overlap_ind_mat[, 2],]


                  trans_inds_vec <- vector(length = 0)
                  for(transform_ind in 1:24){

                    #check if transformation applied to b distances gives desired result
                    # print("i = ")
                    # print(i)
                    #
                    # print("At_list[[i]] = ")
                    # print(At_list[[i]])
                    #
                    # print("t(relmat_j) = ")
                    # print(t(relmat_j))


                    if(all(t(relmat_i) == At_list[[transform_ind]]%*%t(relmat_j))){
                      trans_inds_vec <- c(trans_inds_vec, transform_ind)
                    }
                  }

                  if(length(trans_inds_vec) >1){
                    print("trans_inds_vec = ")
                    print(trans_inds_vec)
                    stop("more than one matching transformation")
                  }
                  if(length(trans_inds_vec) ==0){
                    print("overlap_ind_mat")
                    print(overlap_ind_mat)
                    stop("No matching transformation")
                  }

                  transmat <-  At_list[[trans_inds_vec[1] ]]

                  #save transformation
                  scanner_trans_arr[b,a,,] <- transmat

                  #b orientation to a orientation

                  # re-express scanner b vector to beacon i,j
                  # in the coordinate system of scanner a

                  #pick an arbitrary overlapping beacon and obtain
                  #its original scanner a and scanner b coordinates
                  beacon_x_a <- a_list[overlap_ind_mat[1, 1],]
                  beacon_x_b <- b_list[overlap_ind_mat[1, 2],]

                  #transform the scanner b coordinates to the orientation of scanner a
                  beacon_x_b_trans <- (transmat%*%(beacon_x_b))

                  #now can obtain location of scanner b from perspective of scanner a
                  #in scanner a coordinates
                  b_coords_from_a <- beacon_x_a - beacon_x_b_trans

                  #save coordinates
                  scanner_coords_arr[a, b, ] <- b_coords_from_a

                  found_overlap_break <- 1

                  break #break from j0 loop
                }# end found overlap if statement
              }

              #breaking from j0 loop in case somehow not broken above
              if(found_overlap_break==1){
                break
              }
            } # end j0 loop
            if(found_overlap_break==1){
              break
            }
          } # end i0 loop
          if(found_overlap_break==1){
            break
          }
        }# j loop
        if(found_overlap_break==1){
          break
        }
      }# i loop

      print("a = ")
      print(a)
      print("b = ")
      print(b)
    }#b loop
  }#a loop

  #
  #
  #

  tempscan <- 1*(!is.na(scanner_coords_arr[,,1] ) )

  pathlist <- list()
  pathlist[[1]] <- c(1)

  for(start_ind in 2:nrow(tempscan)){
    # testvec4 <- rep(0, nrow(tempscan))
    # testvec4[start_ind] <- 1

    iter <- 1
    allpaths <- matrix(start_ind, nrow = 1, ncol = 1)
    # oldnodes <- c(start_ind)
    while(!(1 %in%  allpaths[,iter] )  ){
      # newnodes <- c( -which((t(tempscan) %*% testvec4) != 0 ),
      #    which((tempscan %*% testvec4  !=0 ) ))

      # newnodes <- vector(length = 0)
      # for(i in 1:length(oldnodes)){
      #   newnodes <- c(newnodes, -which( t(tempscan)[, oldnodes[i]] !=0 ),
      #                  which(tempscan[, oldnodes[i]] !=0 ))
      #
      # }
      # if(length(newnodes)==0){
      #   stop("no new nodes")
      # }
      newallpaths <- matrix(NA, nrow = 0,
                            ncol = ncol(allpaths)+1)
      rowcount <- 1
      for(i in 1:nrow(allpaths)){
        newnodes <- c( -1*which( t(tempscan)[, abs(allpaths[i, ncol(allpaths)])] !=0 ),
                       which(tempscan[, abs(allpaths[i, ncol(allpaths)])] !=0 ))


        tempmat1 <- matrix( rep( t(  allpaths[i, ] ) , length(newnodes) ) ,
                            ncol = ncol(allpaths) , byrow = TRUE )

        tempmat1 <- cbind(tempmat1,newnodes)


        newallpaths <- rbind(newallpaths,tempmat1 )
      }

      #
      # temp_numreps <- nrow(allpaths)
      # allpaths <- matrix( rep( t( allpaths ) , length(newnodes) ) ,
      #                     ncol = ncol(allpaths) , byrow = TRUE )
      #
      # allpaths <- cbind(allpaths,
      #                   kronecker(newnodes, rep(1,temp_numreps)))

      allpaths <- newallpaths
      iter <- iter+1
      # oldnodes <- newnodes

    }

    # print("start_ind = ")
    # print(start_ind)
    #
    # print("viablepaths = ")
    # print(viablepaths)
    viablepaths <- allpaths[which(allpaths[, ncol(allpaths)] ==1)  , , drop=FALSE]

    pathlist[[start_ind]] <- viablepaths[1,]

  }

  ###

  ##
  ##

  trans_to_1_list <- list()
  vec_from_1_list <- list()

  trans_to_1_list[[1]] <- diag(3)
  vec_from_1_list[[1]] <- c(0,0,0)

  for(i in 2:length(scanner_list)){
    trans_to1mat <- diag(3)#matrix(NA, nrow = 3, ncol = 3)
    to1_vec <- c(0,0,0)
    pathtemp <-  pathlist[[i]]
    prevind <- i
    # if(length(pathtemp)==2){
    #   trans_to_1_list[[i]] <- scanner_trans_arr[prevind, pathtemp[j]  , ,]
    #   vec_from_1_list[[i]] <- c(0,0,0)
    # }
    for(j in 2:(length(pathtemp))){

      if(pathtemp[j]<0){
        trans_to1mat <- solve(scanner_trans_arr[abs(pathtemp[j]) ,prevind , ,])%*%trans_to1mat

        #add transformation
        to1_vec <- solve(scanner_trans_arr[abs(pathtemp[j]) ,prevind , ,])%*%(to1_vec - scanner_coords_arr[ prevind, abs(pathtemp[j]),])

      }else{
        trans_to1mat <- scanner_trans_arr[prevind, pathtemp[j]  , ,]%*%trans_to1mat

        #add transformation
        to1_vec <- scanner_trans_arr[prevind, pathtemp[j]  , ,]%*%to1_vec + scanner_coords_arr[pathtemp[j], prevind,]

      }
      prevind <- abs(pathtemp[j])

    }
    trans_to_1_list[[i]] <- trans_to1mat
    vec_from_1_list[[i]] <- to1_vec
  }




  #
  # trans_to_1_list <- list()
  # vec_from_1_list <- list()
  #
  # trans_to_1_list[[1]] <- diag(3)
  # vec_from_1_list[[1]] <- c(0,0,0)
  #
  # #obtain transformations to first scanner
  # for(i in 2:length(scanner_list)){
  #
  #   print("i = ")
  #   print(i)
  #
  #   trans_to1mat <- diag(3)#matrix(NA, nrow = 3, ncol = 3)
  #   to1_vec <- c(0,0,0)
  #   check_trans <- scanner_coords_arr[,,1]
  #
  #   start_ind <- i
  #   end_ind <- 1
  #   prev_ind <- -1
  #   found_trans <- 0
  #   found_end <- 0
  #
  #   while(start_ind!=end_ind){
  #     # print("start_ind = ")
  #     # print(start_ind)
  #     found_trans <- 0
  #
  #     for(rowind in 1:nrow(check_trans)){
  #       if(rowind ==prev_ind){
  #         next
  #       }
  #       print("start_ind = ")
  #       print(start_ind)
  #       if(!(is.na(check_trans[rowind, start_ind]))){
  #         #
  #         if(rowind==end_ind){
  #           #at end
  #           print("line 314")
  #
  #           trans_to1mat <- scanner_trans_arr[start_ind, rowind  , ,]%*%trans_to1mat
  #
  #           #add transformation
  #           to1_vec <- scanner_trans_arr[start_ind, rowind  , ,]%*%to1_vec + scanner_coords_arr[rowind, start_ind,]
  #
  #           found_trans <- 1
  #           found_end <- 1
  #           break # from for loop
  #         }else{
  #           print("line 327")
  #           print("rowind = ")
  #           print(rowind)
  #           print("start_ind = ")
  #           print(start_ind)
  #           print("i = ")
  #           print(i)
  #           trans_to1mat <- scanner_trans_arr[start_ind, rowind  , ,]%*%trans_to1mat
  #
  #           #add transformation
  #           to1_vec <- scanner_trans_arr[start_ind, rowind  , ,]%*%to1_vec + scanner_coords_arr[rowind, start_ind,]
  #
  #           prev_ind <- start_ind
  #           start_ind <- rowind
  #           found_trans <- 1
  #           break # from for loop over rowind
  #         }
  #
  #       }
  #     # }#row ind loop
  #
  #       # print("line 335")
  #
  #     if(found_end == 1){
  #       print("found end")
  #
  #       break # from for loop loop
  #     }
  #     if(found_trans == 1){
  #       stop("should have broken from this loop")
  #       next # from while loop
  #     }
  #
  #       # print("line 343")
  #       # print("rowind = ")
  #       # print(rowind)
  #     #reached end of for loop without finding transformation, try inverse
  #     # for(colind in 1:ncol(check_trans)){
  #       if(!(is.na(check_trans[ start_ind, rowind]))){
  #         #
  #         if(rowind==end_ind){
  #           #at end
  #           print("line 363")
  #           print("rowind")
  #           trans_to1mat <- solve(scanner_trans_arr[rowind ,start_ind , ,])%*%trans_to1mat
  #
  #           #add transformation
  #           to1_vec <- solve(scanner_trans_arr[rowind ,start_ind , ,])%*%(to1_vec - scanner_coords_arr[ start_ind, rowind,])
  #
  #           found_trans <- 1
  #           found_end <- 1
  #
  #           break # from for loop
  #         }else{
  #           print("line 375")
  #           print("rowind")
  #           print(rowind)
  #           print("start_ind")
  #           print(start_ind)
  #           trans_to1mat <- solve(scanner_trans_arr[rowind ,start_ind , ,])%*%trans_to1mat
  #
  #           #add transformation
  #           to1_vec <- solve(scanner_trans_arr[rowind ,start_ind , ,])%*%(to1_vec - scanner_coords_arr[start_ind, rowind,])
  #
  #           prev_ind <- start_ind
  #           start_ind <- rowind
  #           found_trans <- 1
  #           break # from for loop over colind
  #         }
  #
  #       }
  #
  #     }#end loop over rowind
  #     if(found_end == 1){
  #       break # from while loop
  #     }
  #     if(found_trans == 1){
  #       # print("found transformation")
  #       next # from while loop
  #     }
  #
  #     stop("found no transformation")
  #
  #   }#end of while loop
  #
  #
  #   trans_to_1_list[[i]] <- trans_to1mat
  #   vec_from_1_list[[i]] <- to1_vec
  #
  # }# end of loop over i

  #now apply the transformations to each beacon to get coordinates in terms of 1
  # scanner_coords_arr[1,2,]

  full_beaconmat <- t(scanner_list[[1]])

  for(i in 2:length(scanner_list)){
    print("i = ")
    print(i)
    # tempmat <- vec_from_1_list[[i]] + trans_to_1_list[[i]] %*%  t(scanner_list[[i]])
    tempmat <-sweep(trans_to_1_list[[i]] %*%  t(scanner_list[[i]]), 1, vec_from_1_list[[i]], FUN = "+")

    full_beaconmat <- cbind(full_beaconmat, tempmat)
  }

  inds_dup <- duplicated(t(full_beaconmat))
  res <- sum(1- inds_dup)
  # res <- ncol(full_beaconmat[, !duplicated(t(full_beaconmat))])

  # beaconsunique <- full_beaconmat[, !duplicated(t(full_beaconmat))]

  dists <- rep(NA, length(vec_from_1_list)*length(vec_from_1_list))
  ind_dist <- 1
  for(i in 1:length(vec_from_1_list)){
    for(j in 1:length(vec_from_1_list)){

      dists[ind_dist] <- sum(abs(vec_from_1_list[[i]]- vec_from_1_list[[j]]))

      ind_dist <- ind_dist + 1
    }
  }

  reslist <- list()
  reslist[[1]] <- res
  reslist[[2]] <- max(dists)


  return(reslist)

}




###
###
#
# day19func_a(exampledata)
#
# day19func_a(data19a)

