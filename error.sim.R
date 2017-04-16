### this will simulate error terms for a given design and it's deviations. 
## takes vectors and returns a matrix with type1 error and power for each design

error.sim <- function(ntv = c(NA), n1v = c(NA), r1v = c(NA), rtv = c(NA), p0v = c(NA), p1v = c(NA),
                      sim = NULL){

    #ntv = c(53,54); n1v = c(27,25); r1v = c(1,1); rtv = c(5,5); p0v = c(0.05,0.05); p1v = c(0.15,.15); sim = 1000
    type1 <- NULL
    
    pow   <- NULL
    
    
  ###########################	
  ## type I error simulation
  ###########################
  
for(design in 1:length(ntv)){
#print(design)
  r1 <- r1v[design]
  rt <- rtv[design]
  n1 <- n1v[design]
  nt <- ntv[design]
  p0 <- p0v[design]
  p1 <- p1v[design]
  
  #print(c(r1,rt,n1,nt,p0,p1))
  
  ## under null
  results1 <- c()
  results2 <- c()
  rejectNull <- 0
  totalResponse <- NULL
  totalStage1   <- NULL
  totalStage2   <- NULL
  
  for(j in 1:sim){
    
    ## set up first stage
    for(i in 1:n1){
      ## get a number of responses
      results1[i] <- rbinom(1, 1, p0)
    }
    
    totalStage1 <- sum(results1)
    #print(totalStage1)
    if(totalStage1 <= r1){
      rejectNull <- rejectNull + 0
    }
    
    if(totalStage1 > r1){ ## go to second stage
      
      ## enroll n2 patients more
      for(k in 1:(nt-n1)){
        results2[k] <- rbinom(1, 1, p0)
      }
      totalStage2 <- sum(results2)
      totalResponse <- totalStage1 + totalStage2
      rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
    }
  }
  type1[design] <- rejectNull/sim

  
  ###########################
  ## Power simulation
  ## under alternative
  ############################
  results1      <- c()
  results2      <- c()
  rejectNull    <- 0
  totalResponse <- NULL
  totalStage1   <- NULL
  totalStage2   <- NULL
  
  for(j in 1:sim){
    
    ## set up first stage
    for(i in 1:n1){
      ## get a number of responses
      results1[i] <- rbinom(1, 1, p1)
    }
    
    totalStage1 <- sum(results1)
    #print(totalStage1)
    if(totalStage1 <= r1){
      rejectNull <- rejectNull + 0
    }
    
    if(totalStage1 > r1){ ## go to second stage
      
      ## enroll n2 patients more
      for(k in 1:(nt-n1)){
        results2[k] <- rbinom(1, 1, p1)
      }
      totalStage2 <- sum(results2)
      totalResponse <- totalStage1 + totalStage2
      rejectNull <- ifelse(totalResponse > rt, rejectNull + 1, rejectNull + 0)
    }
  }
  
  pow[design] <- rejectNull/sim
  #pow
  }
    return(rbind(type1, pow))
  

}