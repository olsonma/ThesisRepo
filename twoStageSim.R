## This file is a simulation of two stage simon design
## redefine the parameters below accordingly. 
## This simulation is used in the function `changDes`, but they are
## defined as "unplanned" parameters in `changDes`. i.e the parameters are
## probably renamed in the function. 
## this was used for trial and error purposes. 
sim <- 10000
###########################	
## type I error simulation
###########################
#varyN1(nt = 41, n1=15, r1 = 1, rt = 7, p0 = 0.1, p1 = .25, ntaMeth = "n2")
p1 = .4#.6#.9
p0 = .25#.4 #.75
n1 = 35#100#32 ## originally 22
r1 = 9#41 #26 
rt = 24#59#41
nt = n1 + 63-25#n1 + (41-17)#n1 + 39-22
#0.4 0.6 17 41  7 21  0.05   0.8 0.641 0.092 25.628    100   124     41     59  

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

t1err <- rejectNull/sim
t1err


###########################
## Power simulation
## under alternative
############################
## under null
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

pow <- rejectNull/sim
pow

