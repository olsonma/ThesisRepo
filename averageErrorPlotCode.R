## this will use error.sim.many to plot the simulated error rates averaged
error.mat <- error.sim.many()
powerMat <- error.mat[c(1,seq(3,nrow(error.mat),2)),]
type1Mat <- error.mat[c(1,seq(2,nrow(error.mat),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1 <- c()
averagepower <- c()

for(i in 1:ncol(error.mat)){
  averagetype1[i] <- mean(type1Mat[2:nrow(type1Mat),i])
  averagepower[i] <- mean(powerMat[2:nrow(powerMat),i])
}

plot.df <- data.frame(n.diff = error.mat[1,], averagetype1 = averagetype1, averagepower = averagepower)
