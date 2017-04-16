## this will use error.sim.many to plot the simulated error rates averaged
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisRepo/varyN1.R")
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisRepo/error.sim.many.R")
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisRepo/error.sim.one.R")
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisRepo/error.sim.R")
source("~/Documents/Vanderbilt/Masters_Thesis/ThesisRepo/changDesCompare.R")

error.matChang <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                            n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                            r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                            rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                            p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                            p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                            sim = 100,
                            des = "Chang",
                            alpha = 0.05, 
                            beta = 0.8
                            )
powerMatChang <- error.matChang[c(1,seq(3,nrow(error.matChang),2)),]
type1MatChang <- error.matChang[c(1,seq(2,nrow(error.matChang),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Chang <- c()
averagepowerChang <- c()

for(i in 1:ncol(error.matChang)){
  averagetype1Chang[i] <- mean(type1MatChang[2:nrow(type1MatChang),i])
  averagepowerChang[i] <- mean(powerMatChang[2:nrow(powerMatChang),i])
}

plot.dfChang <- data.frame(design = "Chang",n.diff = error.matChang[1,], averagetype1 = averagetype1Chang, averagepower = averagepowerChang)


#########################
## Adaptation
########################
error.matAlter <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                                 n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                                 r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                                 rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                                 p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                                 p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                                 sim = 100,
                                 des = "Alter",
                                 alpha = 0.05, 
                                 beta = 0.8
)
powerMatAlter <- error.matAlter[c(1,seq(3,nrow(error.matAlter),2)),]
type1MatAlter <- error.matAlter[c(1,seq(2,nrow(error.matAlter),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Alter <- c()
averagepowerAlter <- c()

for(i in 1:ncol(error.matAlter)){
  averagetype1Alter[i] <- mean(type1MatAlter[2:nrow(type1MatAlter),i])
  averagepowerAlter[i] <- mean(powerMatAlter[2:nrow(powerMatAlter),i])
}

plot.dfAlter <- data.frame(design = "Chang Adaptation",n.diff = error.matAlter[1,], averagetype1 = averagetype1Alter, averagepower = averagepowerAlter)

##################
## Likelihood
##################

error.matLik <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                               n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                               r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                               rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                               p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                               p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                               sim = 100,
                               des = "Likelihood",
                               alpha = 0.05, 
                               beta = 0.8
)
powerMatLik <- error.matLik[c(1,seq(3,nrow(error.matLik),2)),]
type1MatLik <- error.matLik[c(1,seq(2,nrow(error.matLik),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Lik <- c()
averagepowerLik <- c()

for(i in 1:ncol(error.matLik)){
  averagetype1Lik[i] <- mean(type1MatLik[2:nrow(type1MatLik),i])
  averagepowerLik[i] <- mean(powerMatLik[2:nrow(powerMatLik),i])
}

plot.dfLik <- data.frame(design ="Likelihood",n.diff = error.matLik[1,], averagetype1 = averagetype1Lik, averagepower = averagepowerLik)



###################
## plot
#################
df <- rbind(plot.dfAlter, plot.dfChang, plot.dfLik)
plot <- ggplot(data = df, aes(x=n.diff,y=averagepower, color = design)) + geom_point() + ylab("Average Power") + 
  xlab("Deviation from planned sample size") + scale_colour_manual(name="Design", values = c("Alter" = "skyblue2",
                                                                                                    "Chang"     = "dodgerblue3",
                                                                                                    "like"   = "dodgerblue4"))




  error.matChang <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                                   n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                                   r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                                   rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                                   p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                                   p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                                   sim = 10000,
                                   des = "Chang",
                                   alpha = 0.05, 
                                   beta = 0.8,
                                   ntaMeth = "n2"
  )
powerMatChang <- error.matChang[c(1,seq(3,nrow(error.matChang),2)),]
type1MatChang <- error.matChang[c(1,seq(2,nrow(error.matChang),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Chang <- c()
averagepowerChang <- c()

for(i in 1:ncol(error.matChang)){
  averagetype1Chang[i] <- mean(type1MatChang[2:nrow(type1MatChang),i])
  averagepowerChang[i] <- mean(powerMatChang[2:nrow(powerMatChang),i])
}

plot.dfChang <- data.frame(design = "Chang",n.diff = error.matChang[1,], averagetype1 = averagetype1Chang, averagepower = averagepowerChang)


#########################
## Adaptation
########################
error.matAlter <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                                 n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                                 r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                                 rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                                 p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                                 p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                                 sim = 10000,
                                 des = "Alter",
                                 alpha = 0.05, 
                                 beta = 0.8,
                                 ntaMethod = "n2"
)
powerMatAlter <- error.matAlter[c(1,seq(3,nrow(error.matAlter),2)),]
type1MatAlter <- error.matAlter[c(1,seq(2,nrow(error.matAlter),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Alter <- c()
averagepowerAlter <- c()

for(i in 1:ncol(error.matAlter)){
  averagetype1Alter[i] <- mean(type1MatAlter[2:nrow(type1MatAlter),i])
  averagepowerAlter[i] <- mean(powerMatAlter[2:nrow(powerMatAlter),i])
}

plot.dfAlter <- data.frame(design = "Chang Adaptation",n.diff = error.matAlter[1,], averagetype1 = averagetype1Alter, averagepower = averagepowerAlter)

##################
## Likelihood
##################

error.matLik <- error.sim.many(ntv = c(41,49,75,73,61,52,43,43,55,77,83,67,59,48,40,49,70,55,49,39),
                               n1v = c(15,21,35,31,25,17,20,18,19,26,28,18,19,13,22,21,42,31,23,22),
                               r1v = c(1 ,3, 17,16,17,12,16, 2, 3,12,15,12,14,10, 2,3, 19,20,16,15),
                               rtv = c(7, 11,40,43,45,41,36, 7,12,41,48,49,46,40, 7,11,38,41,39,33),
                               p0v = c(.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.65,.7,.75,.1,.15,.45,.5,.7,.75),
                               p1v = c(.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.65,.8,.85,.9,.25,.3,.6,.8,.85,.9),
                               sim = 10000,
                               des = "Likelihood",
                               alpha = 0.05, 
                               beta = 0.8,
                               ntaMethod = "n2"
)
powerMatLik <- error.matLik[c(1,seq(3,nrow(error.matLik),2)),]
type1MatLik <- error.matLik[c(1,seq(2,nrow(error.matLik),2)),]

#mean(type1Mat[2:nrow(mat),])

averagetype1Lik <- c()
averagepowerLik <- c()

for(i in 1:ncol(error.matLik)){
  averagetype1Lik[i] <- mean(type1MatLik[2:nrow(type1MatLik),i])
  averagepowerLik[i] <- mean(powerMatLik[2:nrow(powerMatLik),i])
}

plot.dfLik <- data.frame(design ="Likelihood",n.diff = error.matLik[1,], averagetype1 = averagetype1Lik, averagepower = averagepowerLik)


###################
## plot
#################
df <- rbind(plot.dfAlter, plot.dfChang, plot.dfLik)
plot <- ggplot(data = df, aes(x=n.diff,y=averagepower, color = design)) + geom_point() + ylab("Average Power") + 
  xlab("Deviation from planned sample size") + scale_colour_manual(name="Design", values = c("Chang Adaptation" = "skyblue2",
                                                                                             "Chang"     = "dodgerblue3",
                                                                                             "Likelihood"   = "dodgerblue4"))
plot

ggplot(data = df, aes(x=n.diff,y=averagetype1, color = design)) + geom_point() + ylab("Average Type I Error") + 
  xlab("Deviation from planned sample size") + scale_colour_manual(name="Design", values = c("Chang Adaptation" = "skyblue2",
                                                                                             "Chang"     = "dodgerblue3",
                                                                                             "Likelihood"   = "dodgerblue4"))

#   % \begin{figure}[H]
# % \caption{Monte Carlo Simulation of Average Power of 20 Simon-like Designs when Stage I Sample Size Deviates from Planned for Attained Designs ($n_t$ = $n_1^{\ast\ast} + n_2$)}
# % %\includegraphics{t1errn2.png}
# % \end{figure}
# % 
# % \begin{figure}[H]
# % \caption{Monte Carlo Simulation of Average Type I Error Rates of 20 Simon-like Designs when Stage I Sample Size Deviates from Planned for Attained Designs ($n_t$ = $n_1^{\ast\ast} + n_2$)}
# % %\includegraphics{powern2.png}
# % \end{figure}
