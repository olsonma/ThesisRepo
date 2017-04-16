## this will take one original design and use the error.sim function to simulate the errors
## for deviations. returns a matrix with n.diff separations and type 1 error and power for each 
error.sim.one <- function(ntplan = 53, n1plan = 27, r1plan = 1, rtplan = 5, 
                          p0plan = 0.05, p1plan = 0.15,
                          sim = NULL, des = "blah", alpha = 0.05, beta = 0.8){

    #ntplan = 53; n1plan = 27; r1plan = 1; rtplan = 5 
    #p0plan = 0.05; p1plan = 0.15
    #sim = 100; des = "Chang"; alpha = 0.05; beta = 0.8
    

    temp <- varyN1(r1 = r1plan, rt = rtplan, n1 = n1plan, nt = ntplan, 
                   p0 = p0plan, p1 = p1plan, beta = beta, alpha = alpha,
                   nta = ntplan, sim = FALSE, vary = 10, char = FALSE)
    
    n1 <- temp$n1star
    nt <- rep(ntplan,length(temp$n1star))
    if(des == "Chang"){r1 <- temp$r1star.Chang} 
    if(des == "Likelihood"){r1 <- temp$r1star.Likelihood}
    if(des == "Alter"){r1 <- temp$`r1star.Chang Alter`}
    if(des == "Chang"){rt <- temp$rtstar.Chang} 
    if(des == "Likelihood"){rt <- temp$rtstar.Likelihood}
    if(des == "Alter"){rt <- temp$`rtstar.Chang Alter`}
    p0 <- rep(p0plan, length(temp$n1star))
    p1 <- rep(p1plan, length(temp$n1star))

    sim.results <- error.sim(n1v = n1, ntv = nt, r1v = r1, rtv = rt, p0v = p0, p1v = p1, sim = sim)
    n.diff <- n1-rep(n1plan,length(n1))
    rbind(n.diff, sim.results)
  
}
