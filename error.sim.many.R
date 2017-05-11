## this will do error.sim.one many times. 
## takes vectors of different designs

error.sim.many <- function(ntv = c(53,54), n1v = c(27,25), r1v = c(1,1), rtv = c(5,5), p0v = c(0.05,0.05), p1v = c(0.15, 0.15),
                           sim = NULL, ntaMeth = "nt", des = "Likelihood", alpha = 0.05, beta = 0.8){
  #ntv = c(53,54); n1v = c(27,25); r1v = c(1,1); rtv = c(5,5); p0v = c(0.05,0.05); p1v = c(0.15,.15); sim = 1000
  #alpha = 0.05
  #beta = 0.8
  #des = "Chang"

  mat <- error.sim.one(ntplan = ntv[1],
                       n1plan = n1v[1],
                       r1plan = r1v[1],
                       rtplan = rtv[1],
                       p0plan = p0v[1],
                       p1plan = p1v[1],
                       ntaMeth = ntaMeth,
                       sim    = sim,
                       des    = des,
                       alpha  = alpha,
                       beta   = beta)

  for(i in 2:length(ntv)){
   # print(i)
    
    mat <- rbind(mat, error.sim.one(ntplan = ntv[i],
                                       n1plan = n1v[i],
                                       r1plan = r1v[i],
                                       rtplan = rtv[i],
                                       p0plan = p0v[i],
                                       p1plan = p1v[i],
                                       ntaMeth = ntaMeth,
                                       sim    = sim,
                                       des    = des,
                                       alpha  = alpha,
                                       beta   = beta)[2:3,])
  }
  
  error.mat <- mat
  error.mat
  
}
