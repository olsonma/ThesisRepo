## varyN1 will use changDesCompare and vary the stage 1 sample size and produce a table
library(Hmisc)
varyN1 <- function(r1   = 7,   rt  = 21, beta = 0.2, alpha = 0.05,
                   n1  = 17,  nt = 41, 
                   nta = 41,
                   p0  = 0.4, p1 = 0.6, 
                   sim = FALSE, vary = 10){
  
  table <- NULL
  for(i in seq(n1-vary, n1+vary, 2)){  
    table <- rbind(table, changDesCompare(r1   = r1,  rt  = rt, beta = beta, alpha = alpha,
                                                    n1  = n1,  nt = nt, 
                                                    n1a = i, nta = nta,
                                                    p0  = p0, p1 = p1, 
                                                    sim = sim)) 
  }
  table[Cs(Design, n1star, r1star, type1Obs, powerObs, pet0star, pet1star, EN0star)]
}