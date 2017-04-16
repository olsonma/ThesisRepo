## varyN1 will use changDesCompare and vary the stage 1 sample size and produce a table
## the table will be wide
library(Hmisc)
varyN1 <- function(r1   = 7,   rt  = 21, beta = 0.2, alpha = 0.05,
                   n1  = 17,  nt = 41, 
                   nta = 41,
                   p0  = 0.4, p1 = 0.6, 
                   sim = FALSE, vary = 10, char = TRUE){
  #print(paste0("vary start nta: ", nt))
  table <- NULL
  for(i in seq(n1-vary, n1+vary, 2)){  
    #print(i)
    table <- rbind(table, changDesCompare(r1   = r1,  rt  = rt, beta = beta, alpha = alpha,
                                                    n1  = n1,  nt = nt, 
                                                    n1a = i, nta = nt,
                                                    p0  = p0, p1 = p1, 
                                                    sim = sim)) 
  }
  table <- table[Cs(p0, p1, n1, n, r1, rt, EN0, Design, n1star, r1star, rtstar, type1Obs, powerObs, pet0star, EN0star)]
      if(char == TRUE){
      table$p0 <- as.character(table$p0)
      table$p1 <- as.character(table$p1)
      table$n1 <- as.character(table$n1)
      table$n  <- as.character(table$n)
      table$r1 <- as.character(table$r1)
      table$rt <- as.character(table$rt)
      table$n1star <- as.character(table$n1star)
      table$r1star <- as.character(table$r1star)
      table$rtstar <- as.character(table$rtstar)
      }
      
  table$des.num <- rep(seq(1,nrow(table)/3,1), each = 3)
  table.wide <- reshape(table, 
               timevar = "Design",
               idvar = c("des.num","p0", "p1", "n1", "n", "r1", "rt","EN0","n1star"),
               direction = "wide")
  #table.wide[c(1:6,9:10,15:16,21:22)] <- as.character(table.wide[c(1:6,9:10,15:16,21:22)])
  #print(paste0("vary end nta: ", nt))
  
  table.wide[c(1:8,10:27)]
  
}
