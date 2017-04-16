dfCompare <- NULL

dfCompare <- changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
               n1  = 17,  nt = 41, 
               n1a = 17, ntaMeth = "nt",
               p0  = 0.4, p1 = 0.6)

dfCompare <- rbind(dfCompare, changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 19, ntaMeth = "nt", ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfCompare <- rbind(dfCompare, changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 19, ntaMeth = "nt", ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfCompare <- rbind(dfCompare, changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 21, ntaMeth = "nt", ## keep original sample size the same
                         p0  = 0.4, p1 = 0.6))	

dfCompare <- rbind(dfCompare, changDesCompare(a   = 7,   c  = 21, beta = 0.2, alpha = 0.05,
                         n1  = 17,  nt = 41, 
                         n1a = 21, ntaMeth = "nt", ## keep original stage 2 sample size the same
                         p0  = 0.4, p1 = 0.6))

                         
								   