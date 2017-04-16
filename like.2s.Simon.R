like.2s.Simon = function(p0 = 0.4, p1 = 0.6, n1 = 17, nt = 41, 
                         r1 = 7, rt = 21, n1a = 17, ntaMeth = "nt", 
                         beta = 0.2, alpha = 0.05, sim = TRUE){
  nta <- nt
  if(ntaMeth == "n2"){
    nta <- n1a + (nt-n1)
  }
  ######################################
  ## Author: Molly Olson
  ## Date:   April 2017
  ## Based off of Jeffrey Blume's Likelihood 2 stage (like.2s)
  ##
  ## Name: like.2s.Simon
  ## Description: Give operating characteristics of 
  ## the Likelihood 2-stage design (Extension of Simon's Optimal)
  ## that works nicely with Chang
  #######################################
  
  #######################################	
  ## Function Inputs:
  ## p1       is response rate under H1
  ## p0       is response rate under H0
  ## n1      is sample size for Stage 1
  ## n	    is Total sample size (Stage 1+ Stage 2)
  ## r1	    is number of successes in simon's Stage 1
  ## r	    is number of successes in simon's Final stage
  ##
  ## LR and FDR computations assume flat prior
  #######################################	
  #p1=0.60;p0=0.40;n1a=16;nta=41;ka.i=(1/4);kb.i=4;ka=(1/4);kb=4;r1=7;r=21
  R <- function(x){
    round(x, 3)
  }

  # r1   = 1;   rt  = 7; beta = 0.05; alpha = 0.2
  #        n1  = 15;  nt = 41 
  #        n1a = 5
  #        nta = 41;
  #        p0  = 0.1; p1 = 0.25 
  #        
  
  ## returns odds ratio
  or=(p1*(1-p0))/((p0)*(1-p1))
  
  
  ############################
  ## under planned conditions
  ############################
  ka.iplan= (or^(r1))*((1-p1)/(1-p0))^(n1)
  kb.iplan=Inf
  kaplan= (or^(rt))*((1-p1)/(1-p0))^(nt)
  kbplan=Inf
  
  top.iplan=round((log(kb.iplan)-n1*log((1-p1)/(1-p0)))/log(or),10)
  bot.iplan=round((log(ka.iplan)-n1*log((1-p1)/(1-p0)))/log(or),10)
  
  ## if our r1 (bot.i) isn't an integer, change accordingly. The LR will then change, so change that too. 
  if(floor(bot.iplan) < bot.iplan & bot.iplan < ceiling(bot.iplan)){
    bot.iplan <- floor(bot.iplan)
    ka.iplan  <- (or^(bot.iplan))*((1-p1)/(1-p0))^(n1)
  }
  
  topplan=round((log(kbplan)-nt*log((1-p1)/(1-p0)))/log(or),10)
  botplan=round((log(kaplan)-nt*log((1-p1)/(1-p0)))/log(or),10)
  
  pstr.i0p=pbinom(floor(bot.iplan),size=n1,prob=p0)
  pmis.i0p=1-pbinom(floor(top.iplan),size=n1,prob=p0)
  pwek.i0p=(pbinom(floor(top.iplan),size=n1,prob=p0)-pbinom(floor(bot.iplan),size=n1,prob=p0))
  
  pstr.i1p=1-pbinom(floor(top.iplan),size=n1,prob=p1)
  pmis.i1p=pbinom(floor(bot.iplan),size=n1,prob=p1)
  pwek.i1p=(pbinom(floor(top.iplan),size=n1,prob=p1)-pbinom(floor(bot.iplan),size=n1,prob=p1))
  
  ## Expected sample size
  ess.0plan=n1+(pwek.i0p)*(nt-n1)
  ess.1plan=n1+(pwek.i1p)*(nt-n1) 
  
  ###########################
  ## under attained conditions
  ###########################
  
  ka.i= (or^(r1))*((1-p1)/(1-p0))^(n1) #1/3.375 
  kb.i=Inf
  ka= (or^(rt))*((1-p1)/(1-p0))^(nt)
  kb=Inf
  
  
  #######################################	
  ## LR bounds translated to successes
  #######################################	
  
  top.i=round((log(kb.i)-n1a*log((1-p1)/(1-p0)))/log(or),10)
  if(top.i < 0){top.i = 0 }
  bot.i=round((log(ka.i)-n1a*log((1-p1)/(1-p0)))/log(or),10)
  if(bot.i < 0){bot.i = 0}
  
  ## if our r1 (bot.i) isn't an integer, change accordingly. The LR will then change, so change that too. 
  if(floor(bot.i) < bot.i & bot.i < ceiling(bot.i)){
    bot.i <- floor(bot.i)
    ka.i  <- (or^(bot.i))*((1-p1)/(1-p0))^(n1a)
  }
  
  top=round((log(kb)-nta*log((1-p1)/(1-p0)))/log(or),10)
  bot=round((log(ka)-nta*log((1-p1)/(1-p0)))/log(or),10)
  #print(c(top.i,bot.i,top,bot))
  #######################################	
  ## Interim Stage 
  #######################################
  
  pstr.i0=pbinom(floor(bot.i),size=n1a,prob=p0)
  pmis.i0=1-pbinom(floor(top.i),size=n1a,prob=p0)
  pwek.i0=(pbinom(floor(top.i),size=n1a,prob=p0)-pbinom(floor(bot.i),size=n1a,prob=p0))
  
  pstr.i1=1-pbinom(floor(top.i),size=n1a,prob=p1)
  pmis.i1=pbinom(floor(bot.i),size=n1a,prob=p1)
  pwek.i1=(pbinom(floor(top.i),size=n1a,prob=p1)-pbinom(floor(bot.i),size=n1a,prob=p1))
  
  ## Expected sample size
  ess.0=n1a+(pwek.i0)*(nta-n1a)
  ess.1=n1a+(pwek.i1)*(nta-n1a)
  
  #######################################	
  ## Final Stage 
  #######################################	
  
  ## Identify data (successes) that allow continuation (=weak evi at interim stage)
  x=floor((bot.i+1)):min(n1a,top.i)  
  
  pwek.0=sum(dbinom(x,size=n1a,prob=p0)*(pbinom((top-x),size=(nta-n1a),prob=p0)-pbinom((bot-x),size=(nta-n1a),prob=p0)))
  pet0 <- 1-pwek.0
  pstr.0=pstr.i0+sum(dbinom(x,size=n1a,prob=p0)*(pbinom((bot-x),size=(nta-n1a),prob=p0)))
  pmis.0=pmis.i0+sum(dbinom(x,size=n1a,prob=p0)*(1-pbinom((top-x),size=(nta-n1a),prob=p0)))
  
  pwek.1=sum(dbinom(x,size=n1a,prob=p1)*(pbinom((top-x),size=(nta-n1a),prob=p1)-pbinom((bot-x),size=(nta-n1a),prob=p1)))
  pet1 <- 1-pwek.1
  pstr.1=pstr.i1+sum(dbinom(x,size=n1a,prob=p1)*(1-pbinom((top-x),size=(nta-n1a),prob=p1)))
  pmis.1=pmis.i1+sum(dbinom(x,size=n1a,prob=p1)*(pbinom((bot-x),size=(nta-n1a),prob=p1)))
  
  ## Notice pwek.i0=sum(dbinom(x,size=n1a,prob=p0)) for ease of computation
  
  ##################
  ## Combine results for return (if needed)
  ##################
  
  inter=rbind(c(1,0,pstr.i0,pmis.i0,pwek.i0,pstr.i0+pmis.i0+pwek.i0,n1a),c(1,1,pstr.i1,pmis.i1,pwek.i1,pstr.i1+pmis.i1+pwek.i1,n1a))
  final=rbind(c(2,0,pstr.0,pmis.0,pwek.0,pstr.0+pmis.0+pwek.0,ess.0),c(2,1,pstr.1,pmis.1,pwek.1,pstr.1+pmis.1+pwek.1,ess.1))
  results=rbind(inter,final)
  dimnames(results)[[2]]=c("Stage","Hyp","Str","Mis","Weak","All","ESS")
  
  ##################
  ## LR and FDR computations
  ##################
  
  lri.1=results[2,'Str']/results[1,'Mis']
  fdri.1=(1+lri.1)^-1
  lri.0=results[1,'Str']/results[2,'Mis']
  fndi.0=(1+lri.0)^-1
  
  lrf.1=results[4,'Str']/results[3,'Mis']
  fdrf.1=(1+lrf.1)^-1
  lrf.0=results[3,'Str']/results[4,'Mis']
  fndf.0=(1+lrf.0)^-1
  
  lr=c(lri.0,lri.1,lrf.0,lrf.1)
  fdr=c(fndi.0,fdri.1,fndf.0,fdrf.1)
  
  x=floor((bot.i+1):min(n1a,bot))
  ptrm.0=pbinom(floor(bot.i),size=n1a,prob=p0)+sum(dbinom(x,size=n1a,prob=p0)*pbinom((bot-x),size=(nta-n1a),prob=p0))
  ptrm.1=pbinom(floor(bot.i),size=n1a,prob=p1)+sum(dbinom(x,size=n1a,prob=p1)*pbinom((bot-x),size=(nta-n1a),prob=p1))
  
  ## Type I error
  t1err <- 1-ptrm.0
  
  ## Power
  pow <- 1-ptrm.1
  
  
  
  results=cbind(results,unname(lr),unname(fdr))
  dimnames(results)[[2]]=c("Stage","Hyp","Str","Mis","Weak","All","ESS","LR","FDR")
  
  ##################
  ## Output
  ##################


  if(sim == FALSE){
    resultsdf <- data.frame(p0= p0,  p1=p1, n1 = n1, n = nt, r1 = r1, rt = rt, alpha = alpha, power = 1-beta,  
                            pet0 = R(pstr.i0p), pet1 = R(pmis.i1p), EN0 = R(ess.0plan), n1star = n1a, nstar = nta, r1star = bot.i, rtstar =bot, 
                            type1Obs = R(t1err), powerObs = R(pow), pet0star = R(pstr.i0), pet1star = R(pmis.i1), 
                            EN0star = R(ess.0), EN1star = R(ess.1))
    
  }
  if(sim == TRUE){
    resultsdf <- data.frame(p0= p0,  p1=p1, n1 = n1, n = nt, r1 = r1, rt = rt, alpha = alpha, power = 1-beta,  
                            pet0 = R(pstr.i0p), pet1 = R(pmis.i1p), EN0 = R(ess.0plan), n1star = n1a, nstar = nta, r1star = bot.i, rtstar =bot, 
                            type1Obs = R(t1err), powerObs = R(pow), pet0star = R(pstr.i0), pet1star = R(pmis.i1), 
                            EN0star = R(ess.0), EN1star = R(ess.1),type1Sim = NA, powerSim = NA)
  }
  resultsdf
  
}


####
###
##
#











####
###
##
#