## jblume

## Likelihood ratio criterion
## ka<=LR<=kb

#### Binomial calculations
## X~Bin(n,p) ; X is number of success in n trials
## then P(a <= X <= B)=
## =sum(dbinom(bot:top,size=n,prob=p0)) 
## =pbinom(top,size=n,prob=p0)-pbinom((bot-1),size=n,prob=p0)


kb=4
ka=1/4
p1=0.4
p0=0.2
or=(p1*(1-p0))/((p0)*(1-p1))
n=30

## Likelihood ratio calculations
## LR=(or^(x))*((1-p1)/(1-p0))^(n)
## LR=dbinom(x,size=n,p1)/dbinom(x,size=n,p0)

## LR criteria in data space (# of successes)
top=(log(kb)-n*log((1-p1)/(1-p0)))/log(or)
bot=(log(ka)-n*log((1-p1)/(1-p0)))/log(or)

## Probability of observing strong evidence
## P(LR>=kb|p1) and P(LR<=ka|p0)
pstr.1=1-pbinom(top,size=n,prob=p1)
pstr.0=pbinom(bot,size=n,prob=p0)

## Probability of weak evidence
## P(ka < LR < kb|p1) and P(ka < LR < kb|p0)
pwek.1=pbinom(top,size=n,prob=p1)-pbinom(bot,size=n,prob=p1)
pwek.0=pbinom(top,size=n,prob=p0)-pbinom(bot,size=n,prob=p0)

## Probability of misleading evidence
## P(LR<=ka|p1) and P(LR>=kb|p0)
pmis.1=pbinom(bot,size=n,prob=p1)
pmis.0=1-pbinom(top,size=n,prob=p0)

## Double check: (pstr.1+pmis.1+pwek.1)==1&(pstr.0+pmis.0+pwek.0)==1
## Pass

n.i=17
n=37

bot.i=3
bot=10

etrm.0=pbinom(floor(bot.i),size=n.i,prob=p0)
etrm.1=pbinom(floor(bot.i),size=n.i,prob=p1)

etrm.0
etrm.1

x=floor((bot.i+1):min(n.i,bot))
ptrm.0=pbinom(floor(bot.i),size=n.i,prob=p0)+sum(dbinom(x,size=n.i,prob=p0)*pbinom((bot-x),size=(n-n.i),prob=p0))
ptrm.1=pbinom(floor(bot.i),size=n.i,prob=p1)+sum(dbinom(x,size=n.i,prob=p1)*pbinom((bot-x),size=(n-n.i),prob=p1))

## Type I error
1-ptrm.0

## Power
1-ptrm.1

## Validated vs. http://www.cscc.unc.edu/cscc/aivanova/SimonsTwoStageDesign.aspx
###########################################


p1=0.30
p0=0.10
or=(p1*(1-p0))/((p0)*(1-p1))

n.i=4
n=20

ka.i=(or^(0))*((1-p1)/(1-p0))^(4)
kb.i=(or^(6))*((1-p1)/(1-p0))^(17)
ka=(or^(3))*((1-p1)/(1-p0))^(20)
kb=(or^(13))*((1-p1)/(1-p0))^(37)

#ka.i=1/4
kb.i=Inf
#ka=4
kb=Inf

top.i=round((log(kb.i)-n.i*log((1-p1)/(1-p0)))/log(or),10)
bot.i=round((log(ka.i)-n.i*log((1-p1)/(1-p0)))/log(or),10)

top=round((log(kb)-n*log((1-p1)/(1-p0)))/log(or),10)
bot=round((log(ka)-n*log((1-p1)/(1-p0)))/log(or),10)

## c(bot.i,top.i,bot,top)

## Interim look characteristics
pstr.i0=pbinom(floor(bot.i),size=n.i,prob=p0)
pmis.i0=1-pbinom(floor(top.i),size=n.i,prob=p0)
pwek.i0=(pbinom(floor(top.i),size=n.i,prob=p0)-pbinom(floor(bot.i),size=n.i,prob=p0))

pstr.i1=1-pbinom(floor(top.i),size=n.i,prob=p1)
pmis.i1=pbinom(floor(bot.i),size=n.i,prob=p1)
pwek.i1=(pbinom(floor(top.i),size=n.i,prob=p1)-pbinom(floor(bot.i),size=n.i,prob=p1))

x=floor((bot.i+1)):min(n.i,top.i)  ## check limits include top and bot?
## Note pwek.i0=sum(dbinom(x,size=n.i,prob=p0)) and pwek.i1=sum(dbinom(x,size=n.i,prob=p1))

pwek.0=sum(dbinom(x,size=n.i,prob=p0)*(pbinom((top-x),size=(n-n.i),prob=p0)-pbinom((bot-x),size=(n-n.i),prob=p0)))
pstr.0=pstr.i0+sum(dbinom(x,size=n.i,prob=p0)*(pbinom((bot-x),size=(n-n.i),prob=p0)))
pmis.0=pmis.i0+sum(dbinom(x,size=n.i,prob=p0)*(1-pbinom((top-x),size=(n-n.i),prob=p0)))

pwek.1=sum(dbinom(x,size=n.i,prob=p1)*(pbinom((top-x),size=(n-n.i),prob=p1)-pbinom((bot-x),size=(n-n.i),prob=p1)))
pstr.1=pstr.i1+sum(dbinom(x,size=n.i,prob=p1)*(1-pbinom((top-x),size=(n-n.i),prob=p1)))
pmis.1=pmis.i1+sum(dbinom(x,size=n.i,prob=p1)*(pbinom((bot-x),size=(n-n.i),prob=p1)))

inter=rbind(c(0,pstr.i0,pmis.i0,pwek.i0,pstr.i0+pmis.i0+pwek.i0),c(1,pstr.i1,pmis.i1,pwek.i1,pstr.i1+pmis.i1+pwek.i1))
final=rbind(c(0,pstr.0,pmis.0,pwek.0,pstr.0+pmis.0+pwek.0),c(1,pstr.1,pmis.1,pwek.1,pstr.1+pmis.1+pwek.1))

dimnames(inter)[[2]]=c("Hyp","Str","Mis","Weak","All")
dimnames(final)[[2]]=c("Hyp","Str","Mis","Weak","All")
round(inter,4)
round(final,4)

##################
## Output
##################

cat("#########################################################################\n")
cat("##  Likelihood Two-Stage Design \n")
cat("##  Hypotheses : H1: p =",round(p1,2),"; H0: p =",round(p0,2),"; OR =",round(or,2),"\n")
cat("##  Interim	: Sample Size = ",format(round(n.i,1),width=3),", Continue if (1/", round(1/ka.i,2),") < LR < ",round(kb.i,2),"\n",sep="")
cat("##  Final		: Sample Size = ",format(round(n,1),width=3),  ", Weak ev. if (1/", round(1/ka,2),") < LR < ",round(kb,2),"\n",sep="")
cat("## ---------------------------------------------------------------------\n")
cat("##  Intermin Characteristics \n")
cat("##  Continue 	:",format(round(pwek.i0,4),width=6), "|H0;",format(round(pwek.i1,4),width=6),"|H1\n")
cat("##  Strong Ev.	:",format(round(pstr.i0,4),width=6), "|H0;",format(round(pstr.i1,4),width=6),"|H1\n")
cat("##  Mislead Ev.:",format(round(pmis.i0,4),width=6), "|H0;",format(round(pmis.i1,4),width=6),"|H1\n")
cat("##  Final Characteristics \n")
cat("##  Weak Ev. 	:",format(round(pwek.0,4),width=6), "|H0;",format(round(pwek.1,4),width=6),"|H1\n")
cat("##  Strong Ev.	:",format(round(pstr.0,4),width=6), "|H0;",format(round(pstr.1,4),width=6),"|H1\n")
cat("##  Mislead Ev.:",format(round(pmis.0,4),width=6), "|H0;",format(round(pmis.1,4),width=6),"|H1\n")
cat("#########################################################################\n")


## logic check
pwek.1+pstr.1+pmis.1

pstr.i1+pmis.i1+pwek.i1
pwek.i1
sum(dbinom(x,size=n.i,prob=p1))

(pbinom((top-x),size=(n-n.i),prob=p1)-pbinom((bot-x),size=(n-n.i),prob=p1))+
(1-pbinom((top-x),size=(n-n.i),prob=p1))+
(pbinom((bot-x),size=(n-n.i),prob=p1))

sum(dbinom(x,size=n.i,prob=p1)*(pbinom((top-x),size=(n-n.i),prob=p1)-pbinom((bot-x),size=(n-n.i),prob=p1)))+
sum(dbinom(x,size=n.i,prob=p1)*(1-pbinom((top-x),size=(n-n.i),prob=p1)))+
sum(dbinom(x,size=n.i,prob=p1)*(pbinom((bot-x),size=(n-n.i),prob=p1)))


####
###
##
#