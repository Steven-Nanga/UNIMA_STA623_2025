set.seed(0000) # REPLACE 0000 with your individual seed value!
# Solutions using the seed value 0000 will not be accepted.

# Generate data
n<-rpois(n=1,lambda=2000)
hospRf<-rnorm(n=8,mean=0,sd=0.5)
hospRf<-hospRf-mean(hospRf)

ilogit<-function(x){
  res<-exp(x)/(1-exp(x))
  return(res)
}

dat<-data.frame(
  PID=paste(sep="","P",25000+1:n),
  sex=sample(c("M","F"),size=n,replace=TRUE,prob=c(0.5,0.5)),
  triage=factor(
    levels=c("Emergency","Priority","Queue"),
    sample(x=c("Emergency","Priority","Queue"),
           size=n,replace=TRUE,prob=c(0.1,0.25,0.65))
  ),
  hospital=factor(
    levels=paste(sep="","H",1:8),
    sample(x=paste(sep="","H",1:8),
           size=n,replace=T,prob=c(0.25,0.15,0.15,rep(0.09,5)))
  )
) %>%
  dplyr::mutate(
    hospRanEf=hospRf[as.integer(hospital)],
    died=rbinom(n=n,size=1,
                prob=ilogit(-4.5
                            +case_when(
                              triage=="Emergency"~rnorm(n=1,mean=1,sd=0.1),
                              triage=="Priority"~rnorm(n=1,mean=0.2,sd=0.02),
                              triage=="Queue"~0)
                            +hospRanEf)
    )
  ) %>%
  dplyr::select(!hospRanEf)