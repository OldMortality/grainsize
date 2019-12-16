library(MASS)


head(df)
powers <- vector()
# t.test on the difference
N.SIMS <- 10000
for (N in 35:60) {
  #
  base <- 75
  sd <- 15  #18.6
  effect.size <-  -7 #10.93
  ids <- c(seq(1:N),seq(1:N))
  errs <- 0
  for (sim in 1:N.SIMS) {
    if (sim %% 1000 == 0) { print(c(N,sim,1-errs/sim))}  
    # control group
    Means <- c(base,base)
    d.cov <- 0.7 * sd ^ 2 
    Sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
    simulation <- mvrnorm(n = N, Means, Sigma)
    t0 <- simulation[,1]
    t1 <- simulation[,2]
    df.control <- data.frame(response=c(t0,t1),
                             id = seq(1,N),
                             time = c(rep('0',N),
                                      rep('1',N)),
                             type='control')
    # treatment group
    Means <- c(base,base+effect.size)
    d.cov <- 0.7 * sd ^ 2 
    Sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
    simulation <- mvrnorm(n = N, Means, Sigma)
    t0 <- simulation[,1]
    t1 <- simulation[,2]
    df.treat <- data.frame(response=c(t0,t1),
                           id = seq(N+1,2*N),
                           time = c(rep('0',N),
                                    rep('1',N)),
                           type='treat')
    
    df <- rbind(df.control,df.treat)
    df$id <- factor(df$id)
    df$time <- factor(df$time)
    #sqrt(var(x))
    #sqrt(var(y))
    #cor(x,y)
    
    diff <- vector()
    for (id in seq(1,2*N)) {
      diff[id] <-  df[which(df$id==id),c("response","type")][2,1]-df[which(df$id==id),c("response","type")][1,1]
    }
    #df[which(df$id==33),]
    
    type <- c(rep('control',N),rep('treat',N))
    df2 <- data.frame(diff=diff,type=type)
    pval <- t.test(diff~type,data=df2)$p.val
    
    
    
    
    if ( pval > 0.05 ) {
      # ci contains zero. No difference detected
      errs <- errs + 1
    }
    
    
  }
  print(c(N,1-errs/N.SIMS))
  powers[N] <- 1 - errs/N.SIMS
}


sd <- 1.4
d.cov <- 0.8 * sd ^ 2 
sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 


diff <- -0.57
x <- mvrnorm(n = 10000, mu= c(8.9,8.9 + diff) ,Sigma = sigma)
x1 <- x[,1]
x2 <- x[,2]
mean(x1)
mean(x2)
cor(x1,x2)
sqrt(var(x1))
sqrt(var(x2))

plot(density(x1))
lines(density(x2),col='red')
abline(v=7,col='blue')
plot(density(x1-x2))
mean(x1-x2)
var(x1-x2)



rm(list = ls())




getpow2 <- function(n, diff, sd, rho,  N.SIMS = 1000) {

  d.cov <- rho * sd ^ 2 
  sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
  
  
  pow <- 0
  x.m <- vector()
  s.m <- vector()
  base <- 8.6
  for (sim in 1:N.SIMS) {
    x <- mvrnorm(n , mu= c(base,base + diff) ,Sigma = sigma)
    x1 <- x[,1]
    x2 <- x[,2]
    # control group
    xc <- mvrnorm(n , mu= c(base,base ) ,Sigma = sigma)
    x1c <- xc[,1]
    x2c <- xc[,2]
    
    # x.m[sim] <- mean(x1-x2)
    # xc.m[sim] <- mean(x1-x2)
    # 
    #s.m[sim] <- sqrt(var(x1-x2))
    if (t.test(x1-x2,x1c-x2c)$p.val < 0.05) {
      pow <- pow + 1
    }
  }
  pow/N.SIMS
  
}


getpow <- function(n,N.SIMS = 1000) {

diff <- -0.4
pow <- 0

x.m <- vector()
s.m <- vector()
base <- 0
for (sim in 1:N.SIMS) {
  x <- mvrnorm(n , mu= c(base,base + diff) ,Sigma = sigma)
  x1 <- x[,1]
  x2 <- x[,2]
  x.m[sim] <- mean(x1-x2)
  s.m[sim] <- sqrt(var(x1-x2))
  if (t.test(x1-x2)$p.val < 0.05) {
    pow <- pow + 1
  }
}
pow/N.SIMS

}

library(pipeR)
par(mfrow=c(1,1))

lowest <- 50
highest <- 90
ns <- seq(lowest,highest,1)
ns %>% map(getpow2,diff = -0.4,sd=1.4, rho = 0.8, N.SIMS=5000) %>% 
  unlist %>% 
  ( function(x) { c(rep(NA,lowest),x) } ) %>>% 
  (~ pows2) %>%
  plot
lines(pows2)  
abline(h=0.8,col='blue')
pows2

#pows <- map(ns,getpow)

#ns %>>%  (~ fred)

plot(ns,unlist(pows))
lines(ns,unlist(pows))
lines(ns,unlist(pows2),col='blue')
lines(ns,1-pnorm(1.75/sqrt(ns),mean=0.57,sd=sqrt(0.8/ns)),col='red')
abline(h=0.8)
grid()
#unlist(pows2)


hist(s.m,prob=T)
abline(v=0.8,col='red')
mean(s.m)
hist(x.m)
mean(x.m)
sqrt(var(x.m))

N <- 41
# sampling distribution of x1-x2
par(mfrow=c(1,1))
x.bar.h0 <- rnorm(10000,mean=0   ,sd=sqrt(0.8/N))
x.bar.h1 <- rnorm(10000,mean=0.57,sd=sqrt(0.8/N))
plot(density(x.bar.h0),xlim=c(-1,1))
lines(density(x.bar.h1),col='red',xlim=c(-1,1))


#acceptance region for H0
upp <- 1.96 * sqrt(0.8/N)
low <- -1.96 * sqrt(0.8/N)
abline(v= c(low,upp)  ,col='blue')
# power
sum(x.bar.h1 < upp) /length(x.bar.h1)

# power
1-pnorm(upp,mean=0.57,sd=sqrt(0.8/N))


#effect of rho

rho = 0.2
d.cov <- rho * sd ^ 2 
sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
base <- 8.6
x <- mvrnorm(n , mu= c(base,base + diff) ,Sigma = sigma)
x1 <- x[,1]
x2 <- x[,2]
xc <- mvrnorm(n , mu= c(base,base ) ,Sigma = sigma)
x1c <- xc[,1]
x2c <- xc[,2]

par(mfrow=c(2,1))
plot(density(x2-x1),xlim=c(-10,10),ylim=c(0,1),main=rho)
lines(density(x2c-x1c),col='red')


