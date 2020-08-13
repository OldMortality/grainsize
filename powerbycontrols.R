library(MASS)
library(purrr)
library(pipeR)

getpow2 <- function(nt,nc, diff.t, diff.c, sd, rho,  N.SIMS = 1000) {
  
  if (nc < 0) nc <- nt
  
  d.cov <- rho * sd ^ 2 
  sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
  
  
  pow <- 0
  x.m <- vector()
  s.m <- vector()
  base <- 2.9
  for (sim in 1:N.SIMS) {
    x <- mvrnorm(nt , mu= c(base,base + diff.t) ,Sigma = sigma)
    x1 <- x[,1]
    x2 <- x[,2]
    # control group
    xc <- mvrnorm(nc , mu= c(base,base + diff.c) ,Sigma = sigma)
    x1c <- xc[,1]
    x2c <- xc[,2]
    
   
    if (t.test(x1-x2,x1c-x2c)$p.val < 0.05) {
      pow <- pow + 1
    }
  }
  pow/N.SIMS
  
}

# get ballpark
getpow2(nt=37,nc=-1, diff.t=-0.6, diff.c=-0.3, sd=0.7, rho=0.8,  N.SIMS = 5000)

# do power plot
lowest <- 25
highest <- 45

par(mfrow=c(1,1))
plot('',xlim=c(lowest,highest),ylim=c(0,1),
     main='power curve',xlab='Number in each group')

count <- 0
#for (nc in c(-1,10,20,100,1000)) {
## set the number of controls == number of treatments by setting nc=-1
for (nc in c(-1)) {
  count <- count + 1
  pows2 <- vector()
  ns <- seq(lowest,highest,1)
  ns %>% map(getpow2,nc = nc, diff.t = -0.6, diff.c = -0.3, sd=0.7, rho = 0.8, N.SIMS=5000) %>% 
    unlist %>% 
    ( function(x) { c(rep(NA,lowest),x) } ) %>>% 
    (~ pows2) 
  lines(x = seq(lowest+1,highest+1), y=pows2[(lowest+1):(highest+1)] ,col=rainbow(5)[count])
}
abline(h=0.8,col='blue')
#legend(-100,1,legend=c('balanced',1000,100,20,10),fill = rainbow(5)[5:1])
grid()
min(which(pows2>0.8))
