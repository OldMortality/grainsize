getpow2 <- function(nt,nc, diff, sd, rho,  N.SIMS = 1000) {
  
  if (nc < 0) nc <- nt
  
  d.cov <- rho * sd ^ 2 
  sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
  
  
  pow <- 0
  x.m <- vector()
  s.m <- vector()
  base <- 8.6
  for (sim in 1:N.SIMS) {
    x <- mvrnorm(nt , mu= c(base,base + diff) ,Sigma = sigma)
    x1 <- x[,1]
    x2 <- x[,2]
    # control group
    xc <- mvrnorm(nc , mu= c(base,base ) ,Sigma = sigma)
    x1c <- xc[,1]
    x2c <- xc[,2]
    
   
    if (t.test(x1-x2,x1c-x2c)$p.val < 0.05) {
      pow <- pow + 1
    }
  }
  pow/N.SIMS
  
}


library(purrr)
library(pipeR)
library(MASS)
par(mfrow=c(1,1))
plot('',xlim=c(-100,250),ylim=c(0,1),
     main='power curve by Ncontrols')

count <- 0
for (nc in c(-1,10,20,100,1000)) {
  count <- count + 1
  pows2 <- vector()
  lowest <- 50
  highest <- 250
  ns <- seq(lowest,highest,1)
  ns %>% map(getpow2,nc = nc,diff = -0.4,sd=1.4, rho = 0.8, N.SIMS=5000) %>% 
    unlist %>% 
    ( function(x) { c(rep(NA,lowest),x) } ) %>>% 
    (~ pows2) 
  lines(x = seq(lowest+1,highest+1), y=pows2[(lowest+1):(highest+1)] ,col=rainbow(5)[count])
}
abline(h=0.8,col='blue')
legend(-100,1,legend=c('balanced',1000,100,20,10),fill = rainbow(5)[5:1])


