
## power calculation for Andrew Reynolds
rm(list=ls())
library(pipeR)
library(tmvtnorm)

calculatePower <- function(n, diff, sd, rho,  N.SIMS = 1000) {

  d.cov <- rho * sd ^ 2 
  sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
  pow <- 0
  base <- 8.6 # irrelevant, really
  for (sim in 1:N.SIMS) {
    # treatment group
    x <- mvrnorm(n , mu= c(base,base + diff), Sigma = sigma)
    # control group
    xc <- mvrnorm(n , mu= c(base,base), Sigma = sigma)
    if (t.test(x[,1]-x[,2],xc[,1]-xc[,2])$p.val < 0.05) {
      pow <- pow + 1
    }
  }
  pow/N.SIMS
}
# x <- rtmvnorm(n, mean=c(base,base + diff),
#          sigma=sigma, lower=c(6,0)) 
# xc <- rtmvnorm(n, mean=c(base,base),
#               sigma=sigma, lower=c(6,0)) 



par(mfrow=c(1,1))


lowest <- 50
highest <- 100
plot('',xlim=c(lowest,highest),ylim=c(0,1),
     xlab='N (per group)',ylab='power')
ns <- seq(lowest,highest,1)
ns %>% map(calculatePower,diff = -0.4,sd=1.4, rho = 0.8, N.SIMS=3000) %>% 
  unlist %>% 
  ( function(x) { c(rep(NA,lowest),x) } ) %>>% 
  (~ powers) %>%
  lines
abline(h=0.8,col='blue')
which(powers > 0.8)
powers[60]
grid()


## how does the power vary with unbalanced design
# calculatePowerUnbalanced <- function(n, nc, diff, sd, rho,  N.SIMS = 1000) {
#   
#   d.cov <- rho * sd ^ 2 
#   sigma <- matrix(c(sd^2,d.cov,d.cov,sd^2),nrow = 2) 
#   pow <- 0
#   x.m <- vector()
#   s.m <- vector()
#   base <- 8.6
#   for (sim in 1:N.SIMS) {
#     x <- mvrnorm(n , mu= c(base,base + diff) ,Sigma = sigma)
#     x1 <- x[,1]
#     x2 <- x[,2]
#     # control group
#     xc <- mvrnorm(nc , mu= c(base,base ) ,Sigma = sigma)
#     x1c <- xc[,1]
#     x2c <- xc[,2]
#     
#     if (t.test(x1-x2,x1c-x2c)$p.val < 0.05) {
#       pow <- pow + 1
#     }
#   }
#   pow/N.SIMS
# }
# 
# pows <- vector()
# for (nc in 2:158) {
#   nt <- 160 - nc
#   pows[nt] <- calculatePowerUnbalanced(n = nt, nc=nc, diff = -0.4,sd=1.4, rho = 0.8, N.SIMS=1000)
# }
# plot(pows,xlab='n treatment',ylab='power')
# abline(h=0.80,col='blue')
