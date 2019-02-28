rm(list=ls())
S = c(969,989,995,957,915,880,858,859,848,836,845)
N = length(S)
R = numeric(N-1)
for (i in 1:(N-1)){
  R[i]=log(S[i]/S[i+1])
}
R_mean = sum(R)/(N-1)
Rt = numeric(N-1)
for (i in 1:(N-1)){
  Rt[i] = (R[i] - R_mean)^2
}
STD = round(sqrt(sum(Rt)/(N-2)) * 100, 4)
HV = round(STD * sqrt(252), 2)


## Bisection Method
S = 100; E = 100; T = 1; Market = 20; r = 0.05
vol_a = 0.0001; vol_b = 5
vol_mid = (vol_a + vol_b)/2; tol = 1e-6; n = 1
while (abs(vol_mid - vol_b) >= tol){
  d1 = (log(S/E) + (r + 0.5 * vol_a^2 * T))/(vol_a * sqrt(T))
  d2 = d1 - vol_a * sqrt(T)
  BScall1 = S * pnorm(d1) - E * exp(-r * T) * pnorm(d2)
  d1 = (log(S/E) + (r + 0.5 * vol_mid^2 * T))/(vol_mid * sqrt(T))
  d2 = d1 - vol_mid * sqrt(T)
  BScall2 = S * pnorm(d1) - E * exp(-r * T) * pnorm(d2)
  
  f_a = BScall1 - Market; f_mid = BScall2 - Market
  
  if (f_a * f_mid <= 0) {
    vol_b <- vol_mid
  }
  else {
    vol_a <- vol_mid
  }
  vol_mid <- 0.5 * (vol_a + vol_b)
  n <- n + 1
}
Bisection_n = n
Bisection_vol = vol_mid
c(Bisection_n, Bisection_vol)

##Newton-Rhapson Method
S = 100; E = 100; T = 1; V_Market = 20; r = 0.05
vol = 0.2; tol = 1e-6; n = 1
d1 = (log(S/E) + (r + 0.5 * vol^2 * T))/(vol*sqrt(T))
d2 = d1 - vol*sqrt(T)
V_Newton = S * pnorm(d1) - E * exp(-r*T) * pnorm(d2)
diff = V_Newton - V_Market
vega = S * dnorm(d1) * sqrt(T)
while (abs(diff) >= tol){
  vol <- vol - diff/vega
  d1 = (log(S/E) + (r + 0.5 * vol^2 * T))/(vol*sqrt(T))
  d2 = d1 - vol*sqrt(T)
  V_Newton = S * pnorm(d1) - E * exp(-r*T) * pnorm(d2)
  diff = V_Newton - V_Market
  vega = S * dnorm(d1) * sqrt(T)
  n <- n + 1
}
Newton_n = n; Newton_Implied_vol = vol
c(Newton_n, Newton_Implied_vol)
c(Bisection_n, Bisection_vol)

##Exercise
library(Deriv)
x = 1; tol = 1e-10; n = 0
f <- function(x) exp(2*x) - exp(x) - 2
# Choose whatever you want from below.
# f1 <- function(x) 2*exp(2*x) - exp(x)
# f1 <- Deriv(f) 
c(f(0),f(1)) ## root s.t. f(root) = 0 is between 0 and 1.
while (abs(f(x)) >= tol){
  x <- x - (f(x)/f1(x))
  n <- n + 1
}
c(n,x)
