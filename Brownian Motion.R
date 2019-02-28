rm(list=ls())
N <- 1e4; T <- 1; dt = T/N
W <- numeric(N+1)
t <- seq(0, 1, dt);
for (i in 2:N+1){
  W[i] <- W[i-1] + sqrt(dt) * rnorm(1)
}
plot(t, W, xlab = "T", main = "Brownian Path", type = 'l')
