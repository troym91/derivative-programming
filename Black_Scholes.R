rm(list=ls())
## Computing Call Price using B.S. Formula
S = 240; E = 250; T = 2/12; vol = 0.38; r = 0.06
d1 = (log(S/E) + (r+0.5*vol^2)*T)/(vol*sqrt(T))
d2 = d1 - vol*sqrt(T)
Call_Price = S*pnorm(d1)-E*exp(-r*T)*pnorm(d2)
round(Call_Price,4)

## What if S changes from 0 to 400 by increment of 8?
S = seq(0,400,8); E = 250; T = 2/12; vol = 0.38; r = 0.06
d1 = (log(S/E) + (r+0.5*vol^2)*T)/(vol*sqrt(T))
d2 = d1 - vol*sqrt(T)
Call_Price = S*pnorm(d1)-E*exp(-r*T)*pnorm(d2)
plot(S, Call_Price, main = "How Stock Price Change Affects Call Price")
lines(S, Call_Price)

## We shall see how changes in E(Exercise/Strike Price), T(Maturity), vol(Volatility), 
## r(risk free rate of return) affects Call price as S(Stock Price) increases.

## 3 Exercise Prices(200, 250, 300)?
S = seq(0,400,8); E = c(200, 250, 300); T = 2/12; vol = 0.38; r = 0.06
Call_Price = matrix(0, nrow = length(S), ncol = length(E))
for (i in 1:3){
  d1 = (log(S/E[i]) + (r+0.5*vol^2)*T)/(vol*sqrt(T))
  d2 = d1 - vol*sqrt(T)
  Call_Price[,i] = S*pnorm(d1)-E[i]*exp(-r*T)*pnorm(d2)
}

df.S <- as.data.frame(cbind(S,Call_Price))
colnames(df.S) <- c("S", "E1", "E2", "E3")
colnames(df.S)

library("dplyr")
library("plotly")
p.E <- plot_ly(data = df.S, x = ~S, y = ~E1,  name = 'E = 200', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(data = df.S, y = ~E2, name = 'E = 250', mode = 'lines+markers') %>%
  add_trace(data = df.S, y = ~E3, name = 'E = 300', mode = 'lines+markers') %>%
  layout(
    title = "How Stock Price Change Affects Call Price",
    xaxis = list(title = "S"), yaxis = list(title = "E"))
p.E

## 3 interest rates(0.0, 0.2, 0.4)?
S = seq(0, 400, 8); E = 250; T= 2/12; vol = 0.38; r = c(0.0, 0.2, 0.4)
Call_Price = as.data.frame(matrix(0,nrow=length(S),ncol=length(r)))
for (i in 1:3){
  d1 = (log(S/E) + (r[i] + 0.5 * vol^2)*T)/(vol*sqrt(T))
  d2 = d1 - vol*sqrt(T)
  Call_Price[,i] = S * pnorm(d1) - E * exp(-r[i]*T) * pnorm(d2)
}

df.r = cbind(S, Call_Price)
colnames(df.r) <- c("S", "r1", "r2", "r3")
p.r <- plot_ly(data = df.r, x = ~S, y = ~r1, name = 'r = 0%',
               type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "How Stock Price Change Affects Call", 
         xaxis = list(title = "Stock Price"), yaxis = list(title = "Call Price")) %>%
  add_trace(data = df.r, x = ~S, y = ~r2, name = 'r = 20%', mode = 'lines+markers') %>%
  add_trace(data = df.r, x = ~S, y = ~r3, name = 'r = 40%', mode = 'lines+markers')
p.r  
package_version(R.version)

## 3 maturities(a month, 5 months, 10 months)?
rm(list=ls())
S = seq(0, 400, 8); E = 250; vol = 0.38; r = 0.06; T = c(1/12, 5/12, 10/12)
Call_Price = as.data.frame(matrix(0, nrow = length(S), ncol = length(T)))
for (i in 1:3){
  d1 = (log(S/E) + (r + 0.5 * vol^2 * T[i]))/(vol * sqrt(T[i]))
  d2 = d1 - vol * sqrt(T[i])
  Call_Price[,i] = S * pnorm(d1) - E * exp(-r*T[i]) * pnorm(d2)
}
df.T = cbind(S, Call_Price)
colnames(df.T) = c("S", "T1", "T2", "T3")
p.T <- plot_ly(data = df.T, x = ~S, y = ~T1, name = '1 month',
               type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "How Stock Price Change Affects Call", 
         xaxis = list(title = "Stock Price"), yaxis = list(title = "Call Price")) %>%
  add_trace(data = df.T, x = ~S, y = ~T2, name = '5 months', mode = 'lines+markers') %>%
  add_trace(data = df.T, x = ~S, y = ~T3, name = '10 months', mode = 'lines+markers')
p.T

## 3 volatilities(10%, 38%, 70%)?
rm(list=ls())
S = seq(0, 400, 8); E = 250; vol = c(0.10, 0.38, 0.70); r = 0.06; T = 2/12
Call_Price = as.data.frame(matrix(0, nrow = length(S), ncol = length(vol)))
for (i in 1:3){
  d1 = (log(S/E) + (r + 0.5 * vol[i]^2 * T))/(vol * sqrt(T))
  d2 = d1 - vol[i] * sqrt(T)
  Call_Price[,i] = S * pnorm(d1) - E * exp(-r*T) * pnorm(d2)
}
df.vol = cbind(S, Call_Price)
colnames(df.vol) = c("S", "V1", "V2", "V3")
p.vol <- plot_ly(data = df.vol, x = ~S, y = ~V1, name = '10% vol',
               type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "How Stock Price Change Affects Call", 
         xaxis = list(title = "Stock Price"), yaxis = list(title = "Call Price")) %>%
  add_trace(data = df.vol, x = ~S, y = ~V2, name = '38% vol', mode = 'lines+markers') %>%
  add_trace(data = df.vol, x = ~S, y = ~V3, name = '70% vol', mode = 'lines+markers')
p.vol

