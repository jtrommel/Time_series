# Finding periodicity with "lag"
library(tidyverse)
library(gridExtra)
Fs <- 1000 # sample frequentie (hier 1 kHz)
Ts <- 1/Fs # sample periode (hier 1 msec)
N <- 100 # aantal samples 
t <- c(0 : (N-1)*Ts) # tijdsverloop
F0 <- 20 # frequentie van het tijdssignaal
y <- sin(2*pi*F0*t) + cos(2*pi*3*F0*t) # tijdssignaal
y.ts <- ts(y)
signal <- data.frame(t,y)
signal %>% filter(t<=0.1) %>% ggplot() + geom_line(aes(t,y))
# lag 0
lag <- 0
signal.lag <- data.frame(x1=y,x2=y)
cov(signal.lag$x1,signal.lag$x2)
p0 <- ggplot(data=signal.lag) +
  geom_point(aes(x=x1,y=x2)) +
  labs(x="y[1:N]",y="y[1:N]",title="lag=0")
# lag 1
lag <- 1
signal.lag <- data.frame(x1=y[-c(N:(N-lag+1))],x2=y[-c(1:lag)])
p1 <- ggplot(data=signal.lag) +
  geom_point(aes(x=x1,y=x2)) +
  labs(x="y[1:(N-lag+1)]",y="y[(1+lag):N]",title="lag=1")
# lag 10
lag <- 10
signal.lag <- data.frame(x1=y[-c(N:(N-lag+1))],x2=y[-c(1:lag)])
cov(signal.lag$x1,signal.lag$x2)
p10 <- ggplot(data=signal.lag) +
  geom_point(aes(x=x1,y=x2)) +
  labs(x="y[1:(N-lag+1)]",y="y[(1+lag):N]",title="lag=10")
# lag 20
lag <- 20
signal.lag <- data.frame(x1=y[-c(N:(N-lag+1))],x2=y[-c(1:lag)])
cov(signal.lag$x1,signal.lag$x2)
p20 <- ggplot(data=signal.lag) +
  geom_point(aes(x=x1,y=x2)) +
  labs(x="y[1:(N-lag+1)]",y="y[(1+lag):N]",title="lag=20")
grid.arrange(p0,p1,p10,p20,ncol=2)
par(mfrow=c(1:2))
pacf <- acf(y)
# The graph gets smaller because less and less points are taken into account. If we had inf. number
# of points it there would be no difference between lag=0, lag=T0, lag=2*T0 ...
ppacf <- pacf(y)
# Very hard to read. What is the connection with the original signal???
par(mfrow=c(1,1))
spec.y <- spectrum(y) # Unit of frequency is unit of sample frequency (here kHz)
spec.y$freq[spec.y$spec>spec.y$spec[1]]*Fs

