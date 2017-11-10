# Fast Fourier Transform
library(tidyverse)
Fs <- 1000 # sample frequentie (hier 1 kHz)
Ts <- 1/Fs # sample periode (hier 1 msec)
L <- 1000 # lengte van het tijdssignaal als veelvoud van Ts (hier 1 sec)
t <- c(0 : (L-1)*Ts) # tijdsverloop
F0 <- 50 # frequentie van het tijdssignaal
x <- sin(2*pi*F0*t) # tijdssignaal
signal <- data.frame(t,x)
signal %>% filter(t<=0.1) %>% ggplot() + geom_line(aes(t,x))
# Berekening van het double sided en single sided spectrum
z <- fft(x)
P2 <- abs(z/L)
P1 <- P2[1:((L/2)+1)]
P1[2:(length(P1)-1)] <- 2*P1[2:(length(P1)-1)]
freq <- seq(0, (Fs/2)-(Fs/L), Fs/L)
freqspec <- data.frame(freq=freq,amp=P1[1:(L/2)])
ggplot(data=freqspec) + geom_line(aes(freq,amp))
