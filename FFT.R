# Fast Fourier Transform
library(tidyverse)
Fs <- 1000 # sample frequentie (hier 1 kHz)
Ts <- 1/Fs # sample periode (hier 1 msec)
N <- 1000 # aantal samples 
freq.resolution <- Fs/N # frequentie resolutie
print(freq.resolution)
t <- c(0 : (N-1)*Ts) # tijdsverloop
F0 <- 50 # frequentie van het tijdssignaal
x <- sin(2*pi*F0*t) + 0.6*cos(2*pi*3*F0*t + pi/3) # tijdssignaal
signal <- data.frame(t,x)
signal %>% filter(t<=0.1) %>% ggplot() + geom_line(aes(t,x))
# Berekening van het double sided en single sided spectrum
z <- fft(x)
P2 <- Mod(z/N)
P1 <- P2[1:((N/2)+1)]
P1[2:(length(P1)-1)] <- 2*P1[2:(length(P1)-1)]
freq <- seq(0, (Fs/2)-(Fs/N), Fs/N)
freqspec <- data.frame(freq=freq,amp=P1[1:(N/2)])
ggplot(data=freqspec) + geom_line(aes(freq,amp))
grens <- 0.1
aantal <- length(z[Mod(z)>grens])
fasehoek <- Arg(z[Mod(z)>grens])/pi
resultaat <- data.frame(freq=freqspec$freq[freqspec$amp>grens],
                        amp=freqspec$amp[freqspec$amp>grens],
                        fasehoek=fasehoek[1:(aantal/2)])
print(resultaat)