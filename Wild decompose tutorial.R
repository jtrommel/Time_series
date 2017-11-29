# Uitleg Wild over decompose van tijdsseries
#
# https://www.youtube.com/watch?v=85XU1T9DIps
#
# werken met beschikbare dataset "Airpassengers"
require(tidyverse)
require(lubridate)
require(forecast)
data("AirPassengers")
ap.df <- as.data.frame(AirPassengers)
ap.df$x <- as.numeric(ap.df$x)
ap.df$jaar <- rep(c(1949:1960),each=12)
ap.df$quarter <- rep(rep(c("Q1","Q2","Q3","Q4"),each=3),12)
ap.df$tijd <- seq(ymd("1949-01-01"),ymd("1960-12-01"),by="months")
ggplot(data=ap.df) +
  geom_line(aes(x=tijd,y=x)) +
  labs(title="original data")
#
# Zoeken van de trendlijn. Hiervoor kunnen we HoltWinters NIET gebruiken 
# omdat die als input een time series object gebruikt.
#
# Met de ma-function (moving average) van package "forecast"
#
ap.df$ma <- ma(ap.df$x, order=5, centre=TRUE)
ggplot(data=ap.df) +
      geom_line(aes(x=tijd,y=x)) +
      geom_line(aes(x=tijd,y=ma),color="red") +
      labs(title="original data", subtitle="moving average (order=5 centre=TRUE)")
# Venster te klein, te veel seasonality, we verliezen info van de eindpunten
ap.df$ma <- ma(ap.df$x, order=40, centre=FALSE)
ggplot(data=ap.df) +
  geom_line(aes(x=tijd,y=x)) +
  geom_line(aes(x=tijd,y=ma),color="red") +
  labs(title="original data", subtitle="moving average (order=40 centre=FALSE)")
# centre beinvloedt enkel hoe er omgegaan wordt met een moving average window dat even is. Dus
# ook hier verlies van info naar het einde.
#
# Met functie rollmean van package "zoo"
require(zoo)
n <- 40
ap.df$rollmean[1:(n-1)] <- NA
ap.df$rollmean[n:nrow(ap.df)] <- rollmean(ap.df$x,n)
ggplot(data=ap.df) +
  geom_line(aes(x=tijd,y=x)) +
  geom_line(aes(x=tijd,y=rollmean),color="red") +
  labs(title="original data", subtitle="rollmean, n=40")
# Loopt duidelijk achter op de feiten! Dat is het klassieke nadeel van een backward looking
# moving average systeem: als er een trend is, dan wordt die pas laat opgepikt.
# We kunnen dat gedeeltelijk opvangen door n kleiner te maken bv. n=12 (smoothing over 1 jaar)
n <- 12
ap.df$rollmean[1:(n-1)] <- NA
ap.df$rollmean[n:nrow(ap.df)] <- rollmean(ap.df$x,n)
ggplot(data=ap.df) +
  geom_line(aes(x=tijd,y=x)) +
  geom_line(aes(x=tijd,y=rollmean),color="red") +
  labs(title="original data", subtitle="rollmean, n=12")
#
# Maar je blijft "te laat" komen.
#
# Om rekening te houden met de trend is er Double Exponential Smoothing (des). Op basis van het artikel
# van Grisha Trubetskoy heb ik dat in een functie ondergebracht. De input is een dataframe met twee 
# variabelen x en y. x=c(1:nfrow(y)) en y=data
#
JT.des <- function(serie.des, alpha=0.1, beta=0.1) {
  serie.des$level <- NA
  serie.des$slope <- NA
  serie.des$yhat <- NA
  serie.des$level[1] <- serie.des$y[1]
  serie.des$slope[1] <- 0
  serie.des$yhat[1] <- serie.des$y[1]
  for (i in (2:nrow(serie.des))) {
    if (i==2) {
      new.level <- serie.des$y[i]
      new.slope <- serie.des$y[i]-serie.des$y[i-1]
      yvalue <- serie.des$y[i]
    } else {
      yvalue <- serie.des$y[i]
      }
  last.level <- serie.des$level[i-1]
  last.slope <- serie.des$slope[i-1]
  new.level <- alpha*yvalue + (1 - alpha)*(last.level + last.slope)
  new.slope <- beta*(new.level - last.level) + (1 - beta)*last.slope
  serie.des$yhat[i] <- new.level + new.slope
  serie.des$level[i] <- new.level
  serie.des$slope[i] <- new.slope
  }
  return(serie.des)
}
ap.df$des <- JT.des(reeks, alpha=0.1, beta=0.1)$yhat
ggplot(data=ap.df, aes(x=tijd)) + 
  geom_line(aes(y=x), colour="black") + 
  geom_line(aes(y=des), colour="red") +
  labs(title="Double exponential smoothing (alpha=0.1, beta=0.1)")
#
# Dat ziet er al wat beter uit, maar de slope-volging geeft ook het volgen van de seizoensgebondenheid
#
ap.df$des <- JT.des(reeks, alpha=0.1, beta=0.05)$yhat
ggplot(data=ap.df, aes(x=tijd)) + 
  geom_line(aes(y=x), colour="black") + 
  geom_line(aes(y=des), colour="red") +
  labs(title="Double exponential smoothing (alpha=0.1, beta=0.05)")
#
ap.df$des <- JT.des(reeks, alpha=0.05, beta=0.1)$yhat
ggplot(data=ap.df, aes(x=tijd)) + 
  geom_line(aes(y=x), colour="black") + 
  geom_line(aes(y=des), colour="red") +
  labs(title="Double exponential smoothing (alpha=0.1, beta=0.05)")
#
# Werken met een polynoommodel
model.pol <- lm(ap.df$x ~ poly(ap.df$tijd,3))
summary(model.pol)
# De berekende waarden zitten in model.pol$fitted.values
ap.df$pol <- model.pol$fitted.values
ggplot(data=ap.df, aes(x=tijd)) + 
  geom_line(aes(y=x), colour="black") + 
  geom_line(aes(y=pol), colour="red") +
  labs(title="Polynoommodel (n=3)")
#
# Werken met een loess-kromme
#
model.loess <- loess(ap.df$x ~ as.numeric(rownames(ap.df)), span=0.5)
ap.df$loess <- predict(model.loess)
ggplot(data=ap.df, aes(x=tijd)) + 
  geom_line(aes(y=x), colour="black") + 
  geom_line(aes(y=loess), colour="red") +
  labs(title="Loess-functie (span=0.5)")
# Op zoek naar seasonality
#
# Groepering per quarter
#
ap.df %>% group_by(jaar,quarter) %>% summarise(som=sum(x)) -> ap.df.q
ggplot(data=ap.df.q) + geom_point(aes(x=jaar,y=som,color=quarter))
ggplot(data=ap.df.q,aes(x=quarter,y=som,group=jaar)) + geom_line(aes(color=as.factor(jaar)))
#
# ase = additive seasonal effect
#
ap.df.q$ase <- 0
for (i in c(1949:1960)) {
  mod <- lm(ap.df.q$som[ap.df.q$jaar==i] ~ c(1:4))
  ap.df.q$ase[ap.df.q$jaar==i] <- mod$residuals
}
ggplot(data=ap.df.q, aes(x=quarter, group=jaar, color=as.factor(jaar))) +
  geom_line(aes(y=ase)) +
  labs(title="additive seasonal effect per year")
#
# Average seasonal effect
#
ap.df.q %>% group_by(quarter) %>% summarise(avg=mean(ase)) -> ap.df.q.average                                     
ggplot(data=ap.df.q.average, aes(x=quarter, y=avg, group=1)) +
  geom_line() +
  labs(title="additive seasonal effect (average)")
