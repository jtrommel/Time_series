# Uitleg Wild over decompose van tijdsseries
# werken met beschikbare dataset "Airpassengers"
require(tidyverse)
data("AirPassengers")
ap.df <- as.data.frame(AirPassengers)
ap.df$jaar <- rep(c(1949:1960),each=12)
ap.df$quarter <- rep(rep(c("Q1","Q2","Q3","Q4"),each=3),12)
ap.df %>% group_by(jaar,quarter) %>% summarise(som=sum(x)) -> ap.df.q
ggplot(data=ap.df.q) + geom_point(aes(x=jaar,y=som,color=quarter))
ggplot(data=ap.df.q,aes(x=quarter,y=som,group=jaar)) + geom_line(aes(color=as.factor(jaar)))
#
# ase = additive seasonal effect
#
ap.df.q$ase <- 0
for (i in c(1949:1960)) {
  mod <- lm(ap.df.q$som[ap.df.q$jaar==i] ~ c(1:4))
  ap.df.q$ase[ap.df.q$jaar==i] <- mean(ap.df.q$som[ap.df.q$jaar==i]) + mod$residuals
}
ggplot(data=ap.df.q, aes(x=quarter, group=jaar, color=as.factor(jaar))) +
  geom_line(aes(y=som)) +
  geom_line(aes(y=ase), linetype=3)
#
# Average seasonal effect
#
ap.df.q %>% group_by(quarter) %>% summarise(avg=mean(ase))                                     