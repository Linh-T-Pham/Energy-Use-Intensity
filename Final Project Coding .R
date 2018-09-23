library(dplyr)
library(tools)
library(readr)
library(ggplot2)

sf <- read.csv('SF Energy Benchmarking .csv')
View(sf)

sf <- rename(sf, Building.Area.sq.ft = X2013.Building.Area..sq..ft..)
sf <- rename(sf, EUI.kBTU.sq.ft = X2013.Site.EUI..kBtu.sq.ft..)
sf <- rename(sf, Building.Category = General.Building.Category)
sf <- rename(sf, Energy.Star.Rating = X2013.ENERGY.STAR.rating)

sf <- mutate(sf, SFPUC.Code = na_if(SFPUC.Code, 'XXX'))
sf <- mutate(sf, Energy.Star.Rating = na_if(Energy.Star.Rating, ''))

sf <- mutate(sf, Facility.Type = toTitleCase(Facility.Type))

is.na(sf$Energy.Star.Rating)

ggplot(sf, aes(x = Year.Built, y = EUI.kBTU.sq.ft)) + 
  theme_bw()+
  geom_point()+
  labs(y = "EUI", x = "Year Built", title = "Year Built vs EUI in 2013")

ggplot(sf, aes(x = EUI.kBTU.sq.ft)) +  
  geom_histogram(binwidth = 30) +  
  labs(title = 'Histogram of EUI')

View(filter(sf, Building.Area.sq.ft > 5000000))
View(filter(sf, EUI.kBTU.sq.ft > 600))
View(filter(sf, EUI.kBTU.sq.ft > 400))
View(filter(sf, EUI.kBTU.sq.ft > 300))

model1 = lm(EUI.kBTU.sq.ft ~ Year.Built, data = sf)  
model1
boxplot(EUI.kBTU.sq.ft ~ Building.Category, data = sf)
boxplot(EUI.kBTU.sq.ft ~ Building.Category, ylab='EUI', xlab='Building Category', main='EUI of Different Building Categories', data = sf) 
abline(model1, col="red")
summary(model1)
cor(sf$EUI.kBTU.sq.ft, sf$Year.Built)
model1$residuals
SSE = sum(model1$residuals^2)
SSE
model3 = lm(EUI.kBTU.sq.ft ~ Year.Built + Building.Category + Building.Area.sq.ft, data = sf) 
model3
summary(model3)
cor(sf)
?if_else()


