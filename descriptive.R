crime <- read.csv("attach/Crime_Incidents_2016.csv")

# Districts
d1 <- crime$WARD=="1"
d2 <- crime$WARD=="2"
d3 <- crime$WARD=="3"
d4 <- crime$WARD=="4"
d5 <- crime$WARD=="5"
d6 <- crime$WARD=="6"
d7 <- crime$WARD=="7"
d8 <- crime$WARD=="8"
raw.tot <- sum(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8) + 1

# Crime Ratio
summary(crime$WARD)

sum(d1)/raw.tot
sum(d2)/raw.tot
sum(d3)/raw.tot
sum(d4)/raw.tot
sum(d5)/raw.tot
sum(d6)/raw.tot
sum(d7)/raw.tot


yes.CI <- CI>=1
yes.EP <- EP>=1
freq.table.CI.EP = addmargins(table(yes.CI, yes.EP)) 
rownames(freq.table.CI.EP) <- c('No Injury', 'Civilian Injury', 'total')
colnames(freq.table.CI.EP) <- c('No EMS', 'EMS Only', 'total')
freq.table.CI.EP

percent.CI.EP <- addmargins(round(prop.table(table(yes.CI, yes.EP),2)*100,2))
rownames(percent.CI.EP) <- c('No Injury', 'Civilian Injury', 'total')
colnames(percent.CI.EP) <- c('No EMS', 'EMS Only', 'total')
percent.CI.EP
