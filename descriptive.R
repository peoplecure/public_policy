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
raw.tot <- sum(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8)
ds <- crime$WARD
cr <- crime$CR
dn <- crime$SHIFT
descriptive <- data.frame(ds, dn, cr)

# Crime Ratio by Raw Number
descriptive.freq <- crosstab(descriptive, row.vars = "ds", col.vars = "cr", type = "f")
descriptive.perc <- crosstab(descriptive, row.vars = "ds", col.vars = "cr", type = "r")

# ... By hand: Crime Ratio by Raw Number
district.table <- addmargins(table(crime$WARD, crime$CR))
rownames(district.table) <- c('District 1','District 2','District 3','District 4','District 5','District 6','District 7','District 8','Total')
colnames(district.table) <- c('Arson','Assault*','Burglary','Homicide','Veh Theft','Robbery','Sexual Abuse','Theft F/Auto','Theft/Other', 'Total')
district.table

# ... By hand: Crime Ratio by Percentage
percent.table <- addmargins(round(prop.table(table(crime$WARD, crime$CR),2)*100,2))
rownames(percent.table) <- c('District 1','District 2','District 3','District 4','District 5','District 6','District 7','District 8','Total')
colnames(percent.table) <- c('Arson','Assault*','Burglary','Homicide','Veh Theft','Robbery','Sexual Abuse','Theft F/Auto','Theft/Other', 'Total')
percent.table


# Playground ————————————————————————————————————————
sjt.df(crime)
data(district.table)
plot(district.table)
sjt.frq(as.data.frame(district.table))

crosstab("crime$WARD", "crime$CR", type = "f")

