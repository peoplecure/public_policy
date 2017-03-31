crime <- read.csv("attach/Crime_Incidents_2016.csv")


# Naming Districts  ————————————————————————————————————————
d1 <- crime$WARD=="1"
d2 <- crime$WARD=="2"
d3 <- crime$WARD=="3"
d4 <- crime$WARD=="4"
d5 <- crime$WARD=="5"
d6 <- crime$WARD=="6"
d7 <- crime$WARD=="7"
d8 <- crime$WARD=="8"
raw.tot <- sum(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8)
tot <- crime$TOT
X <- crime$X
Y <- crime$Y
shift <- crime$SHIFT
ward <- crime$WARD
cr <- crime$CR
year <- crime$YR
month <- crime$MO
descriptive <- data.frame(ds, shift, cr)
df <- data.frame(ward, shift, year, month, cr, tot, X, Y) 
# End of Naming  ————————————————————————————————————————



# Tabling: Crime Ratio by Raw Number  ————————————————————————————————————————
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
descriptive.freq <- crosstab(descriptive, row.vars = "ward", col.vars = "cr", type = "f")
descriptive.freq.joint <- crosstab(descriptive, row.vars = c("ward", "shift"), col.vars = "cr", type = "f")
descriptive.perc.col <- crosstab(descriptive, row.vars = "ward", col.vars = "cr", type = "c")
descriptive.perc.row <- crosstab(descriptive, row.vars = "ward", col.vars = "cr", type = "r")
descriptive.perc.joint <- crosstab(descriptive, row.vars = c("ward", "shift"), col.vars = "cr", type = "t")
descriptive.freq.joint
descriptive.perc.joint

# ...Or, by hand: Crime Ratio by Raw Number
district.table <- addmargins(table(crime$WARD, crime$CR))
rownames(district.table) <- c('District 1','District 2','District 3','District 4','District 5','District 6','District 7','District 8','Total')
colnames(district.table) <- c('Arson','Assault*','Burglary','Homicide','Veh Theft','Robbery','Sexual Abuse','Theft F/Auto','Theft/Other', 'Total')
district.table

# ... By hand: Crime Ratio by Percentage
percent.table <- addmargins(round(prop.table(table(crime$WARD, crime$CR),2)*100,2))
rownames(percent.table) <- c('District 1','District 2','District 3','District 4','District 5','District 6','District 7','District 8','Total')
colnames(percent.table) <- c('Arson','Assault*','Burglary','Homicide','Veh Theft','Robbery','Sexual Abuse','Theft F/Auto','Theft/Other', 'Total')
percent.table
# End of Tabling  ————————————————————————————————————————




# Playground  ————————————————————————————————————————
sjt.df(crime)
data(district.table)
plot(district.table)
sjt.frq(as.data.frame(district.table))

crosstab("crime$WARD", "crime$CR", type = "f")

