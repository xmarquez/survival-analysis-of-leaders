#The DD data has some missing values that I've replaced manually. 
#un_region_name for Tuvalu should be Polynesia, and un_continent_name should be Oceania; 
#un_continent_name for Taiwan should be Asia, and un_region_name should be Eastern Asia
#North and South Vietnam do not have leadership info
#I've also generated a new variable, edate2, from edate. edate2 is the year in which the 
#effective head of government took power. 

#Reads the manually modified dataset
dd <- read.csv("ddrevisited-data-v1.csv")

#Create factors for easier use
dd$democracy <- factor(dd$democracy,labels=c("Non-democracy","Democracy"))
dd <- transform(dd, regime2 = ifelse(regime < 3, 1, regime))
dd$regime <- factor(dd$regime,labels=c("Parliamentary Dem","Mixed Dem","Presidential Dem","Civilian Dict","Military Dict","Monarchy"))
dd$regime2 <- factor(dd$regime2,labels=c("Democracy","Civilian Dict","Military Dict","Monarchy"))
dd$comm <- factor(dd$comm,labels=c("Non-communist","Communist"))

#fill in the missing dates of entry of leaders
dd <- ddply(dd,.(ctryname,ehead,tenure08),transform,edate3 = min(edate2,na.rm=TRUE))
#these dates have to be adjusted manually for 18 leaders; do
#subset(dd,edate3==Inf)$ehead 
#to see which

#set up the censoring variable properly
dd <- ddply(dd,.(ctryname,ehead,tenure08),transform,ecens2 = min(ecens08,na.rm=TRUE))

#generate regime tenure variable
dd2 <- ddply(dd,.(ctryname,ehead,regime2,edate3),transform,regtenure = length(regime2))

#drop uneeded variables; we keep one code (cowcode)
dd <- dd[,c(1:3,5,26,28,58,62,63,67,70,79,80,81,83)]

#Now we use reshape to get the data in the right format
library(reshape)
ddmelted <- melt(dd,id=c(1:14))
ddsurvival <- cast(ddmelted, ctryname + cowcode + un_region_name + un_continent_name + ehead + tenure08 + democracy + stra + edate3 ~ variable,min)
ddsurvival <- transform(ddsurvival,enddate = edate3+tenure08-1)

dd2 <- dd2[,c(1:3,5,26,28,58,62,63,67,70,79,80,81,83,84)]
dd2melted <- melt(dd2,id=c(1:14,16))
ddsurvival2 <- cast(dd2melted, ctryname + cowcode + un_region_name + un_continent_name + ehead + regime2 + regtenure + stra + edate3 ~ variable,min)
ddsurvival2 <- transform(ddsurvival2,enddate = edate3+tenure08-1)

write.csv(ddsurvival,"ddsurvival.csv")
write.csv(ddsurvival2,"ddsurvival2.csv")

#to create ddcoldwar.csv, we need to split the cases that "straddle" 1989, which we take as the end of the cold war. 
#There is probably a much easier and faster way to do this

ddsurvivalcw1 <- subset(ddsurvival,enddate <= 1989)
ddsurvivalcw2 <- subset(ddsurvival,edate3 > 1989)
ddsurvivalcw1 <- transform(ddsurvivalcw1,coldwar=TRUE)
ddsurvivalcw2 <- transform(ddsurvivalcw2,coldwar=FALSE)
a <- subset(ddsurvival,enddate > 1989)
b <- subset(a,edate3 < 1989)
a <- b
a <- transform(a,enddate=1989)
a <- transform(a,tenure08=enddate-edate3+1)
a <- transform(a,coldwar=TRUE)
b <- transform(b,edate3=1989)
b <- transform(b,tenure08=enddate-edate3)
b <- transform(b, coldwar=FALSE)

ddcoldwar <- rbind(ddsurvivalcw1,ddsurvivalcw2,a,b)

write.csv(ddcoldwar,"ddcoldwar.csv")