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

#find leadership transitions; this code is slower than it needs to be, I'm sure there are better ways
a <- 2:length(dd$ehead) #a counter variable
b <- 1 #another counter
leaderspellnum <- rep(1,length(dd$ehead))
for(i in a) { 
		if (dd$ehead[i-1] != dd$ehead[i] && dd$ctryname[i-1]==dd$ctryname[i]) 
			{
				b <- b + 1
				leaderspellnum[i:length(leaderspellnum)]=b
			}	
		}	
dd <- cbind(dd,leaderspellnum)

#fill in the missing dates of entry of leaders and adjust tenure

library(plyr)
#Create a "leader spell" identifier for each leader/democracy/entry date triad
dd <- ddply(dd,.(leaderspellnum,democracy),transform,leaderspelldem = paste(ehead,ctryname,min(year),democracy,sep="."))
dd <- dd[ order(dd$order), ]

#Create a new tenure variable for the democratic leader spell
dd <- ddply(dd,.(leaderspelldem),transform,tenuredem = length(year))
dd <- dd[ order(dd$order), ]


#Add a variable for the entry date of the leader spell
dd <- ddply(dd,.(leaderspelldem),transform,edatedem = min(year))
dd <- dd[ order(dd$order), ]

# There are 173 cases where the tenure variable created in this way differs from tenure08;
# use unique(subset(dd,tenure08 != tenuredem)$leaderspelldem) to see them. Many of these are cases of 
# "left censoring" - the leader enters into the dataset after 1946 already in power. To fix those cases, 
# we do the following:

dd <- ddply(dd,.(leaderspelldem),transform,edate2 = min(edate2,na.rm=TRUE))
dd <- transform(dd, edate2 = ifelse(edate2 == Inf, edatedem, edate2))
dd <- dd[ order(dd$order), ]
dd <- transform(dd,edate2 = ifelse(leaderspelldem == "Sukarno.Indonesia.1949.Non-democracy",1946,edate2)) #Manually adjust Sukarno

dd <- transform(dd, tenuredemadj = ifelse(edate2 < entryy ,tenure08, tenuredem)) 

#Create a "leader spell" identifier for each leader/regime/entry date triad
dd <- ddply(dd,.(leaderspellnum,regime),transform,leaderspellreg = paste(ehead,ctryname,min(year),regime,sep="."))
dd <- dd[ order(dd$order), ]

#Same thing for regime2
dd <- ddply(dd,.(leaderspellnum,regime2),transform,leaderspellreg2 = paste(ehead,ctryname,min(year),regime2,sep="."))
dd <- dd[ order(dd$order), ]

#Create tenure variables for leader spells per regime
dd <- ddply(dd,.(leaderspellreg),transform,tenurereg = length(year))
dd <- dd[ order(dd$order), ]

#Create left-censoring indicator
dd <- transform(dd, leftcensored = ifelse(edate2 < entryy,TRUE, FALSE)) 

dd <- ddply(dd,.(leaderspellreg2),transform,tenurereg2 = length(year))
dd <- dd[ order(dd$order), ]

#Add a variable for the entry date of the leader spell for each regime type
dd <- ddply(dd,.(leaderspellreg),transform,edatereg = min(year))
dd <- dd[ order(dd$order), ]

dd <- ddply(dd,.(leaderspellreg2),transform,edatereg2 = min(year))
dd <- dd[ order(dd$order), ]

#drop uneeded variables; we keep two codes (cowcode,politycode)
dd <- dd[,c(1:3,5,26,28,58,62,63,67,70,79,80,81,83)]

#set up the right-censoring indicator
dd <- ddply(dd,.(leaderspelldem),transform,rightcensdem = min(ecens08,na.rm=TRUE))
dd <- ddply(dd,.(leaderspellreg),transform,rightcensreg = min(ecens08,na.rm=TRUE))

#Now we create the sample files
ddsurvival <- dd[,c(2,5,26,28,58,67,81,82,84,91,92,93)]
ddsurvival <- unique(ddsurvival)

ddsurvival2 <- dd[,c(2,5,26,28,58,67,85,87,89,92,94)]
ddsurvival2 <- unique(ddsurvival2)

write.csv(ddsurvival,"ddsurvival.csv")
write.csv(ddsurvival2,"ddsurvival2.csv")

#to create ddcoldwar.csv, we need to split the cases that "straddle" 1989, which we take as the end of the cold war. 
#There is probably a much easier and faster way to do this

#create enddates

dd <- transform(dd, enddate = edate2 + tenuredemadj - 1)
ddsurvival3 <- dd[,c(2,5,26,28,58,61,95,67,81,82,91,92,93)]
ddsurvival3 <- unique(ddsurvival3)
ddsurvivalcw1 <- subset(ddsurvival3,enddate <= 1989)
ddsurvivalcw2 <- subset(ddsurvival3,edate2 > 1989)
ddsurvivalcw1 <- transform(ddsurvivalcw1,coldwar=TRUE)
ddsurvivalcw2 <- transform(ddsurvivalcw2,coldwar=FALSE)
a <- subset(ddsurvival3,enddate > 1989)
b <- subset(a,edate2 < 1989)
a <- b
a <- transform(a,enddate=1989)
a <- transform(a,tenuredemadj = enddate - edate2 + 1)
a <- transform(a,coldwar=TRUE)
b <- transform(b,edate2=1989)
b <- transform(b,tenuredemadj = enddate - edate2)
b <- transform(b, coldwar=FALSE)

ddcoldwar <- rbind(ddsurvivalcw1,ddsurvivalcw2,a,b)

write.csv(ddcoldwar,"ddcoldwar.csv")