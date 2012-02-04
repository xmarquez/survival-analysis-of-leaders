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
		if (dd$ehead[i-1] != dd$ehead[i]) 
			{
				b <- b + 1
				leaderspellnum[i:length(leaderspellnum)]<-b
			}	
		}	
dd <- cbind(dd,leaderspellnum)

#find regime transitions
a <- 2:length(dd$regime) #a counter variable
b <- 1 #another counter
regimespellnum <- rep(1,length(dd$regime))
for(i in a) { 
	if (dd$regime[i-1] != dd$regime[i] || dd$ctryname[i-1]!=dd$ctryname[i]) {
		b <- b + 1
		regimespellnum[i:length(regimespellnum)]<-b
		}
	}
dd <- cbind(dd,regimespellnum)
	
#find democracy transitions
a <- 2:length(dd$democracy) #a counter variable
b <- 1 #another counter
democracyspellnum <- rep(1,length(dd$democracy))
for(i in a) { 
	if (dd$democracy[i-1] != dd$democracy[i] || dd$ctryname[i-1]!=dd$ctryname[i]) {
		b <- b + 1
		democracyspellnum[i:length(democracyspellnum)]<-b
		}
	}
dd <- cbind(dd,democracyspellnum)

#fill in the missing dates of entry of leaders and adjust tenure
library(plyr)
#Create a "leader spell" identifier for each leader/democracy/entry date triad
dd <- ddply(dd,.(leaderspellnum,democracy,ctryname),transform,leaderspelldem = paste(ehead,ctryname,min(year),max(year),democracy,sep="."))
dd <- dd[ order(dd$order), ]

#Add a variable for the end date of the leader spell
dd <- ddply(dd,.(leaderspelldem),transform,enddateleaderdem = max(year))
dd <- dd[ order(dd$order), ]

#Add a variable for the beginning date of the leader spell
dd <- ddply(dd,.(leaderspelldem),transform,begindateleaderdem = ifelse(enddateleaderdem-tenure08+1 < entryy, enddateleaderdem-tenure08+1, min(year)))
dd <- dd[ order(dd$order), ]

#Create a new tenure variable for the democratic leader spell
dd <- transform(dd, tenuredem = enddateleaderdem - begindateleaderdem + 1)

#Create a "leader spell" identifier for each leader/regime/entry date triad
dd <- ddply(dd,.(leaderspellnum,regime,ctryname),transform,leaderspellreg = paste(ehead,ctryname,min(year),max(year),regime,sep="."))
dd <- dd[ order(dd$order), ]

#Same thing for regime2
dd <- ddply(dd,.(leaderspellnum,regime2,ctryname),transform,leaderspellreg2 = paste(ehead,ctryname,min(year),max(year),regime2,sep="."))
dd <- dd[ order(dd$order), ]

#Create tenure variables for leader spells per regime
dd <- ddply(dd,.(leaderspellreg),transform,tenurereg = length(year))
dd <- dd[ order(dd$order), ]

dd <- ddply(dd,.(leaderspellreg2),transform,tenurereg2 = length(year))
dd <- dd[ order(dd$order), ]

#Create left-censoring indicator
dd <- transform(dd, leftcensored = ifelse(edate2 < entryy,TRUE, FALSE)) 

#Add a variable for the entry date of the leader spell for each regime type
dd <- ddply(dd,.(leaderspellreg),transform,edatereg = min(year))
dd <- dd[ order(dd$order), ]

dd <- ddply(dd,.(leaderspellreg2),transform,edatereg2 = min(year))
dd <- dd[ order(dd$order), ]

#set up the right-censoring indicator
dd <- ddply(dd,.(leaderspelldem),transform,rightcensdem = min(ecens08,na.rm=TRUE))
dd <- ddply(dd,.(leaderspellreg),transform,rightcensreg = min(ecens08,na.rm=TRUE))
dd <- dd[ order(dd$order), ]

#Now we create the sample files
ddsurvival.cols <- c("ctryname","cowcode2","politycode","un_region_name","un_continent_name",
					"ehead","democracy","begindateleaderdem","enddateleaderdem","tenuredem","rightcensdem")
ddsurvival <- unique(dd[,ddsurvival.cols])
rownames(ddsurvival) <- 1:nrow(ddsurvival)

ddsurvival2.cols <- c("ctryname","cowcode2","politycode","un_region_name","un_continent_name",
					"ehead","leaderspellreg","democracy","regime","edatereg","tenurereg","rightcensreg")
ddsurvival2 <- unique(dd[,ddsurvival2.cols])
rownames(ddsurvival2) <- 1:nrow(ddsurvival2)

write.csv(ddsurvival,"ddsurvival.csv")
write.csv(ddsurvival2,"ddsurvival2.csv")

#to create ddcoldwar.csv, we need to split the cases that "straddle" 1989, which we take as the end of the cold war. 
#There is probably a much easier and faster way to do this

ddcoldwar.cols <- c("ctryname","cowcode2","politycode","un_region_name","un_continent_name",
					"ehead","democracy","leaderspelldem","begindateleaderdem","enddateleaderdem","tenuredem","rightcensdem")
ddsurvival3 <- unique(dd[,ddcoldwar.cols])

ddsurvival.coldwar <- subset(ddsurvival3,enddateleaderdem <= 1989 & begindateleaderdem > 1945)
ddsurvival.notcoldwar <- subset(ddsurvival3,begindateleaderdem >= 1989)
ddsurvival.coldwar <- transform(ddsurvival.coldwar,coldwar=TRUE)
ddsurvival.notcoldwar <- transform(ddsurvival.notcoldwar,coldwar=FALSE)
straddle.1989 <- subset(ddsurvival3,enddateleaderdem > 1989 & begindateleaderdem < 1989)
straddle.1945 <- subset(ddsurvival3,enddateleaderdem > 1945 & begindateleaderdem < 1946)

before.1989 <- transform(straddle.1989,enddateleaderdem=1989, tenuredem = 1989 - begindateleaderdem + 1, coldwar=TRUE)
after.1989 <- transform(straddle.1989,begindateleaderdem=1990, tenuredem = enddateleaderdem - 1989, coldwar=FALSE)

before.1945 <- transform(straddle.1945,enddateleaderdem=1945, tenuredem = 1945 - begindateleaderdem + 1, coldwar=FALSE)
after.1945 <- transform(straddle.1945,begindateleaderdem=1946, tenuredem = enddateleaderdem - 1945, coldwar=TRUE)

ddcoldwar <- rbind(ddsurvival.coldwar,ddsurvival.notcoldwar,before.1945,after.1945,before.1989,after.1989)
ddcoldwar <- ddcoldwar[ order(ddcoldwar$ctryname,ddcoldwar$begindateleaderdem), ]
rownames(ddcoldwar) <- 1:nrow(ddcoldwar)

write.csv(ddcoldwar,"ddcoldwar.csv")