#For reading Archigos; you will need to download both files here:
# http://www.rochester.edu/college/faculty/hgoemans/data.htm

# this is all setup
library(foreign)
archigos <- read.dta("Archigos_v.2.9_tv-Public.dta")

archigos$startdate <- as.Date(archigos$startdate,format="%d/%m/%Y")
archigos$enddate <- as.Date(archigos$enddate,format="%d/%m/%Y")
archigos$eindate <- as.Date(archigos$eindate,format="%d/%m/%Y")
archigos$eoutdate <- as.Date(archigos$eoutdate,format="%d/%m/%Y")

archigos$entry <- factor(archigos$entry, labels=c("Unknown","Regular","Irregular","Foreign Imposition"))

archigos$exit <- factor(archigos$exit,labels=c("Still in power","Unknown","Regular means","Natural causes","Ill health","Suicide","Irregular means","Deposed by another state"))

archigos$exit_tv <- factor(archigos$exit_tv,labels=c("Still in power","Unknown","Regular means","Natural causes","Ill health","Suicide","Irregular means","Deposed by another state"))

labelsexit <- c("Missing","Regular","Popular protest with foreign support","Popular protest without foreign support","Rebel forces with foreign support","Rebel forces without foreign support","Military actors with foreign support", "Domestic military actors without foreign support","Other domestic government actors with foreign support", "Other domestic government actors without foreign support","Foreign force","Assassination by unsupported individual","Power struggle within military short of coup","Other irregular")

archigos$exitcode <- factor(archigos$exitcode,labels=labelsexit)

posttenurelabels  <- c("Lost office in 2004","Still in power","Natural death up to six months after losing office", "No information could be found","OK","Exile","Imprisonment (including house arrest)","Death","Death")

archigos$posttenurefate <- factor(archigos$posttenurefate,labels=posttenurelabels)

archigos$posttenurefate_tv <- factor(archigos$posttenurefate_tv,labels=posttenurelabels)

# a hack due to a slight mistake
levels(archigos$posttenurefate)[9] <- levels(archigos$posttenurefate)[8]
levels(archigos$posttenurefate_tv)[9] <- levels(archigos$posttenurefate_tv)[8]


archigos$gender <- factor(archigos$gender, labels=c("Male","Female"))

archigos$yrdied <- ifelse(archigos$yrdied == -777, NA,archigos$yrdied)

archigos$obsid <- as.factor(archigos$obsid)
archigos$leadid <- as.factor(archigos$leadid)
archigos$idacr <- as.factor(archigos$idacr)
archigos$leader <- as.factor(archigos$leader)

archigos <- transform(archigos, tenure = enddate - startdate)
archigos <- transform(archigos, tenureyrs = as.numeric(tenure, unit="days")/365.25)

archigos <- transform(archigos, dead = ifelse(archigos$exit=="Still in power",FALSE,TRUE))

archigos.simple <- read.dta("Archigos_2.9-Public.dta")

archigos.simple$startdate <- as.Date(archigos.simple$startdate,format="%d/%m/%Y")
archigos.simple$enddate <- as.Date(archigos.simple$enddate,format="%d/%m/%Y")
archigos.simple$eindate <- as.Date(archigos.simple$eindate,format="%d/%m/%Y")
archigos.simple$eoutdate <- as.Date(archigos.simple$eoutdate,format="%d/%m/%Y")

archigos.simple$entry <- factor(archigos.simple$entry, labels=c("Unknown","Regular","Irregular","Foreign Imposition"))

archigos.simple$exit <- factor(archigos.simple$exit,labels=c("Still in power","Unknown","Regular means","Natural causes","Ill health","Suicide","Irregular means","Deposed by another state"))

labelsexit <- c("Missing","Regular","Popular protest with foreign support","Popular protest without foreign support","Rebel forces with foreign support","Rebel forces without foreign support","Military actors with foreign support", "Domestic military actors without foreign support","Other domestic government actors with foreign support", "Other domestic government actors without foreign support","Foreign force","Assassination by unsupported individual","Power struggle within military short of coup","Other irregular")

archigos.simple$exitcode <- factor(archigos.simple$exitcode,labels=labelsexit)

posttenurelabels  <- c("Lost office in 2004","Still in power","Natural death up to six months after losing office", "No information could be found","OK","Exile","Imprisonment (including house arrest)","Death","Death")

archigos.simple$posttenurefate <- factor(archigos.simple$posttenurefate,labels=posttenurelabels)

# a hack
levels(archigos.simple$posttenurefate)[9] <- levels(archigos$posttenurefate)[8]

archigos.simple$gender <- factor(archigos.simple$gender, labels=c("Male","Female"))

archigos.simple$yrdied <- ifelse(archigos.simple$yrdied == -777, NA,archigos.simple$yrdied)

archigos.simple$obsid <- as.factor(archigos.simple$obsid)
archigos.simple$leadid <- as.factor(archigos.simple$leadid)
archigos.simple$idacr <- as.factor(archigos.simple$idacr)
archigos.simple$leader <- as.factor(archigos.simple$leader)

archigos.simple <- transform(archigos.simple, tenure = enddate - startdate)
archigos.simple <- transform(archigos.simple, tenureyrs = as.numeric(tenure, unit="days")/365.25)

archigos.simple <- transform(archigos.simple, dead = ifelse(archigos.simple$exit=="Still in power",FALSE,TRUE))

#Simple survival model
library(survival)
fit.all.leaders <- survfit(Surv(time=tenureyrs,event=dead) ~ 1,data=archigos.simple)
print(fit.all.leaders)
png("Tenure of leaders 1840-2004.png")
plot(fit.all.leaders,mark.time=FALSE, xmax=20, xlab="Years in power",ylab = "Proportion surviving")
abline(h=0.5,col="red")
title(main="Tenure of leaders, 1840-2004")
dev.off()

#Survival by previous times in office
archigos.simple <- transform(archigos.simple, timesinofficefactor = as.factor(ifelse(prevtimesinoffice < 1, "First time", ifelse(prevtimesinoffice < 2,"Once","More than once"))))
fit.by.prev.times <- survfit(Surv(time=tenureyrs,event=dead) ~ timesinofficefactor,data=archigos.simple)
print(fit.by.prev.times)
png("Survival of all leaders by previous times in office since 1840.png")
plot(fit.by.prev.times,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=10,lty=1:3,col=1:3)
legend(legend=levels(archigos.simple$timesinofficefactor),x="topright",lty=1:3,col=1:3)
abline(h=0.5,col="red")
title(main="Survival of all leaders by previous times in office since 1840")
dev.off()

# For merging

code.table <- read.csv("codes.csv")

#
#Adding codes
archigos.simple <- merge(archigos.simple, code.table, all.x=TRUE)
archigos <- merge(archigos, code.table, all.x=TRUE)

#Distribution of tenures by country
with(archigos.simple, table( idacr, cut( tenureyrs, breaks = quantile(tenureyrs))))

#Distribution of tenures by region
with(archigos.simple, table( un_region_name, cut( tenureyrs, breaks = quantile(tenureyrs))))

# Survival by region
fit.by.region <- survfit(Surv(time=tenureyrs,event=dead) ~ un_continent_name,data=archigos.simple)
print(fit.by.region)
plot(fit.by.region,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=20,lty=1:5,col=1:5)
legend(legend=levels(archigos.simple$un_continent_name),x="topright",lty=1:5,col=1:5)
abline(h=0.5)
title(main="Survival of all leaders by region since 1800, ARCHIGOS dataset")

# Merging with polity; a lot that follows is pure setup
# You will need he polity data in the same directory
polity <- read.csv("p4v2010.csv")

#create factors
polity <- subset(polity, !is.na(exrec))

#create factors
polity <- transform(polity, exrec2 = exrec, polcomp2 = polcomp, exconst2 = exconst)

polity$exrec2 <- factor(polity$exrec2, levels = c(-88,-77,-66, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("Transition","Interregnum","Interruption","Hereditary monarchy","Hereditary monarchy plus limited elite selection","Limited elite selection","Self-selection","Executive-guided transition", "Ascription plus election","Unfair election","Competitive Election"))

polity$polcomp2 <- factor(polity$polcomp2, levels = c(-88, -77, -66, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("Transition","Interregnum","Interruption","Suppressed","Restricted","Imposed transition","Uninstitutionalized","Transition from uninstitutionalized","Factional restricted","Factional","Electoral transition - persistent conflict","Electoral transition - limited conflict","Institutionalized electoral"))

polity$exconst2 <- factor(polity$exconst2, levels = c(-88, -77, -66, 1, 2, 3, 4, 5, 6, 7), labels=c("Transition","Interregnum","Interruption","Unlimited","Intermediate between unlimited and moderate","Slight to moderate limitations", "Intermediate between moderate and substantial","Substantial","Intermediate between substantial and parity or subordination","Executive parity or subordination"))

# Create a full regime description string
polity <- transform(polity, regdescription = paste(exrec2,polcomp2,exconst2,sep="."))

# Create a dichotomous democracy/non-democracy regime variable
polity <- transform(polity, demstatus = ifelse(exrec < 0, "Transitional",ifelse(exrec < 8, "Non-democratic authority pattern","Competitive electoral regime")))



archigos <- merge(archigos, polity, all.x=TRUE)
 
archigos <- archigos[ order( archigos$leadid ), ]

archigos <- transform(archigos, yearbegin = as.Date(paste(year,"1","1",sep="-")))
archigos <- transform(archigos, yearend = as.Date(paste(year,"12","31",sep="-")))

select.cols <- c("obsid","leadid","year","ccname","leader","exrec","exrec2","tenureyrs","prevtimesinoffice","startdate","enddate","yearbegin","yearend","un_continent_name","exitcode") 

archigossurvival <- unique(archigos[, select.cols])

require(plyr)

archigossurvival <- ddply(archigossurvival, .(obsid,leadid,exrec2), transform, startdate2 = ifelse(startdate < min(yearbegin), min(yearbegin), startdate))

archigossurvival <- archigossurvival[ order( archigossurvival$leadid, archigossurvival$year ), ]

archigossurvival <- ddply(archigossurvival, .(obsid,leadid,exrec2), transform, endate2 = ifelse(enddate > max(yearend), max(yearend), enddate))

archigossurvival <- archigossurvival[ order( archigossurvival$leadid, archigossurvival$year ), ]

archigossurvival <- transform(archigossurvival, tenureyrs2 = (endate2 - startdate2)/365.25)

archigossurvival <- transform(archigossurvival, dead = ifelse(endate2 == max(enddate),FALSE,TRUE))

select.cols <- c("obsid","leadid","ccname","leader","exrec","exrec2","tenureyrs2","prevtimesinoffice","startdate2","endate2","un_continent_name","dead","exitcode") 

archigossurvival <- unique(archigossurvival[, select.cols])

# A trick to check
# repeated <- subset(count(archigossurvival,"leadid"),freq>1)
# head(subset(archigossurvival, (leadid %in% repeated$leadid)),20)

# Distribution of exit by tenure
with(archigossurvival, table( exitcode, cut( tenureyrs2, breaks = quantile(tenureyrs2))))

# Distribution of exit by regime
with(archigossurvival, table( exitcode, exrec2))

# Survival by regime
fit.by.regime <- survfit(Surv(time=tenureyrs2,event=dead) ~ exrec2,data=subset(archigossurvival,exrec > 0))
print(fit.by.regime)
png("Survival of all leaders by regime type since 1840.png")
plot(fit.by.regime,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=25,lty=1:8,col=1:8)
legend(legend=c("Hereditary monarchy","Hereditary monarchy plus limited elite selection","Limited elite selection","Self-selection","Executive-guided transition", "Ascription plus election","Competitive authoritarian","Competitive Election"),x="topright",lty=1:8,col=1:8)
abline(h=0.5)
title(main="Survival of all leaders by regime type since 1840")
dev.off()



fit.by.regime.region <- survfit(Surv(time=tenureyrs2,event=dead) ~ exrec2 + un_continent_name,data=subset(archigossurvival,exrec > 0))
print(fit.by.regime.region)

select.cols <- c("obsid","year","leadid","ccname","leader","demstatus","tenureyrs","prevtimesinoffice","exitcode","startdate","enddate","yearbegin","yearend","un_continent_name")

archigossurvival2 <- unique(archigos[, select.cols])

archigossurvival2 <- ddply(archigossurvival2, .(obsid,leadid,demstatus), transform, startdate2 = ifelse(startdate < min(yearbegin), min(yearbegin), startdate))

archigossurvival2 <- archigossurvival2[ order( archigossurvival2$leadid, archigossurvival2$year ), ]

archigossurvival2 <- ddply(archigossurvival2, .(obsid,leadid,demstatus), transform, endate2 = ifelse(enddate > max(yearend), max(yearend), enddate))

archigossurvival2 <- archigossurvival2[ order( archigossurvival2$leadid, archigossurvival2$year ), ]

archigossurvival2 <- transform(archigossurvival2, tenureyrs2 = (endate2 - startdate2)/365.25)

archigossurvival2 <- transform(archigossurvival2, dead = ifelse(endate2 == max(enddate),FALSE,TRUE))

select.cols <- c("obsid","leadid","ccname","leader","demstatus","tenureyrs2","prevtimesinoffice","exitcode","startdate","enddate","dead","un_continent_name")

archigossurvival2 <- unique(archigossurvival2[, select.cols])

#Survival by region
fit.by.regime.region <- survfit(Surv(time=tenureyrs2,event=dead) ~ demstatus + un_continent_name,data=subset(archigossurvival2,demstatus == "Non-democratic authority pattern"))
print(fit.by.regime.region)
png("Survival of autocratic leaders by region since 1840.png")
plot(fit.by.regime.region,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:5,col=1:5)
legend(legend=levels(archigossurvival2$un_continent_name),x="topright",lty=1:5,col=1:5)
abline(h=0.5)
title(main="Survival of autocratic leaders by region since 1840")
dev.off()

fit.by.regime.region.2 <- survfit(Surv(time=tenureyrs2,event=dead) ~ demstatus + un_continent_name,data=subset(archigossurvival2,demstatus == "Competitive electoral regime"))
print(fit.by.regime.region.2)
png("Survival of democratic leaders by region since 1840.png")
plot(fit.by.regime.region.2,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=20,lty=1:5,col=1:5)
legend(legend=levels(archigossurvival2$un_continent_name),x="topright",lty=1:5,col=1:5)
abline(h=0.5)
title(main="Survival of leaders in competitive regimes by region since 1840")
dev.off()

# Survival by regime
fit.by.demstatus <- survfit(Surv(time=tenureyrs2,event=dead) ~ demstatus,data=archigossurvival2)
print(fit.by.demstatus)
png("Survival of all leaders by regime type since 1840.png")
plot(fit.by.demstatus,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=15,lty=1:3,col=1:3)
legend(legend=c("Competitive electoral","Non-democratic","Transitional"),x="topright",lty=1:3,col=1:3)
abline(h=0.5)
title(main="Survival of all leaders by regime type since 1840")
dev.off()



