# You will need the polity case file, available at
# http://www.systemicpeace.org/inscr/p4v2010d.xls
# For a description of the variables, see the Polity IV
# codebook, available at:
# http://www.systemicpeace.org/inscr/p4manualv2010.pdf
# You will need to save the polity case file as a csv file

polity <- read.csv("p4v2010d.csv")

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
 

#create dates
polity <- transform(polity, dead = TRUE)
polity[ polity$eyear == 9999,] <- transform(polity[ polity$eyear == 9999,], eyear = 2010, emonth = 12, eday = 31, dead = FALSE )

polity <- transform(polity, begindate = as.Date(paste(byear,bmonth,bday,sep="-")))
polity <- transform(polity, enddate = as.Date(paste(eyear,emonth,eday,sep="-")))

# regime duration spell duration
polity <- transform(polity, duration = enddate - begindate)
polity <- transform(polity, durationyrs = as.numeric(duration)/365.25)

# order of cases

order <- 1:nrow(polity)
polity <- cbind(polity, order)
rm(order)

# You will need the "codes.csv" file I've included in the repository
code.table <- read.csv("codes.csv")
polity <- merge(polity,code.table,all.x=TRUE)

#This is the basic survival plot of all regimes
library(survival)
fit.all.regimes <- survfit(Surv(time=durationyrs,event=dead) ~ 1,data=subset(polity, demstatus != "Transitional"))
print(fit.all.regimes)
png("Duration of authority patterns 1800-2010.png")
plot(fit.all.regimes,mark.time=FALSE, xmax=100, xlab="Duration of authority pattern, in years",ylab = "Proportion surviving")
abline(h=0.5,col="red")
title(main="Duration of authority patterns, 1800-2010",sub="Excludes interruptions of state authority (codes -88,-77,-66)")
dev.off()

# The logplot version of it
png("Duration of authority patterns 1800-2010 logplot.png")
plot(fit.all.regimes,mark.time=FALSE, xmax=100, xlab="Duration of authority pattern, in years",ylab = "Proportion surviving", log="xy")
abline(h=0.5,col="red")
title(main="Duration of authority patterns, 1800-2010",sub="Excludes interruptions of state authority (codes -88,-77,-66)")
dev.off()

# Regional distribution of regime survival
fit.by.region.1 <- survfit(Surv(time=durationyrs,event=dead) ~ un_continent_name,data=subset(polity, demstatus != "Transitional"))
print(fit.by.region.1)
png("Duration of authority patterns 1800-2010.png")
plot(fit.by.region.1,mark.time=FALSE, xmax=20, xlab="Duration of authority pattern, in years",ylab = "Proportion surviving", lty=1:5,col=1:5)
abline(h=0.5,col="red")
legend(legend=levels(polity$un_continent_name),lty=1:5,col=1:5, x="topright")
title(main="Duration of authority patterns by region, 1800-2010",sub="Excludes interruptions of state authority (codes -88,-77,-66)")
dev.off()

# We can also plot the duration of competitive electoral vs. non-democratic 
# authority patterns
fit.by.democracy.patterns <- survfit(Surv(time=durationyrs,event=dead) ~ demstatus,data=polity)
print(fit.by.democracy.patterns)
png("Duration of authority patterns by competition 1800-2010.png")
plot(fit.by.democracy.patterns,mark.time=FALSE, xmax=20,xlab="Duration of authority pattern, in years",ylab = "Proportion surviving",lty=1:3,col=1:3)
abline(h=0.5,col="red")
legend(legend=c("Competitive electoral","Non-democratic patterns","Breakdown or transition"),lty=1:3,col=1:3, x="topright")
title(main="Duration of authority patterns, 1800-2010")
dev.off()

# And by region
fit.by.region <- survfit(Surv(time=durationyrs,event=dead) ~ demstatus + un_continent_name,data=polity)
print(fit.by.region)

# But non-democratic authority patterns are of many kinds. In order to 
# estimate the half-lives of broader patterns, we need to recalculate
# the duration per type of executive recruitment mechanism

# calculate exrec duration; first we identify the spell
require(plyr)

polity2 <- polity

polity2 <- polity2[ order(polity2$order), ]
exrecspell <- as.factor(paste(polity2$exrec2,polity2$country))
polity2 <- cbind(polity2,exrecspell)
polity2 <- transform(polity2, exrecspellnum = 1)

b <- 1
for(i in 2:length(exrecspell)) {
      if (polity2$exrecspell[i - 1] != polity2$exrecspell[i]) {
        b <- b + 1
        
        }
      polity2$exrecspellnum[i] <- b
      }

polity2 <- ddply(polity2, .(exrecspellnum), transform, exrecbegin = min(begindate), exrecend = max(enddate))
polity2 <- polity2[ order(polity2$order), ]

polity2 <- transform(polity2, exrecduration = exrecend - exrecbegin)
polity2 <- transform(polity2, exrecdead = ifelse(exrecend == as.Date("2010-12-31"),FALSE,TRUE))
polity2 <- transform(polity2, exrecdurationyrs = as.numeric(exrecduration)/365.25)

polity2 <- unique(polity2[, c("country","exrec","exrecspellnum","exrecspell","exrec2","exrecbegin","exrecend","exrecduration","exrecdurationyrs","exrecdead","un_continent_name")])

# Check cases like this: subset(polity2, exrec2=="Ascription plus election")

fit.by.exrec <- survfit(Surv(time=exrecdurationyrs,event=exrecdead) ~ exrec2,data=polity2[ polity2$exrec > 0 , ])
print(fit.by.exrec)
regtypes <- c("Hereditary monarchy","Hereditary monarchy plus limited elite selection","Limited elite selection","Self-selection","Executive-guided transition", "Ascription plus election","Competitive authoritarian","Competitive Election")
png("Survival of regimes 1800-2010.png")
plot(fit.by.exrec,mark.time=FALSE,lty=1:length(regtypes),col=1:length(regtypes),xlab="Duration in years",ylab="Proportion surviving",xmax=50)
legend(legend=regtypes,x="topright",lty=1:length(regtypes),col=1:length(regtypes))
abline(h=0.5,col="red")
title(main="Survival of regimes, 1800-2010 (Polity IV data)")
dev.off()

# We can also try to compare the length of non-democratic to competitive
# electoral periods

polity2 <- polity
polity2 <- polity2[ order(polity2$order), ]


demspell <- as.factor(paste(polity2$demstatus,polity2$ccode))
polity2 <- cbind(polity2,demspell)
polity2 <- transform(polity2, demspellnum = 1)

b <- 1
for(i in 2:length(demspell)) {
      if (polity2$demspell[i - 1] != polity2$demspell[i]) {
        b <- b + 1
        
        }
      polity2$demspellnum[i] <- b
      }
          
      
polity2 <- ddply(polity2, .(demspellnum), transform, dembegin = min(begindate), demend = max(enddate))
polity2 <- polity2[ order(polity2$order), ]

polity2 <- transform(polity2, demduration = demend - dembegin)
polity2 <- transform(polity2, demdead = ifelse(demend == as.Date("2010-12-31"),FALSE,TRUE))
polity2 <- transform(polity2, demdurationyrs = as.numeric(demduration)/365.25)

polity2 <- unique(polity2[, c("country","demstatus","demspellnum","dembegin","demend","demduration","demdurationyrs","demdead","un_continent_name")])

fit.by.demstatus <- survfit(Surv(time=demdurationyrs,event=demdead) ~ demstatus,data=polity2)
print(fit.by.demstatus)
regtypes <- c("Competitive electoral regime","Non-democratic regime pattern","Transitional or breakdown of authority")
png("Survival of democracies and non democracy 1800-2010.png")
plot(fit.by.demstatus,mark.time=FALSE,lty=1:length(regtypes),col=1:length(regtypes),xlab="Duration in years",ylab="Proportion surviving",xmax=50)
legend(legend=regtypes,x="topright",lty=1:length(regtypes),col=1:length(regtypes))
abline(h=0.5,col="red")
title(main="Survival of regimes, 1800-2010 (Polity IV data)")
dev.off()

# We can also look at patterns of authority by region
fit.by.region2 <- survfit(Surv(time=demdurationyrs,event=demdead) ~ un_continent_name,data=subset(polity2, demstatus != "Competitive electoral regime"))
print(fit.by.region2)

fit.by.region3 <- survfit(Surv(time=demdurationyrs,event=demdead) ~ un_continent_name,data=subset(polity2, demstatus == "Competitive electoral regime"))
print(fit.by.region3)