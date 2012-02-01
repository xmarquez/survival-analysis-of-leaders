ddsurvival <- read.csv("ddsurvival.csv") #tenure of leaders by democracy/non-democracy
ddsurvival2 <- read.csv("ddsurvival2.csv") #tenure of authoritarian leaders by regime type
ddcoldwar <- read.csv("ddcoldwar.csv")

library(survival)

# Survival estimates for all leaders, all regimes
fit.all.leaders <- survfit(Surv(time=tenure08,event=ecens2) ~ 1,data=ddsurvival)
png("All leaders.png")
plot(fit.all.leaders,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE)
title(main="Survival of all leaders since 1946")
dev.off()

# Survival estimates for all leaders, all regimes, log scale
png("All leaders logplot.png")
plot(fit.all.leaders,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,log="xy")
title(main="Survival of all leaders since 1946, log scale")
dev.off()

# Survival estimates for leaders in democracies and non-democracies
fit.by.democracy <- survfit(Surv(time=tenure08,event=ecens2) ~ democracy,data=ddsurvival)
png("Leaders by democracy.png")
plot(fit.by.democracy,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:2,col=1:2)
legend(legend=levels(ddsurvival$democracy),x="topright",lty=1:2,col=1:2)
title(main="Survival of leaders by regime type since 1946")
dev.off()

png("Leaders by democracy logplot.png")
plot(fit.by.democracy,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:2,col=1:2,log="xy")
legend(legend=levels(ddsurvival$democracy),x="topright",lty=1:2,col=1:2)
title(main="Survival of leaders by regime type since 1946, log plot")
dev.off()

# Survival estimates in different non-democratic regimes
fit.by.regime2 <- survfit(Surv(time=regtenure,event=ecens2) ~ regime2,data=ddsurvival2)
png("Leaders by regime type.png")
plot(fit.by.regime2,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:4,col=1:4)
legend(legend=levels(ddsurvival2$regime2),x="topright",lty=1:4,col=1:4)
title(main="Survival of leaders by authoritarian regime type since 1946")
dev.off()

# Survival estimates of dictators by region
fit.by.region <- survfit(Surv(time=tenure08,event=ecens2) ~ un_continent_name,data=subset(ddsurvival,democracy=="Non-democracy"))
png("Dictator survival by region.png")
plot(fit.by.region,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:5,col=1:5)
legend(legend=levels(ddsurvival$un_continent_name),x="topright",lty=1:5,col=1:5)
title(main="Survival of authoritarian leaders by region since 1946")
dev.off()

# Survival estimates of democratic leaders by region
fit.by.region <- survfit(Surv(time=tenure08,event=ecens2) ~ un_continent_name,data=subset(ddsurvival,democracy=="Democracy"))
png("Democrat survival by region.png")
plot(fit.by.region,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=40,lty=1:5,col=1:5)
legend(legend=levels(ddsurvival$un_continent_name),x="topright",lty=1:5,col=1:5)
title(main="Survival of democratic leaders by region since 1946")
dev.off()

# Survival estimates of dictators during and after the cold war
fit.by.coldwar <- survfit(Surv(time=tenure08,event=ecens2) ~ coldwar,data=subset(ddcoldwar,democracy=="Non-democracy"))
fit.by.coldwar.dem <- survfit(Surv(time=tenure08,event=ecens2) ~ coldwar,data=subset(ddcoldwar,democracy=="Democracy"))
png("Autocrat survival during and after the cold war.png")
plot(fit.by.coldwar,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=20,lty=1:2,col=1:2)
title(main="Survival of autocrats during and after the cold war")
legend(legend=c("After the cold war","During the cold war"),x="topright",lty=1:2,col=1:2)
dev.off()

png("Democrat survival during and after the cold war.png")
plot(fit.by.coldwar.dem,xlab="Years in power",ylab="Proportion surviving",mark.time=FALSE,xmax=20,lty=1:2,col=1:2)
title(main="Survival of democratic leaders during and after the cold war")
legend(legend=c("After the cold war","During the cold war"),x="topright",lty=1:2,col=1:2)
dev.off()