library(readxl)
library(foreign)
library(sandwich)
library(lmtest)
library(AER)
library(systemfit)
library(tidyverse)

datademo <- read.spss("~/GitHub/Applied/2002 1 Demography.sav", to.data.frame=TRUE)
datademo <- datademo[c(1,2,4)] 
names(datademo)

datahouse <- read.spss("~/GitHub/Applied/2002 2 Housing and durables.sav", to.data.frame=TRUE)
datahouse <- datahouse[c(1,2,5,6,7,43,23,59,81,85,89,93,97,101,105,109,121,141)]
names(datahouse)



dataincome <- read.spss("~/GitHub/Applied/2002 IncomeConsumption.sav", to.data.frame=TRUE)
dataincome <- dataincome[c(1,2,93)]
names(dataincome)

dataLabor <- read.spss("~/GitHub/Applied/2002 7 Labor activity.sav", to.data.frame=TRUE)
dataLabor <- dataLabor[c(1,2,8,56)]

names(dataLabor)



data <- merge(datademo,datahouse,by.x = c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- merge(data,dataincome,by.x=c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- merge(data,dataLabor,by.x=c("mesto","rbd") , by.y = c("mesto","rbd"))
View(data)
data <- na.omit(data)
sum(is.na(data))
#doublon <- which(duplicated2(data))
#table<- data[-doublon]
#View(table)
table2 <- unique(data)
View(table2)
write.csv2(table2, file = "table.csv")




table <- read_excel("~/GitHub/Applied/table.xlsx")
table <- read_excel("C:/Users/Dimitri/Dropbox/[Cours]/Applied/R/applied/table.xlsx")

View(table)

attach(table)

"HousMembers + Age +	NumbRooms +	FloorSpace + 	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income"

dim(table)
View(table)
#deal with missing observations

table <- na.omit(table)
sum(is.na(table))
names(table)
## OLS 

Lconso <- log(Conso)
modelLog <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income)
summary(modelLog)


model <- lm(Conso  ~ HousMembers + Age +	NumbRooms +	FloorSpace +	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	TV + PC +	income)
summary(model)

FS2 <- FloorSpace^2

modelLog2 <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace +	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	TV + PC +	income + FS2)
summary(modelLog2)

logInc <- log(income)

modelLog3 <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace +	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ income )
summary(modelLog3)

modelLog4 <- lm(Lconso ~ HousMembers + Age + NumbRooms +	FloorSpace +	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ +	Aspirateur +	logInc )
summary(modelLog4)

modelLog4bis <- lm(logInc ~ HousMembers + 	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Frigo	+ Freezer	+ 	Aspirateur +	TV +	VideoRecord +	CD +	Lconso)
summary(modelLog4bis)

plot(fitted(modelLog4),residuals(modelLog4))

modelLog5 <- lm(Lconso ~ HousMembers +	NumbRooms +	FloorSpace +	Heating  +	logInc)
summary(modelLog5)

modellog6 <- lm(Lconso ~ logInc + HousMembers + NumbRooms + FloorSpace + Heating + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Age + Frigo)
summary(modellog6)
# de la merde
system <- list(modelLog4bis,modelLog4)
instrument <- ~ Staut + Urban 
fitols <- systemfit(modelLog4,method="2SLS",inst = instrument)
summary(fitols)
summary(modelLog4)
print(fitols)

summary(table)

modelIV <- ivreg(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc |HousMembers +   Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	Heating + LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc +  Age)
summary(modelIV)
modelIV2 <- ivreg(Lconso ~ logInc +  HousMembers + NumbRooms + FloorSpace + Heating + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo| HousMembers + NumbRooms + FloorSpace + Heating + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo + Statut + Urban + TV + PC + LaveVaisselle + LaveLinge)
summary(modelIV2)
?systemfit

##2SLS

lm2sls_1 <- lm(logInc ~ Statut + Urban + TV + PC + LaveVaisselle + LaveLinge)
summary(lm2sls_1)
logIncfit<- fitted(lm2sls_1)
lm2sls_2 <- lm(Lconso ~ logIncfit + HousMembers + NumbRooms + FloorSpace + Heating + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo )
summary(lm2sls_2)
## ssc instal sem 
## look test for multi








betaIncomeOLS <- coefficients(modellog6)[1]
varIncomeOLS <- vcov(modellog6)[1]
betaIncomeIV <- coef(lm2sls_2)[]
varIncomeIV <- vcov(lm2sls_2)[1]

hausman <- (betaIncomeOLS - betaIncomeIV )/ sqrt(varIncomeIV - varIncomeOLS)
hausman
l <- gqtest(modelLog4)
l


vcoc <- vcovHC(modelLog3)
a <- coeftest(modelLog3,vcoc)
a
coeftest(modelLog3)

## IV


cov(income,NumbRooms)/ (sd(income)*sd(NumbRooms))




## 2 samples
sampleA<-subset(table, DistCode<100)
SampleB<- subset (table, DistCode>=100)

summary(sampleA)
summary(SampleB)

lmA <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+Heating+Age+TableYN+HousMembers , data=sampleA)
summary(lmA)

lmB <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+Heating+Age+TableYN+HousMembers , data=SampleB)
summary(lmB)