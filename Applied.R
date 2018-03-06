library(readxl)
library(sandwich)
library(lmtest)
library(AER)
library(systemfit)
library(tidyverse)
table <- read_excel("~/GitHub/Applied/table.xlsx")
table <- read_excel("C:/Users/Dimitri/Dropbox/[Cours]/Applied/R/applied/table.xlsx")

View(table)

attach(table)

"HousMembers + Age +	NumbRooms +	FloorSpace + 	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income"


View(table)
#deal with missing observations

table <- na.omit(table)
sum(is.na(table))

## OLS 

Lconso <- log(Conso)
modelLog <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income)
summary(modelLog)


model <- lm(Conso  ~ HousMembers + Age +	NumbRooms +	FloorSpace +	Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income)
summary(model)

FS2 <- FloorSpace^2

modelLog2 <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace + FS2 +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	income)
summary(modelLog2)

logInc <- log(income)

modelLog3 <- lm(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc)
summary(modelLog3)

modelLog4 <- (Lconso ~ HousMembers + 	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Frigo	+ Freezer	+ 	Aspirateur +	TV +	VideoRecord +	CD +	logInc)
summary(modelLog4)
modelLog4bis <- (logInc ~ HousMembers + 	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Frigo	+ Freezer	+ 	Aspirateur +	TV +	VideoRecord +	CD +	Lconso)
summary(modelLog4bis)


# de la merde
system <- list(modelLog4bis,modelLog4)
instrument <- ~ HousMembers +   Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc +  Age
fitols <- systemfit(system,method="W2SLS",inst = instrument)
summary(fitols)
modelLog4
print(fitols)



modelIV <- ivreg(Lconso ~ HousMembers + Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc |HousMembers +   Age +	NumbRooms +	FloorSpace  +		Loc2 +	Loc3 +	Loc4 +	Loc5 +	Loc6 +	ElecCookYN + Poele	+ LaveLinge	+ Clim	+ LaveVaisselle	+ Frigo	+ Freezer	+ 	Micronde +	Aspirateur +	FerRepasser +	Antenne +	TV +	VideoRecord +	VideoCamera +	CD +	Radio +	PC +	logInc +  Age)
summary(modelIV)


?systemfit




## ssc instal sem 
## look test for multi








betaIncomeOLS <- coefficients(modelLog3)[1]
varIncomeOLS <- vcov(modelLog3)[1]
betaIncomeIV <- coef()[]
varIncomeIV <- vcov()[1]

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

lmA <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+ElecCookYN+Age+TableYN+HousMembers , data=sampleA)
summary(lmA)

lmB <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+ElecCookYN+Age+TableYN+HousMembers , data=SampleB)
summary(lmB)