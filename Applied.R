# Package
load.libraries <- c('tidyverse', 'readxl', 'foreign','sandwich', 'lmtest','AER')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE, repos = "https://cloud.r-project.org")
sapply(load.libraries, require, character = TRUE)



# Preparing Data Base
"We took data from 4 differents table"
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

"We merge all the table, delete missing values, delete duplicate observation"
data <- merge(datademo,datahouse,by.x = c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- merge(data,dataincome,by.x=c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- merge(data,dataLabor,by.x=c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- na.omit(data)
sum(is.na(data))
table2 <- unique(data)
View(table2)

"Create the data base we use"
write.csv2(table2, file = "table.csv")


"Differente ways to download the table"
table <- read_excel("~/GitHub/Applied/table.xlsx")
table <- read_excel("C:/Users/Dimitri/Dropbox/[Cours]/Applied/R/applied/table.xlsx")
table <- read_excel(file = file.choose())
dim(table)
"Information about data base"
View(table)
dim(table)
summary(table)


# Outlier 
"create a table with cooks distance"
infl = lm.influence(modellog2, do.coef = FALSE)
cd <- cooks.distance(modellog2, infl = infl,
                     res = weighted.residuals(modellog2),
                     sd = sqrt(deviance(modellog2)/df.residual(modellog2)),
                     hat = infl$hat)

write.csv2(cd, file = "cook.csv")

"taking a ceilling value egual to 4 times the mean of distcook"
seuil <- 7.068e-04
"Take a subset without the outliers"
table <- subset(table, distcook< seuil)


dim(table)
attach(table)
sink("DE09.txt")



## OLS 

Lconso <- log(Conso)
FS2 <- FloorSpace^2
logInc <- log(income)


modelLog1 <- lm(Lconso ~ 	logInc + HousMembers +	NumbRooms +	FloorSpace +	Heating + Age + Frigo + LaveLinge)
summary(modelLog1)

modellog2 <- lm(Lconso ~ logInc + HousMembers + NumbRooms + FloorSpace + Heating + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Age + Frigo + LaveLinge)
summary(modellog2)



##2SLS

lm2sls_1 <- lm(logInc ~ HousMembers + NumbRooms + FloorSpace
               + Heating + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6
               + Frigo +Statut + Urban + LaveLinge + PC)
summary(lm2sls_1)
logIncfit<- fitted(lm2sls_1)         


lm2sls_2 <- lm(Lconso ~ logIncfit + HousMembers + NumbRooms + FloorSpace
               + Heating + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo +  LaveLinge)
summary(lm2sls_2)

# IV


modelIV <- ivreg(Lconso ~ logInc +  HousMembers + FloorSpace + Heating
                 + NumbRooms + Age + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo + LaveLinge
                 | HousMembers + NumbRooms + FloorSpace + Heating + Age 
                 + Loc2 + Loc3 + Loc4 + Loc5 +Loc6 + Frigo + LaveLinge + Urban + Statut + PC)

summary(modelIV, vcoc=sandwich, diagnostics=TRUE)


# Hausman Test

betaIncomeOLS <- coefficients(modellog2)[2]
varIncomeOLS <- vcov(modellog2)[2,2]
betaIncomeIV <- coef(lm2sls_2)[2]
varIncomeIV <- vcov(lm2sls_2)[2,2]

hausman <- (betaIncomeOLS - betaIncomeIV )/ sqrt(varIncomeIV - varIncomeOLS)
abs(hausman)



# Goldfeld-Quant
gqtest(modelLog1)
gqtest(modellog2)
gqtest(lm2sls_2)


sink()



