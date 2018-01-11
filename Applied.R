library(foreign)

datademo <- read.spss("~/GitHub/Applied/2002 1 Demography.sav", to.data.frame=TRUE)
datademo <- datademo[c(2,4,13)] 
names(datademo)

datahouse <- read.spss("~/GitHub/Applied/2002 2 Housing and durables.sav", to.data.frame=TRUE)
datahouse <- datahouse[c(2,5,6,7,43,23,59)]
names(datahouse)



dataincome <- read.spss("~/GitHub/Applied/2002 IncomeConsumption.sav", to.data.frame=TRUE)
dataincome <- dataincome[c(2,93)]
names(dataincome)


summary(datahouse)

data <- merge(datademo,datahouse,by.x = "rbd" , by.y = "rbd")
data <- merge(data,dataincome,by.x="rbd" , by.y = "rbd")

names(datademo)
