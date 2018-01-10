library(foreign)

datademo <- read.spss("~/GitHub/Applied/2002 1 Demography.sav", to.data.frame=TRUE)
datademo <- c(datademo$rbd, datademo$brojclan , datademo$a7 )

datahouse <- read.spss("~/GitHub/Applied/2002 2 Housing and durables.sav", to.data.frame=TRUE)
datahouse <- c(datahouse$rbd , datahouse$s2 , datahouse$s3 , datahouse$s14_4(y))

dataincome <- read.spss("~/GitHub/Applied/2002 IncomeConsumption.sav", to.data.frame=TRUE)
dataincome <-c(dataincome$rbd , dataincome$income )
dataincome$rbd
class(dataincome)
class(datahouse)
datademo$rbd
data <- merge(datademo,datahouse,by.x = "rbd" , by.y = "rbd")
data <- merge(data,dataincome,by.x=data$rbd , by.y = dataincome$rdb)

names(datademo)
summary(datahouse)