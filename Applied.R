library(foreign)

datademo <- read.spss("~/GitHub/Applied/2002 1 Demography.sav", to.data.frame=TRUE)
datademo <- datademo[c(1,2,4)] 
names(datademo)

datahouse <- read.spss("~/GitHub/Applied/2002 2 Housing and durables.sav", to.data.frame=TRUE)
datahouse <- datahouse[c(1,2,5,6,7,43,23,59)]
names(datahouse)



dataincome <- read.spss("~/GitHub/Applied/2002 IncomeConsumption.sav", to.data.frame=TRUE)
dataincome <- dataincome[c(1,2,93)]
names(dataincome)

View(datademo)

dim(table)

data <- merge(datademo,datahouse,by.x = c("mesto","rbd") , by.y = c("mesto","rbd"))
data <- merge(data,dataincome,by.x=c("mesto","rbd") , by.y = c("mesto","rbd"))


doublon <- which(duplicated2(data))
table<- data[-doublon]
View(table)
table <- unique(data)

write.csv2(table, file = "table.csv")

attach(table)

lm<-lm(s14_4 ~ brojclan+s2+s3+s4+s14_4+s9_1+s171+income)

summary(lm)
