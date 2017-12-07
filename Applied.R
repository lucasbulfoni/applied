library(foreign)

datademo = read.spss("~/GitHub/Applied/2002 1 Demography.sav", to.data.frame=TRUE)
datahouse = read.spss("~/GitHub/Applied/2002 2 Housing and durables.sav", to.data.frame=TRUE)

names(datademo)
summary(datahouse)
