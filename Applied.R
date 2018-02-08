library(foreign)



tab <- read_excel("~/GitHub/Applied/table.xlsx")


head(tab)
attach(tab)
summary(tab)
na.omit(tab)
View(tab)
tab$conso <- log(tab$conso)



tab$YearBuilt <- tab$YearBuilt - 1800
summary(tab)



