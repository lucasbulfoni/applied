library(foreign)

table <- read.csv(file=file.choose(),sep=";")
head(table)
attach(table)
summary(table)
na.omit(table$s14_4)
na.omit(table)
names(table) <- c("x","DistCode","SerialNumb","HousMembers","YearBuilt","NumbRooms","FloorSpace","Conso","tableYN","TypeCook","income")

table$conso <- table$conso + 1
table$conso <- log(table$conso)

#  X
#  mesto	DistCode
#  rdb	SerialNumb
#  brojclan	HousMembers
#  s2	YearBuilt
#  s3	NumbRooms
#  s4	FloorSpace
#  s14_4	Conso
#  s9_1	tableY/N
#  
#  income	income


LM1 <- lm()
