library(readxl)

table <- read_excel("~/GitHub/Applied/table.xlsx")
table <- read_excel("C:/Users/Dimitri/Dropbox/[Cours]/Applied/R/applied/table.xlsx")

View(table)

attach(table)


modelLog <- lm(Lconso ~ income + HousMembers + NumbRooms + FloorSpace + ElecYN + ElecCookYN )
summary(modelLog)

model <- lm(Conso ~ income + HousMembers + NumbRooms + FloorSpace + ElecYN + ElecCookYN )
summary(model)

FS2 <- FloorSpace^2

modelLog2 <- lm(Lconso ~ income + HousMembers + NumbRooms + FloorSpace + FS2 + ElecYN + ElecCookYN )
summary(modelLog2)

logInc <- log(income)

modelLog3 <- lm(Lconso ~ income + HousMembers + NumbRooms + FloorSpace + FS2 + ElecYN + ElecCookYN + logInc)
summary(modelLog3)

sampleA<-subset(table, DistCode<100)
SampleB<- subset (table, DistCode>=100)

summary(sampleA)
summary(SampleB)

lmA <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+ElecCookYN+Age+TableYN+HousMembers , data=sampleA)
summary(lmA)

lmB <- lm (Lconso ~ FloorSpace+NumbRooms+income+ElecYN+ElecCookYN+Age+TableYN+HousMembers , data=SampleB)
summary(lmB)
