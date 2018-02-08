library(readxl)

table <- read_excel("~/GitHub/Applied/table.xlsx")
table <- read_excel("C:/Users/Dimitri/Dropbox/[Cours]/Applied/R/applied/table.xlsx")

View(table)

attach(table)

modelLog <- lm(Lconso ~ income + HousMembers + NumbRooms + FloorSpace + ElecYN + ElecCookYN )
summary(modelLog)
