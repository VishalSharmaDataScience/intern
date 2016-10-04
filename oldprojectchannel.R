setwd("C:\\Users\\inspiron\\Desktop\\RPractice\\Work")
master <- read.csv("masterfinal.csv")
View(master)
usergrade <- master[c(2,38,39,35,21)]
View(usergrade)
View(master)
usergrade <- master[c(2,38,39,35,21,12,17,25)]
usergrade <- usergrade %>% group_by(User.ID) %>% mutate(lastclickdaysbefore = min(Clickdays))
usergrade <- usergrade[-8]
library(dplyr)
usergrade <- usergrade %>% group_by(User.ID) %>% mutate(lastclickdaysbefore = min(Clickdays))
View(usergrade)
usergrade <- usergrade[-8]
View(usergrade)
userge
usergrade <- usergrade %>% group_by(User.ID) %>% mutate (Notpuppy = (Totaltxn/User.since.Days)*365)

hist(usergrade$Totaltxn)
library(ggplot2)
usergrade <- unique(usergrade)
usergrade <- unique(usergrade)
boxplot(subset(usergrade$Totaltxn,usergrade$Totaltxn > 1))
usergrade$Campaign.Name[is.na(usergrade$Campaign.Name),] <- "0"
summary(usergrade$lastclickdaysbefore)
hist(usergrade$Notpuppy)
summary(usergrade$Notpuppy)
plot(usergrade$Notpuppy)
summary(usergrade$Mean.Order.Value1)
boxplot(usergrade$Mean.Order.Value1)
Na <- subset(usergrade , is.na(usergrade$Mean.Order.Value1)
Na <- inner_join(Na,nam)
View(nam)
View(Na)
Na <- subset(Na,is.na(Na$\))
Na <- subset(Na,is.na(Na$Order.Value) == F)
View(Na)
usergrade$Mean.Order.Value1[is.na(usergrade)]
usergrade$Mean.Order.Value1[is.na(usergrade$Mean.Order.Value1)] <- 0

usergrade2 <- usergrade

usergrade$"Statususer" <- ifelse(usergrade$lastclickdaysbefore < 86, "Highly_Active" , ifelse(usergrade$lastclickdaysbefore < 114 , "Active" , ifelse(usergrade$lastclickdaysbefore < 143 , "Inactive" , ifelse(usergrade$lastclickdaysbefore <= 182 , "Lapsed" , 0))))
View(a)
gg <- ggplot(data = usergrade , aes(x = Totaltxn , y = Mean.Order.Value )) + geom_tile()
