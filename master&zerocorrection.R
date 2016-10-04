save.image("C:/Users/inspiron/Desktop/RPractice/Work/userlogistics.RData")
load("C:/Users/inspiron/Desktop/RPractice/Work/userlogistics.RData")
View(refusers)
refusers2 <- refusers[c(1,2,4,5)]
View(refusers2)
colnames(refusers2) <- "User.ID"
library(dplyr)
refusers2 <- refusers[c(1,2,4,5)]
colnames(refusers2)[4] <- "User.ID"
View(masteruser)
masteruser <- merge(masteruser, refusers2)
View(refusers2)
View(masteruser)
View(masteruser)
View(master)
View(master)
a <- master %>% group_by(User.ID,Store) %>% summarise( meanOv = mean(Order.Value))
View(a)
a <- subset(a , is.na(a) == F)
View(a)
a <- master %>% group_by(User.ID,Store) %>% summarise( meanOv = mean(Order.Value))
a <- subset(a , is.na(a$meanOv) == F)
master2 <- merge(master, a , all.x = T,  by = c("User.ID","Store"))
View(master2)
colnames(master2)[31] <- "MeanOVStore"
txn <- master2[c(1,2,27)]
View(txn)
txn <- unique(txn)
total <- txn %>% group_by(User.ID) %>% mutate(totaltxn = sum(Txn\))
total <- txn %>% group_by(User.ID) %>% mutate(totaltxn = sum(Txn.Count.Store))
View(total)
total <- total[1,4]
total <- txn %>% group_by(User.ID) %>% mutate(totaltxn = sum(Txn.Count.Store))
total <- total[c(1,4)]
total <- unique(total)
master2 <- merge(master2 , total , by = "User.ID")
View(a)
rm (txn , total , a)
rm(tet , train)
rm(test , train)
boxplot(master2$totaltxn)
summary(master2$totaltxn)
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = sum(Order.Value))
View(user)
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = sum(Order.Value),na.rm = T)
rm(user)
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = sum(Order.Value,na.rm = T))
View(user)
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = mean(Order.Value,na.rm = T))
user <- user %>% group_by(User.ID,Store) %>% mutate (MeanOVStore = mean(Order.Value,na.rm = T))
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = mean(Order.Value,na.rm = T))
user <- user %>% group_by(User.ID,Store) %>% mutate (MeanOVStore = mean(Order.Value,na.rm = T))
View(user)
rm(user)
View(master2)
View(master2)
master2 <- master2[-31]
View(master2)
user <- master2 %>% group_by(User.ID) %>% mutate (MeanOV = mean(Order.Value,na.rm = T))
View(user)
rm(master,master2)
View(user)
write.csv(user, "master2.csv")
user2 <- user %>% group_by(User.ID,Status == "Cancelled" ,) %>% mutate (MeanOVcancelled = mean(Order.Value,na.rm = T))
user2 <- user %>% group_by(User.ID,Status == "Cancelled " ,) %>% mutate (MeanOVcancelled = mean(Order.Value,na.rm = T))
user2 <- user %>% group_by(User.ID,Status == "Cancelled ") %>% mutate (MeanOVcancelled = mean(Order.Value,na.rm = T))
View(user2)
colnames()[34] <- "MeanOVStatus/Cancelled"
colnames(user2)[34] <- "MeanOVStatus/Cancelled"
library(tidyr)
user <- user2
user <- spread(user, user$`Status == "Cancelled "`, user$`MeanOVStatus/Cancelled`)
user2 <- user %>% group_by(User.ID) %>% mutate(meanCommission = mean(Commision.From.Network,na.rm = T))
usergrade <- user2[1,35,32,31]
write.csv(user2,"master2.csv")
usergrade <- user2[c(1,35,32,31)]
View(usergrade)
usergrade <- unique(usergrade)
View(usergrade)
library(rpart)
library(rpart.plot)
tree <- rpart(totaltxn ~ MeanOv + meanCommission, usergrade )
tree <- rpart(totaltxn ~ MeanOV + meanCommission, usergrade )
prp(tree)
zero <- subset(user2 , user2$Order.Value == 0)
View(zero)
View(user2)
user <- user2
View(zero)
user <- user %>% group_by(User.ID,Store) %>% mutate(zerostoretxn = sum(Transact))
user <- user[-36]
zero <- zero %>% group_by(User.ID,Store) %>% mutate(zerostoretxn = sum(Transact))
View(zero)
zero <- zero %>% group_by(User.ID) %>% mutate(zerototaltxn = sum(Transact))
View(zero)
zero <- zero[c(1,2,36,37)]
View(zero)
zero <- unique(zero)
View(zero)
user2 <- merge(user2 , zero , by = c("User.ID","Store"), all.x =  T)
View(user2)
user <- user2
zerousers <- user2[1,2,27,36,31,37]
zerousers <- user2[c(1,2,27,36,31,37)]
View(zerousers)
zerousers <- subset(zerousers , is.na(zerousers$zerostoretxn) = F)
zerousers <- subset(zerousers , is.na(zerousers$zerostoretxn) == F)
zerousers <- unique(zerousers)
View(zerousers)
zerousers$Txn.Count.Store[is.na(zerousers$Txn.Count.Store)] <- 0
zerousers$totaltxn[is.na(zerousers$totaltxn)] <- 0
zerousers$Txn.Count.Store <- zerousers$Txn.Count.Store + zerousers$zerostoretxn
zerousers$totaltxn <- zerousers$totaltxn + zerousers$zerototaltxn
View(zero)
View(user2)
user2$zerostoretxn[is.na(user2$zerostoretxn)] <- 0
user2$zerototaltxn[is.na(user2$zerototaltxn)] <- 0
user2$totaltxn[is.na(user2$totaltxn)] <- 0
table(is.na(user2$Txn.Count.Store))
user2$Txn.Count.Store[is.na(user2$Txn.Count.Store)] <- 0
table(is.na(user2$Txn.Count.Store))
table(is.na(user2$zerototaltxn)
)
table(is.na(user2$zerostoretxn))
table(is.na(user2$Txn.Count.Store))
table(is.na(user2$totaltxn))
user2$totaltxn <- user2$totaltxn + user2$zerototaltxn
user2$Txn.Count.Store <- user2$zerostoretxn + user2$Txn.Count.Store
a <- subset(user2 , user2$totaltxn < user2$Txn.Count.Store)
View(a)
user2 <- user2[-31]
View(user2)
user3 <- user2 %>% group_by(User.ID) %>% mutate(Totaltxn = sum(Transact))
View(user3)
a <- subset(user3 , user3$totaltxn < user3$Txn.Count.Store)
a <- subset(user3 , user3$Totaltxn < user3$Txn.Count.Store)
write.csv(user3, "masterfinal.csv")
