setwd("C:\\Users\\inspiron\\Desktop\\RPractice")
u <- read.csv("users.csv")
e <- read.csv("exit.csv")
c <- read.csv("cb.csv")
library(dplyr)

master <- merge(e,c , by.x = "ExitID", by.y = "Exit.ID", all.x = T ,all.y = F)
master2 <- merge (master , u , by.x = "User.ID" , by.y = "ID")



master <- merge(e,c , by.x = "ExitID", by.y = "Exit.ID", all.x = T ,all.y = F)
rm(m3)
master2 <- merge (master , u , by.x = "User.ID" , by.y = "ID")
master2 <- merge(master , u , by.x = "User.ID.x" , by.y = "ID")
View(master2)
master <- master2[-c(3,10,12,20)]
View(master)
write.csv(master,master.csv)
write.csv(master,"master.csv")
rm(master2)
table(master$Gender)
13887/104541
master <- master[c(1,2,9,3,10,4,5,11,12,13,14,6,7,15,16,17,20,8,19)]
View(master)
colnames(master)[1] <- "User.ID"
colnames(master)[4] <- "Exit.Store"
colnames(master)[5] <- "Transacting.Store"
colnames(master)[15] <- "Transacting.Day"
colnames(master)[17] <- "User.since.Days"
View(u)
View(master)
write.csv("master,"master.csv)
write.csv("master,"master.csv")
write.csv(master,"master.csv")
