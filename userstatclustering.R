new <- stat %>% group_by(User.ID,Store,Status) %>% summarise(n = n())
new <- spread(new,Status,n,fill = as.numeric(0))

new$"req/paid" <- new$Requested + new$Paid
new <- new[-c(5,7)]
new$"total" <- new$`Cancelled ` + new$Confirmed  + new$Pending +  new$`req/paid`
new$`Cancelled ` = (new$`Cancelled `/new$total)
new$Confirmed = (new$Confirmed/new$total)
new$Pending = new$Pending/new$total
new$`req/paid` = new$`req/paid`/new$total



user <- read.csv("users.csv")
ref <- read.csv("Referral Accounts (2).csv")
userref <- merge(user,ref ,by.x = "Referred.by.username" ,by.y = "Username" , all.x  = F ,all.y = F)
userref <- userref[c(3,7)]
userstat <- merge(new,userref,by.x  = "User.ID" ,by.y =  "ID")
View(userstat)
rm(new,userref)
library(ggplot2)
ggplot(userstat ,aes(userstat$`Cancelled ` , userstat$Confirmed ,color = userstat$Campaign.Name)) +geom_point()
distance <- dist(userstat[3:6])
distances <- distance
clusteruserstat = hclust(distances, method = "ward")
plot(clusteruserstat)
clusterGroups = cutree(clusteruserstat, k = 4)
tapply(userstat$`Cancelled `, clusterGroups, mean)
subset(userstat, userstat$User.ID == "973605")
clusterGroups[1]
tapply(userstat$`Pending `, clusterGroups, mean)
tapply(userstat$Pending, clusterGroups, mean)
tapply(userstat$Confirmed, clusterGroups, mean)
a <- table(clusterGroups , userstat$Campaign.Name)
a <- as.data.frame(table(clusterGroups , userstat$Campaign.Name))
