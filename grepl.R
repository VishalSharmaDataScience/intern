
setwd("C:\\Users\\inspiron\\Desktop\\RPractice")

library(dplyr,tidyr)
master <- tbl_df(read.csv("master.csv"))
master
x <- c("Amazon", "Flipkart", "Snapdeal", "Shopclues")
h <- grepl(paste(x, collapse = "|"), master$Transacting.Store)
f <- subset(master,h == TRUE)
i <- grepl("Snapdeal", master$Exit.Store ,TRUE)
master[i,6] <- "Snapdeal"
i <- grepl("Flipkart", master$Exit.Store , ignore.case = TRUE)
master[i,6] <- "Flipkart"
i <- grepl("Amazon", master$Exit.Store , ignore.case = TRUE)
master[i,6] <- "Amazon"
i <- grepl("ShopClues", master$Exit.Store , ignore.case = TRUE)
master[i,6] <- "ShopClues"
i <- grepl("Ebay", master$Exit.Store , ignore.case = TRUE)
master[i,6] <- "Ebay"
i <- grepl("Paytm", master$Exit.Store , ignore.case = TRUE)
master[i,6] <- "Paytm"
stores <- f[-c(1,5,20,19,17)]
stores <- stores[-c(3)]
colnames(stores)[15] <- "store.Name"

a <- stores %>% group_by(User.ID,store.Name) %>% mutate (n = mean(Order.Value))




