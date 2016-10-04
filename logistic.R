cb1 <- master[c(1,5,8,9,10,11,12,13,23,22,20,24)]
cb1 <- subset(cb1 , cb1$Commision.From.Network > 0)

cb <- cb1

cb$"Commisionpercent" <- (cb$Commision.From.Network/cb$Order.Value)*100
pending <- subset(cb , cb$Status == "Pending")
cbnpending <- subset(cb , cb$Status != "Pending")
View(pending)
View(master)
View(cbnpending)
View(cbnpending)
cbnpending$"Cancelled" <- ifelse(cbnpending$Status == "Cancelled ",1,0)

require(caTools)
set.seed(101)
sample = sample.split(cbnpending$Cancelled, SplitRatio = .75)
train = subset(cbnpending, sample == TRUE)
test = subset(cbnpending, sample == FALSE)
View(train)

model <- glm(Cancelled ~ clickday + Order.Value + Commision.From.Network, data = train , family = binomial(link = "logit") )

summary(model)
anova(model, test="Chisq")
View(test)
fitted.results <- predict(model,newdata=subset(test,select=c(3,13)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Cancelled)
print(paste('Accuracy',1-misClasificError))
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(3,13)), type="response")
pr <- prediction(p, test$Cancelled)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

