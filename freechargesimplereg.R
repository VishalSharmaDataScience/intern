setwd("C:\\Users\\inspiron\\Desktop\\RPractice\\Work")
master2 <- master2[-1]

linear <- lm(b$n.y ~ b$n.x )
summary(linear)
linear$residuals
prediction <- predict(linear, b)
SSE = sum((b$n.y - prediction)^2)
SST = sum((b$n.y - mean(b$n.y))^2 )
1- SSE/SST
abline(linear)




