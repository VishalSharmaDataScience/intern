
df <- read.csv("sampled-cashback.csv")
str(df)

head(df)
View(df)
actual <- subset(df, df$Commision.From.Network > 0 & df$Refferd.By == 0)

amazon <- subset(actual,as.Date(df$Date.Asked) < 29-02-2016 & df$Details == "Amazon")

model.amazon <- lm(amazon$Cashback.Value~Order.Value, data = amazon)
summary(model.amazon)
attach(amazon)
plot(Order.Value,Cashback.Value)
x <- 10000
x.df <- data.frame(x)
x <- predict(model.amazon, x.df, interval =  "predict")

summary(x)
