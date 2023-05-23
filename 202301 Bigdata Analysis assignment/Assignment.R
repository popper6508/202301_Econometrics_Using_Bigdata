source("package_library.R")
#1
ass1 <- as.data.frame(read.csv("sales.csv", sep = " "))

ass1[2] <- ass1[1]
ass1[1] <- row.names(ass1)
row.names(ass1) <- seq(1, nrow(ass1))
colnames(ass1) <- c('ads', 'sales')

ass1$ads <- as.numeric(ass1$ads)
ass1$sales <- as.numeric(ass1$sales)

summary(ass1)
sum((ass1$ads-mean(ass1$ads))*(ass1$sales-mean(ass1$sales)))/sum((ass1$ads-mean(ass1$ads))*(ass1$ads-mean(ass1$ads)))

t.test(anal1$coefficients)

anal1 <- lm(sales ~ ads, data=ass1)
summary(anal1)
stargazer(anal1, type = "text")

X = as.matrix(data.frame(x0 = rep(1,15), ads = ass1$ads))

b = solve(t(X) %*% X) %*% t(X) %*% ass1$sales
b

library(readxl)

##2
ass2 <- as.data.frame(read_excel("umint_kor.xlsx"))
head(ass2)
anal2 <- lm(unemployment ~ interest, data = ass2)
stargazer(anal2, type = "text")


print(summary(anal2))

mean(ass2$interest)


##3
ass3 <- as.data.frame(read.csv("macrodata.csv"))

anal_3_1 <- lm(unemployment ~ interest_rate, data = ass3)

stargazer(anal_3_1, type = "text")
summary(anal_3_1)

anal_3_2 <- lm(unemployment ~ Government_Exapenditure, data = ass3)
summary(anal_3_2)

stargazer(anal_3_2, type = "text")

anal_3_3 <- lm(unemployment ~ interest_rate + Government_Exapenditure, data = ass3)
summary(anal_3_3)
nrow(ass3)
stargazer(anal_3_3, type = "text")

##4
ass4 <- as.data.frame(read.csv("auto.csv"))
head(ass4)

