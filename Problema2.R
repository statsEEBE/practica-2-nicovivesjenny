#Codigo para problema 2

mis_dades <- iris
x <- mis_dades$Petal.Length
mean(x)
sd(x)
hist(x)

y <- mis_dades$Sepal.Length
y
mean(y)
mean(x)

plot(x, y)

m <- sum((x-mean(x))*(y-mean(y)))/((x-mean(x))^2)
m #0,4
b <- mean(y)-m*mean(x)
b #4,3

m*1.5+b

###

summary(lm(y~x))   ###summary para mas decimales, mod para tabla

mod <- lm(y~x)
mod

data.frame(x=1.5)
ypred <- predict(mod, data.frame(x=x))

plot(x,y, col='red', pch=16)
lines(x, ypred)
dev.off()


Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Rsq

summary(mod)

###nico
