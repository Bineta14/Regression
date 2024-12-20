##### Nuages de point
#library(digitize)
#filee<-"nuage_pts.png"
#cal = ReadAndCal(filee)
#data.points<-DigitData()
#df <-Calibrate(data.points, cal, 0.2, 0.8, 0.0, 0.6)


# Points extraits avec digitize de  R
points <- readRDS("C:/Users/fayebine/Downloads/proba_preds/points.rds")
x<-points$x
y<-points$y
plot(x,y)

# Points extraits avec matplotlib de python
coords_py <- read.csv("C:/Users/fayebine/Downloads/proba_preds/coords_py.txt")
x<-coords_py$X[sort(coords_py$X, index=TRUE)$ix]
y<-coords_py$X0[sort(coords_py$X, index=TRUE)$ix]
plot(x,y)


par(mfrow=c(3,2))
# modèle
ml <- lm(y ~ x)
plot(x, y, main="Régression linéaire", pch=19)
lines(x, predict(ml), col="darkgreen",lwd=2)

# modèle
mlns <- lm(y ~ 1)
plot(x, y, main="L R no slope", pch=19)
lines(x, predict(mlns), col="darkgreen",lwd=2)

# modèle quadratique
mq <- lm(y ~ poly(x, 2))
plot(x, y, main="Régression quadratique", pch=19)
lines(x, predict(mq), col="darkgreen",lwd=2)

# modèle loess
mp <- loess(y ~ x,span = 0.7)
plot(x, y, main="Régression poly ou loess", pch=19)
lines(x, predict(mp), col="darkgreen",lwd=2)

# modèle logarithmique
mlog <- lm(y ~ log(x))
plot(x, y, main="Régression logarithmique", pch=19)
lines(x, predict(mlog), col="darkgreen",lwd=2)


# modèle exponentiel
mexp <- nls(y ~ a * b^x, start = list(a = 1, b = 1))
#mexp<- lm(y~exp(x))
plot(x, y, main="Régression exponentielle", pch=19) #,xlim=c(-0.5,1))
lines(x, predict(mexp), col="darkgreen",lwd=2)

# bmodèle logistic
library(minpack.lm)
mlogt <- nlsLM(y ~ L / (1 + exp(-k * (x - x0)))+ C, 
           start = list(L = max(y), x0 = median(x), k = 1,C=min(y)))
plot(x, y, main="Régression exponentielle", pch=19) #,xlim=c(-0.5,1))
lines(x, predict(mlogt), col="darkgreen",lwd=2)
