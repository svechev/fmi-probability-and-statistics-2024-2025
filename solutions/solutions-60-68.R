# zad60
sumexpon <- function(n) {
  x <- rexp(n, 1/5)
  sum(x)
}
# можеше направо replicate(10000, sum(rexp...))
n <- 100
sums <- replicate(10000, sumexpon(n))
hist(sums)

for (n in c(3,7,10,30,90,200)) {
  xsum <- replicate(10000, sum(rexp(n, 1/5)))
  hist(xsum, main=paste("n =", n))
}

# подточка б)
for (n in c(3,7,10,30,90,200)) {
  xsum <- replicate(10000, sum(rexp(n, 1/5)))
  plot.ecdf(xsum, do.points=FALSE, col="blue", main=paste("n =", n))
  curve(pnorm(x, 5*n, 5*sqrt(n)), add=T, col="red", lty="dashed")
}
# за по-големи n двете функции се приближават

# zad61
for (n in c(3,7,10,30,90,200)) {
  xmean <- replicate(10000, mean(rexp(n, 1/5)))
  hist(xmean, main=paste("n =", n))
}

# б)
for (n in c(3,7,10,30,90,200)) {
  xmean <- replicate(10000, mean(rexp(n, 1/5)))
  plot.ecdf(xmean, do.points=FALSE, col="blue", main=paste("n =", n))
  curve(pnorm(x, 5, 5/sqrt(n)), add=T, col="red", lty="dashed")
}

# zad62
for (n in c(3,7,10,30,90,200)) {
  xsum <- replicate(10000, sum(rpois(n, 3)))
  hist(xsum, main=paste("n =", n))
}

# подточка б)
for (n in c(3,7,10,30,90,200)) {
  xsum <- replicate(10000, sum(rpois(n, 3)))
  plot.ecdf(xsum, do.points=FALSE, col="blue", main=paste("n =", n))
  curve(pnorm(x, 3*n, sqrt(3*n)), add=T, col="red", lty="dashed")
}

# zad63
for (n in c(3,7,10,30,90,200)) {
  xmean <- replicate(10000, mean(rpois(n, 3)))
  hist(xmean, main=paste("n =", n))
}

# подточка б)
for (n in c(3,7,10,30,90,200)) {
  xmean <- replicate(10000, mean(rpois(n, 3)))
  plot.ecdf(xmean, do.points=FALSE, col="blue", main=paste("n =", n))
  curve(pnorm(x, 3, sqrt(3)/sqrt(n)), add=T, col="red", lty="dashed")
}

# Централна гранична теорема
# Нека X1,..,Xn са независими и еднакво разпределени сл.вел.
# Нека miu=E(X1) sigma^2=D(X1), X черта = 1/n*(X1+..+Xn).
# Тогава, за големи стойности на n, случайната величина
# (X черта - miu) / (sigma/sqrt(n)) e приблизително нормално
# разпределение, N(0, 1)
# P(дългият израз <= t) прибл. = pnorm(t, 0, 1) = pnorm(t)

# E(X черта)=miu, D(X черта)= sigma^2/n
# E(дългият израз)=0 D(дълъг израз)=1


# zad65
# Xi - време на живот на i-тата крушка
# Xi~Exp(lambda)  E(Xi)=1/lambda=900 => lambda=1/900
# sqrt(D(Xi)) = 1/lambda = 900
# miu = E(Xi)=900, sigma=sqrt(D(X))=900
# X черта = 1/n*(X1+..+Xn), n=100
# търси се P(X черта > 980)
# P(X черта > 980)= 1 - P(X черта <= 980)=
# =1 - P(дългия израз със X черта, miu и sigma <= 980 - дългия израз)
# и в дългите изрази заместваме със n=100, miu=900 sigma=900 
# =прибл pnorm(a)

a <- (980-900)/ (900/sqrt(100))
1-pnorm(a)
#same thing:
1 - pnorm(980, 900, 900/sqrt(100))
# X черта =прибл N(miu, sigma/sqrt(n))

# друг начин:
mean.vals <- replicate(100000, mean(rexp(100, 1/900)))
sum(mean.vals >980)/length(mean.vals) # броят стойности>980/брой всички

# zad66
# Xi - време на чакане на i-тия човек
# Xi~U(0, 60)
# miu=E(Xi) = 30  // защото е равномерно -> средата на интервала
# sigma=sqrt(D(X))=sqrt(60^2/12)=60/sqrt(12)
# // дисперсията на равномерното е (b-a)^2 / 12
# X черта = 1/n(X1+...+Xn), n = 50
# търсим P(25<X черта<35) = P(X черта<35)-P(X черта<25)=
# = P(заместваме с дългите изрази)-P(същото)
# заместваме sigma=60/sqrt(12) miu=30 n=50
# =прибл pnorm(b)-pnorm(a)

a <- (25-30) / (60/sqrt(12*50))
b <- (35-30) / (60/sqrt(12*50))
pnorm(b)-pnorm(a)

# друг начин
mean.vals <- replicate(100000, mean(runif(50, 0, 60)))
sum(mean.vals > 25 & mean.vals < 35) / length(mean.vals)

hist(mean.vals)

# zad67
# Xi - брой стафиди в i-тата кифличка
# miu = E(Xi) = 4*0.2+5*0.4+... = sum(k from 4 to 7) k*pk 
# sigma^2 = E(Xi^2)-miu^2
# E(Xi^2)= sum(k from 4 to 7)k^2*pk
# X черта = 1/n(X1+...+Xn), n=49
# P(X черта>5.5)=1-P(X черта <= 5.5)=
# =1-P(дългия израз с X черта<= дългия израз но с 5.5 вместо X черта)
# =прибл 1-pnorm(a)

x <- c(4:7)
p <- c(0.2, 0.4, 0.3, 0.1)
mu <- sum(x*p)   # 5.3
sigma <- sqrt( sum((x^2)*p) - mu^2) # 0.89
n <- 49

a <- (5.5-mu) / (sigma/sqrt(n))
1-pnorm(a)

# със симулация:
mean.vals <- replicate(100000, mean(sample(x, 49, replace=T, prob=p)))
sum(mean.vals > 5.5) / length(mean.vals)


# (Xчерта - miu) / (sigma/sqrt(n))
# ако умножим по n/n:
# (X1+...Xn - n*miu) / (sigma*sqrt(n))
# P(новия израз <= t) =прибл pnorm(t)

# zad68
# Xi = количество багаж на i-тия пътник
# miu=E(Xi)=24    sigma=sqrt(D(X))=7  n=160  // дадени
# P(X1+...X160 > 4000)=1-P(same sum <= 4000)
# =1-P((sum-160*24)/(7*sqrt(160)) <= същия израз с 4000 вместо сумата)
# // в P заместихме с новия израз
# =прибл 1-pnorm(a)

a <- (4000 - 160*24) / (7*sqrt(160))
1-pnorm(a)
# не може да се реши със симулация защото не ни е дадено
# точното разпределение, а само очакването и отклонението


















