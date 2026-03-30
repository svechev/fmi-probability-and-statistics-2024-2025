getwd()
exam <- read.table("../data/examAB.txt", header=T) 
# header=T - на първия ред са имената на променливите


# zad80
# Xi = оценка на i-тия студент с вариант А
# Yj = същото с вариант В
# mux = E(Xi), muy = E(Yj)
# Ho: mux = muy      H1: mux > muy
# X черта = 1/n(X1+..Xn), Y черта = 1/m(Y1+...Ym)
# Sx = sqrt(1/(n-1) * сума по i на (Xi-X черта)^2 )
# Sy = sqrt(1/(m-1) * сума по i на (Yi-Y черта)^2 )

# T = X черта - Y черта - (mux-muy)  /   sqrt((Sx^2)/n + (Sy^2)/m)
# ~ t-разпределение
# tobs = формулата за Т с конкретните наблюдавани стойности
# заместваме и (mux-muy)=0 тоест при условие че хипотезата H0 е вярна
# p.value = P(T >= tobs)
# Aко p.value <= alpha=0.05 => отхв H0
# Aко p.value > alpha=0.05 => не отхв H0

x <- exam$points[exam$variant=="A"]
y <- exam$points[exam$variant=="B"]
t.test(x, y, alternative="greater")
t.test(x, y, alternative="greater")$p.value # връща само p.value

# p.value = 0.01668 < alpha 
# => отхвърляме нулевата хипотеза в полза на алтернативната
# може да се твърди, че средно студентите получавата по-малко точки
# ако им се падне вариант B.


# zad81
#  (Xi, Yi)
# Xi = време на реакция на i-тия участник преди водка 
# Yi = време след водка
# mux=E(Xi), muy=E(Yi)
# H0: mux = muy    H1: mux < muy

# можем да вземем Di = Xi - Yi,   i = 1, .. ,n
# D черта = 1/n (D1+ ... Dn),    Sd = дългата формула с корен 
# H0: mud = 0    H1: mud < 0    // mud = mux - muy
# T = D черта - mud   /    Sd/sqrt(n) ~ t-разпределение с n-1 степени
#                                                          на свобода
# tobs = d черта /  Sd/sqrt(n)   - конкретната ст-т (mud=0)
# p.value = P(T<=obs) може да се сметне с pt(tobs, df=n-1)


react <- read.table("../data/reacttime.txt", header=T)
x <- react$before
y <- react$after
t.test(x, y, alternative="less", paired=T)
# paired ако са по двойки
# options(scipen=999) - да излизат малки дроби като числа
# p.value < 0.05 - отхвърляме нулевата хипотеза, можем да кажем
# че средното време на реакция се увеличава

t.test(x-y, miu=0, alternative="less") # това е същото

# друг начин:
d <- x - y
n <- length(d)
t.obs <- mean(d) / (sd(d)/sqrt(n))
p.value <- pt(t.obs, df=n-1) # става същото 


# zad82
# X1 = брой дефектни болтове от машина А (от 200 проверени)
# X2 = същото за машина В
# X1 ~ Bi(n=200, p1)    X2 ~ Bi(n=200, p2)
# H0: p1 = p2        H1: p1 != p2
# P шапка = X1+X2 / n1+n2

#           X1/n1 + X2/n2 - (p1-p2)
# Z =     --------------------------      Z ~ N(0, 1) за големи n1, n2
#      sqrt(P шапка(1-p шапка)(1/n1+1/n2))

# zobs = формулата за Z с конкретни стойности
# p.value = P(Z<=-|zobs|) + P(Z>=|zobs|) = 2 P(Z<=-|zobs|)  //симетрия

x1 <- 8
x2 <- 15
n1 <- 200
n2 <- 200
p.hat <- (x1+x2)/(n1+n2)

z.obs <- (x1/n1 - x2/n2) / sqrt(p.hat*(1-p.hat)*(1/n1+1/n2))
p.value <- 2*(pnorm(-abs(z.obs)))
p.value # 0.1327191 > 0.05 => не можем да отхвърлим нулевата
# хипотеза. Не може да се твърди че двете машини се различават
# по отношение на вероятността да произведат дефектен болт.
help(prop.test)
prop.test(x=c(x1,x2), n=c(n1,n2), correct=F) # смята същото
# correct = F - връща ни "without continuity correction"


# zad83
# 1)   Xi = цена на i-тия уред в магазин А
#      Yi = цена на същия уред в В
#  (Xi, Yi)  -> двойки наблюдения (защото са за един и същи уред)

# 2) независими извадки

# 3) Xi = оценка на i-тия студент на К1
#    Yi = оценка на същия студент на К2
# (Xi, Yi) -> двойки наблюдения

# 4) независими извадки (͡°͜ʖ͡°)


# zad84
sim.t2 <- function(n, mu1, mu2, sigma1, sigma2) {
  x <- rnorm(n, mean=mu1, sd=sigma1)
  y <- rnorm(n, mean=mu2, sd=sigma2)
  t.test(x,y)$p.value
}

rs <- replicate(10000, sim.t2(n=500, mu1=5, mu2=5, sigma1=1, sigma2=0.8))
sum(rs<=0.05) /length(rs)
# при всяко n се отхвърля нулевата хипотеза около 5% от случаите


# zad85
rs <- replicate(10000, sim.t2(n=50, mu1=5, mu2=5.2, sigma1=1, sigma2=1))
sum(rs<=0.05) /length(rs)
# като увеличаваме n шансът за отхвърляне се приближава към 1


#zad86
x1 <- 26
x2 <- 43
n1 <- 500
n2 <- 540
prop.test(x=c(x1,x2), n=c(n1,n2), alternative='l', correct=F)

#zad87
x1 <- c( 1.2,1.3,1.5,1.4,1.7,1.8,1.4,1.3)
x2 <- c(  1.4,
          1.7,
          1.5,
          1.3,
          2.0,
          2.1,
          1.7,
          1.6)
t.test(x1, x2, paired=T)


















