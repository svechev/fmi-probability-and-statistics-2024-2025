# k         1     2     3    4
# P(X=k)   0.5   0.2   0.1  0.2

# X - непрекъсната сл. вел.
# f(x) - плътност на X.     f(x)>= 0, интегралът от минус inf до inf e 1
# P (a<=X<=b)= интеграл от а до b на f(x)

# F(t)=P(X<=t)= интеграл от -inf до t на f(x)
# F(t) - функция на разпределение на Х
# P(a<=X<=b) = F(b) - F(a)         P(X=a) = 0 //конкретна ст-т
#                                  P(X<=a)=P(X<a) и P(X>=a)=P(X>a)

# равномерно разпр: f(x) = 1/b-a за x от [a, b]

# zad32       
x <- runif(500, 3, 5)
hist(x)
hist(x, probability=T, col='pink')  # probability = T: сумата от лицата на
                        # правоъгълниците е 1
curve(dunif(x,3,5), from=3, to=5, col='red', add=T, lwd=2)
help(dunif)
# add = T добавя на предходната графика
# lwd = дебелина на линията

# експоненциално разпределение

# zad33
x <- rexp(500, 1/7)
hist(x, probability=T)
curve(dexp(x, 1/7), from=0, to=max(x), add=T, lwd=2)

# нормално разпределение
#  мю - център, сигма - разтягане

# zad34 
x <- rnorm(5000, 0, 1)
hist(x, probability=T) #breaks=seq(от,до,дебелина)
curve(dnorm(x, 0, 1), add=T, lwd=2) #като има add няма нужда от
                                    #from и to
# x1, ... , xn
# ^F(t) = 1/n * sum(i 1 to n) of I(xi<=t)   // I - индикаторна ф-я
#  P(X<=t)

# zad35
n <- 20
x <- runif(n,7,9)
plot.ecdf(x, do.points=F)  
# do.points=F - да не са много дебели точките
curve(punif(x,7,9), col="cyan", add=T, lwd=5)        


# zad36
x <- rexp(500, 3)
plot.ecdf(x, do.points=F) 
curve(pexp(x, 3), col="cyan", add=T, lwd=2)

# zad37
x <- rnorm(200, 4, 1.2)
plot.ecdf(x, do.points=F) 
curve(pnorm(x, 4, 1.2), col="cyan", add=T, lwd=2)

#zad38     
curve(dunif(x, 7, 9), col="cyan", from=5, to=11, lwd=2)
curve(punif(x, 7, 9), col="red", add=T, lwd=2)
curve(qunif(x, 7, 9), col="gold",from=0, to=1, lwd=2)

# zad39 zad40 същите но с exp и norm

# zad41
# X - количество портокалов сок в случайно избрана бутилка
# X ~ U(495, 502)

# a) P(X > 500) = 1 - P(X<=500)
1 - punif(500, 495, 502)    # 2/7

# b) v=? P(X > v) = 0.8
v <- qunif(0.2, 495, 502)
v    # 496.4
punif(v, 495, 502)   # проверка

#zad42
# a)
# P(X>3) = 1 - P(X <= 3)
1-pexp(3, 1/4)

# b) P(X <= 2)
pexp(2, 1/4)

# в) P(X>6 | X>3) = P(X>6 и X>3) / P(X>3) = P(X>6) / P(X>3)
(1-pexp(6, 1/4))/(1-pexp(3, 1/4))

# г)
# t=? P(X<=t) = 0.9
t <- qexp(0.9, 1/4)
t

# zad43
# a) P(X>51) = 1 - P(X <= 51)
1-pnorm(51, 41, 5)


# b)
# P(45<X<50) = P(X<50)-P(X<45)
pnorm(50, 41, 5)-pnorm(45, 41, 5)

# в)
# kg=? P(X<kg)=0.99
kg <- qnorm(0.99, 41, 5)
kg

#zad47
l <- 1/8
x <- runif(5000, 0, 1)
y <- (-(1/l))*(log(1-x))
hist(y)
curve(dexp(x, l), from=0, to=max(y), add=T, col="cyan", lwd=2)












