# zad44
# P(точка е в кръга)= Sкръг/Sквадр= pi.R^2/4R^2=pi/4
# =приблизително= бр.точки в кръга/бр.т. в квадрата
# =приблизително= pi/4

# кръг с радиус 1
n <- 10^2 # 10^7
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
4*sum(x^2 + y^2 < 1)/n # точките в кръга са sum(x^2+y^2<1)


plot(x, y, pch=".", col="gray")
curve( sqrt(1-x^2), from=-1, to=1, add=T, col="red")

# zad45
# правоъгълник от 0.8 до 4
# I/Sправ =прибл= бр точки под f(x)/бр точки в прав.
f <- function(x) {
  exp(-x^2/2)*(1-sqrt(2*pi))
}

a <- -0.8
b <- 4
n <- 10^3
x <- runif(n, a, b)
y <- runif(n, 0, f(a)) # f(a) е макс на ф-ята - намаляваща
(sum(y <f(x))/n)*(b-a)*f(a)

# I = P(0.8 < X < 4)
pnorm(4) - pnorm(0.8) # истинската стойност на интеграла

# zad49
library(MASS)


data(survey)
?survey

survey

attach(survey) # за да не пишем после survey$Exer
table(Exer)  

?sort
sort(table(Exer), decreasing=T) # колоните се сортират по брой
100*table(Exer)/length(Exer) # таблица с проценти


barplot(table(Exer)) # графика
barplot(100*table(Exer)/length(Exer))

pie(table(Exer))
pie(table(Exer), col=c("red", "cyan", "gold"))# кръгова диаграма

# zad50
table(Pulse)
table(Pulse, useNA="ifany") # показва N/A стойности, ако има такива
pulse.interval <- cut(Pulse, breaks=seq(30,110,10))
pulse.interval # разделяме на интервали (30, 40], (40, 50] до 110
table(pulse.interval)

barplot(table(pulse.interval)) 
hist(Pulse)  # barplot е като hist

stripchart(Pulse, method='stack', pch=20)
stripchart(Pulse, method='stack', pch=18)
stripchart(Pulse, method='stack', pch="*") # като хистограма, но
                                           # по-конкретна

# zad51
table(Age)

age.interval <- cut(Age, breaks=seq(15,75,10))
table(age.interval)

barplot(table(age.interval))
pie(table(age.interval)) # funny
stripchart(Age, method="stack", pch="*")

# zad52

# медиана - числото в средата (ако са сортирани)
# първи квартил - дели на 25/75%
# трети квартил - дели на 75/25%

boxplot(Pulse, horizontal=T)
boxplot(Age, horizontal=T)
?boxplot

x <- rnorm(10, 5, 1)
boxplot(x)  # изобразява квартилите и медианата къде са
boxplot(x, horizontal=T) # хоризотално
# излизат кръгчета за много големи и много малки стойности

# [Q1-1.5(Q3-Q1), Q3+1.5(Q3-Q1)] - извън тези интервал са кръгчетата

# обратно към zad52
v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

median(v1); mean(v1); sd(v1)
median(v2); mean(v2); sd(v2)
median(v3); mean(v3); sd(v3)
median(v4); mean(v4); sd(v4)
median(v5); mean(v5); sd(v5)

# zad53
getwd()
load("../data/cereals.RData")

attach(cereals)
median(sodium, na.rm=T); mean(sodium); sd(sodium)
boxplot(sodium, horizontal=T)
hist(sodium)

# zad54
data(survey)
boxplot(Pulse ~ W.Hnd) # boxplot едно до друго и за двете ръце

median(Pulse[W.Hnd=="Left"], na.rm=T) # смята медиана на пулс 
                                      # само за лява ръка
median(Pulse[W.Hnd=="Right"], na.rm=T)

mean(Pulse[W.Hnd=="Left"], na.rm=T)
mean(Pulse[W.Hnd=="Right"], na.rm=T)

sd(Pulse[W.Hnd=="Left"], na.rm=T)
sd(Pulse[W.Hnd=="Right"], na.rm=T)

# zad55
summary <- function(x) {
  res <- c(median(x, na.rm=T),
           mean(x, na.rm=T),
           sd(x, na.rm=T))
  # names(res) <- ("Median" "Mean", "SD")
  res
}
summary(Pulse)
summary(Pulse[Sex=="Female"])
summary(Pulse[Age <= 25])
summary(Pulse[Exer=="Freq"])
summary(Pulse[Exer=="Freq" & Smoke=="Never"])

# zad56
boxplot(Pulse ~ Exer)
summary(Pulse[Exer=="None"])
summary(Pulse[Exer=="Some"])
summary(Pulse[Exer=="Freq"])

# zad57
Smoke
table(Smoke)
plot(Smoke)
pie(table(Smoke))

# zad58
summary(Age)
summary(Age[Smoke!="Never"])
summary(Age[W.Hnd=="Right"])
summary(Age[Pulse>=70])
summary(Age[Exer=="None"])

# zad59
Height
table(Height)
plot(Height)
pie(table(Smoke))
hgh.intervals <- cut(Height, breaks=seq(150, 200, 5))
table(hgh.intervals)
plot(hgh.intervals)
pie(table(hgh.intervals))
summary(Height[Sex=="Male"])
summary(Height[Sex=="Female"])
