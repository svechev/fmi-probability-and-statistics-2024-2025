# zad98
# Xi - бр. падания на i (i=1, ... ,6) от n хвърляния на зар
# Xi ~ Bi(n, pi)         pi = вероятност да се падне i
# хипотези:
# H0: (p1, ... ,p6)=(1/6, ... ,1/6)
# H1: (p1, ... ,p6)!=(1/6, ... ,1/6) - алтернативна хипотеза

#Chi^2= сума по i (Xi-n*pi)^2 / n*pi   - има хи-квадрат разпр. с 6-1
#                                        степени на свобода (за големи n)

#Chi^2obs = същата сума с малки x  // наблюдаваните ст-и
# = сума по i от (observed i - expected i) / expected i 

# n = 180, pi = 1/6
# p.value = P(Chi^2 > Chi^2obs)
# Ако p.value =< 0.05 => отхв. H0
# Ако p.value > 0.05 => не отхв. H0
# p.value = 1-pchisq(Chi^2obs, df=6-1)

x <- c( 28, 36, 36, 30, 27, 23)
p <- rep(1/6, 6)

n <- sum(x)
k <- length(x)

chi2.obs <- sum( ((x - n*p)^2) / (n*p) )
chi2.obs

p.value <- 1 - pchisq(chi2.obs, df=k-1)
p.value  # 0.48

# p.value > 0.05 -> не можем да отхвърлим нулевата хипотеза
# че зарът е балансиран

chisq.test(x=x, p=p)  #chisq.test(x, p) не става, трябва да ги уточним


# zad108
alco = read.table("../data/bac.txt", header=T)
plot(bac ~ beers, data=alco)

# Y = b0 + b1*X + e
# E(e)=0, Var(e)=sig^2
# (xi, yi), i=1, ... ,n
# min(b0, b1) на сума по i (yi-b0-b1*xi)^2
# -> bo шапка, b1 шапка  // тези, които минимизират

# y шапка = b0 шапка + b1 шапка*x -> оценено регресионно уравнение
# lm(y ~ x)

m1 <- lm(bac ~ beers, data=alco)
m1
# в случая:
# y - съдържание на алкохол в кръвта
# x - брой изпити бутилки бира

# в) H0: b1 = 0 (няма зависимост)
#    H1: b1 != 0 (има зависимост, алт. хипотеза)

# summary(m1)$coefficients

#  г) H0: b1 = 0.02
#     H1: b1 != 0.02
# 95% ДИ за b1 [0.0128, 0.023] - 0.02 e в него -> не можем да 
#                                          отхвърлим H0

confint(m1) # намира дов. инт

# д)
predict(m1, data.frame(beers=5), interval='confidence')



