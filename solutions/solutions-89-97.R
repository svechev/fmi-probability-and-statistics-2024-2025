# zad89

# Xi = диаметър на i-тата сл. избр. топка 
# i = 1, ... ,n, n=45
# mu = E(Xi)  sigma=sqrt(DX)=0.12
# X черта = 1/n(X1+...+Xn)
# (X черта - mu) / (sigma/sqrt(n)) ~ N(0, 1) за големи n (ЦГТ)

# (обикновено) alpha=0.05
# z € N(0, 1)
# P(Z > z alp/2) = alp/2 // симетрично за -z alp/2

# z alp/2 = qnorm(1-alp/2)  // така намираме z alp/2
# P(-zalp/2 <= (Xчерта-mu)/(sigma/sqrt(n)) <= zalp/2) ~ 1-alpha
# P(-zalp/2*sigma/sqrt(n) <= (Xчерта-mu) <= zalp/2 * sigma/sqrt(n)) ~ 1-alpha
# p(X черта - zalp/2*sigma/sqrt(n) <= mu <= X черта + същото) ~ 1-alpha
# // преобразувахме само mu да е в средата

# [Xчерта-zalp/2*sigma/sqrt(n), Xчерта+същото] 
# e 100(1-alpha)-процентен доверителен интервал за mu
# // за alpha = 0.05 => 95% доверителен интервал

# //т.е. в поне 95% от случаите стойността на mu е в този интервал

# сега към самата зад.89
z1.ci <- function(x.bar, sigma, n, alpha) {
  h1 <- x.bar - qnorm(1-alpha/2)*(sigma/sqrt(n))
  h2 <- x.bar + qnorm(1-alpha/2)*(sigma/sqrt(n))
  c(h1, h2)
}

z1.ci(x.bar=6.73, sigma=0.12, n=45, alpha=0.05)

z1.ci(x.bar=6.76, sigma=0.12, n=45, alpha=0.05)


# zad90
# Xi = концентрация на кадмии в i-тата гъба
# i = 1, ... ,n  n=10
# X черта = 1/n(X1+...+Xn)
# S = sqrt(1/n-1  *  сума по i на (Xi-X черта)^2 )

# (X черта - mu) / (S/sqrt(n)) ~ t-разпределение с n-1 степени
#                                                   на свобода
# // тук вместо z alpha/2 имаме t n-1, alp/2
# P(T > t n-1, alp/2) = alpha/2
# t n-1, alp/2 = qt(1-alp/2, df=n-1)  // така го намираме

# [Xчерта - t n-1, alp/2 * S/sqrt(n), Xчерта+същото] 

# сега към самата зад. 90
t1.ci <- function(x.bar, s, n, alpha) {
  b1 <- x.bar - qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  b2 <- x.bar + qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  c(b1, b2)
}

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

t1.ci(x.bar=mean(x), s=sd(x), n=length(x), alpha=0.05)

t1.ci(x.bar=mean(x), s=sd(x), n=length(x), alpha=0.1)

t.test(x, conf.level=0.95)$conf.int[1:2] # прави същото
t.test(x, mu=3.85, conf.level=0.9)


# zad91
# X = брой падания на ези при n хвърляния
# X ~ Bi(n, p)      P("ези")=p
# X/n = p шапка
# (p шапка - p) / (sqrt(p*(1-p)/n)) ~ N(0, 1) за големи n (ЦГТ)
# ако в знаменателя заместим с p шапка става същото за големи n

# P(-z alp/2 <= дългият израз <= z alp/2) ~ 1-alpha
# след преобразуване става:
# P(p шапка -z alp/2*(sqrt(p*(1-p)/n) <= p <= p шапка+същото) ~ 1-alpha

# интервалът става    // 100(1-alpha)-процентен доверителен
# [p шапка -z alp/2*(sqrt(p*(1-p)/n, p шапка + същото]

prop1.ci <- function(x, n, alpha) {
  p.hat <- x/n
  b1 <- p.hat - qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  b2 <- p.hat + qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  c(b1, b2)
}

prop1.ci(x=58, n=100, alpha=0.05)
prop1.ci(x=116, n=200, alpha=0.05)
prop1.ci(x=61, n=100, alpha=0.05)

# прави същото с малка разлика
prop.test(x=58,  n=100, conf.level=0.95, correct=F)
prop.test(x=116,  n=200, conf.level=0.95, correct=F)$conf.int[1:2]
prop.test(x=61,  n=100, conf.level=0.95, correct=F)$conf.int[1:2]


# zad94
sim1 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  ci <- t.test(x, conf.level=1-alpha)$conf.int[1:2]
  ci[1] <= 7 & ci[2] >= 7
}

res1 <- replicate(10000, sim1(n=50, alpha=0.05))
table(res1)/length(res1)


# zad95
sim2 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  p.val <- t.test(x, mu=7)$p.value
  p.val > alpha
}

# не отхвърляме хипотезата в 95% от случаите
res2 <- replicate(10000, sim2(n=500, alpha=0.05))
table(res2)/length(res2)


# zad96
sim3 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  t.result <- t.test(x, mu=7, conf.level=1-alpha)
  ci = t.result$conf.int[1:2]
  cnd1 <- (ci[1] <= 7 & ci[2] >= 7)
  p.val <- t.result$p.value
  cnd2 <- (p.val > alpha)
  c(cnd1, cnd2)
}

res3 <- replicate(10000, sim3(n=500, alpha=0.05))
cnd1r <- res3[1, ]
cnd2r <- res3[2, ]
table(cnd1r, cnd2r) # в никой случай само единия condition е true
# в 95% от случайте двете са true, 5% два false


# zad97
# 95% ДИ за mu:  [25.0128, 26.0212]
# H0: mu=25   H1: mu != 25
# alpha = 0.05, p.value =< 0.05 => отхвърляме H0

# Ако 25 не е в 95%-ния интервал => отхвърляме H0
# Ако 25 e в 95%-ния интервал => не отхвърляме H0

# т.е. в нашия случай отхвърляме H0



# Ho: p = (1/4, 1/2.)

chisq.test(x=c(141, 291, 132), p=c(1/4, 1/2, 1/4))



