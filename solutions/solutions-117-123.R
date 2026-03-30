# zad117
# биномно разпределение
# a) ако p=0.5 се търси вероятността X < 86 или X > 114
1-sum(dbinom(86:114, 200, 0.5))

# b) същото с p=0.61
1-sum(dbinom(86:114, 200, 0.61))

# zad118

# X = време за прочитане на текста от сл. избран ученик
# mu = E(X)
# H0: mu = 25      H1: mu < 25

# става и с x <- scan() //въвеждат се от клавиатурата
x <- c(25, 29, 18, 29, 22, 20, 27, 24, 20, 29, 18, 20, 31, 25, 21, 24, 24, 21, 18, 24, 24, 29, 25, 24, 27, 22, 25, 22, 27, 25)

t.test(x=x, mu=25, alternative='less')
# p.value = 0.06 > 0.05 => не отхвърляме нулевата хипотеза


# zad119

# H0: p = (1/7 ... 1/7)
# H1: p != (1/7 ... 1/7)

x <- c(144, 170, 158, 172, 148, 152, 156)
probs <- rep(1/7, 7)
chisq.test(x=x, p=probs)
# p.value = 0.64 > 0.05 => не отхвърляме H0


# zad120
tomato <- read.table("../data/tomato2.txt", header=T)

# X и Y = добив от сл. избр. раст. от 1 и 2 сорт
# muX = E(X)   muY = E(Y)
# H0: muX = muY       H1: muX < muY

t.test(tomato$v1, tomato$v2, alternative='less')
# p.value = 0.003 < 0.05 => отхвърляме нулевата хипотеза


# zad121
# p1, p2 = вероятност за мотор от марка 1 и 2 да премине теста
# H0: p1 = p2        H1: p1 != p2

n <- c(30, 30)
x <- c(22, 16)
prop.test(x=x, n=n, alternative='greater', correct=F)
# p.value = 0.053 > 0.05 => не отхвърляме нулевата хипотеза


# zad122
books <- read.table("../data/books.txt", header=T)
t.test(books$price1, books$price2, alternative='greater', paired=T)
# p.value < 0.05 => отхвърляме нулевата хипотеза 


# zad123
prop.test(x=89, n=500, p=0.17, alternative='greater', correct=F)
# p.value = 0.317 > 0.05 => не отхвърляме нулевата хипотеза

