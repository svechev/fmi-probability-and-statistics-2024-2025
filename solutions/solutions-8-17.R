
#zad8                                        
sim.eggs <- function() {                      
  eggs <- c(rep("r", 6), rep("b", 2))
  draws <- sample(eggs, 8, replace=F)
  player1 <- draws[seq(1,7,2)]
  player2 <- draws[seq(2,8,2)]
  b1 <- sum(player1=="b")    # boiled eggs for p1
  b2 <- sum(player2=="b")    #                 p2
  c(b1,b2)
}

sim.eggs()

Nrep <- 10000
res <- replicate(Nrep, sim.eggs())
res    # returns matrix
# res[1,]  first row
# res[,3]  third column

# zad8 A
(sum(res[1,]==2) + sum(res[2,]==2)) / Nrep

# zad8 B
sum(res[1,]==1) / Nrep
# sum(res[2,]==1) # is the same

# zad8 C
sum(res[1,]==2) / Nrep

# zad8 D
sum(res[2,]==2) / Nrep

#zad9
sim.zad9 <- function() {
  x <- sample(c(1,0,0,0), 10, replace=T)
  sum(x)
}

res <- replicate(100000, sim.zad9())
sum(res>=5)/length(res)

# does the same thing 
x <- sample( c(1,0), 10, replace=T, prob=c(1/4,3/4))
x

#zad10

# on time is 1, otherwise 0

sim.zad10 <- function() {
  x <- sample( c(1,0), 143, replace=T, prob=c(0.92,0.08))
  sum(x)
}

res <- replicate(100000, sim.zad10())

# zad10 A
sum(res<=138)/length(res)

# zad10 B
sum(res==137)/length(res)

#zad11

sim.zad11a <- function() {
  dice <- sample(c(1:6), 1)
  if (dice == 6) {
    ball <- sample( c("g", "g", "r", "r"), 1)
  }
  else {
    ball <- sample( c(rep("r", 4), "g"), 1)
  }
  ball == "g"
}

res <- replicate(1000000, sim.zad11a())
sum(res)/length(res)

# A = { вадим топка от II кутия } = {не се пада 6}
# B = { вадим зелена топка }

#            P(A и B)     approx    Cn(A и B) /n
# P(A|B) =  ----------      =       -------------
#              P(B)                    Cn(B) /n
 
#  Cn(не се пада 6 и вадим зелена топка)
# ---------------------------------------
#         Cn(вадим зелена топка)

# zad11 B

sim.zad11b <- function() {
  dice <- sample(c(1:6), 1)
  if (dice == 6) {
    ball <- sample( c("g", "g", "r", "r"), 1)
  }
  else {
    ball <- sample( c(rep("r", 4), "g"), 1)
  }
  c(dice, ball)  # different types, make the number a character
}

res <- replicate(100000, sim.zad11b())
sum(res[1,]!="6" & res[2,]=="g") / sum(res[2,]=="g")

# zad12

sim.zad12 <- function() {
  coin <- sample(c(11,11,22,12,12), 1)
  if (coin == 12) {
    up <- sample(c(2,1),1)
  }
  else {
    if (coin == 11) {
      up <- 1
    }
    else {
      up <- 2
    }
  }
  c(coin, up)
}

# zad12 A
res <- replicate(Nrep, sim.zad12())
# matrix - first row is the coin, second row is the up

sum(res[2,]==1) / Nrep

# zad12 B

#  A = {монетата  етип Т12}
#  B = {горната страна е 1}
#  
#           Cn(A и B)   Cn(монетата е Т12 и горната стр е 1)
# P(A|B) = --------- = -------------------------------------
#             Cn(B)            Cn(горната стр е 1)

sum(res[1,]==12 & res[2,]==1) / sum(res[2,]==1)

# other way:
# coin <- ... with strings
# side <- ... with numbers
# up <- substr(coin, start=side, stop=side) takes sub-string 
# c(up, coin)                              from start to stop


#zad14
sim.zad14 <- function() {
  balls <- sample(c(1:99), 4, replace=FALSE)
  cnd1 <- (balls[1] > balls[2])
  cnd2 <- (balls[1] > balls[3])
  cnd3 <- (balls[1] > balls[4])
  cnd1 && cnd2 && cnd3
}

res <- replicate(100000, sim.zad14())
sum(res)/length(res)


# zad16
sim.zad16 <- function() {
  cards <- c(1,1,1,1,rep(0, 48))
  x <- sample(cards, 52, replace=F)
  p1 <- x[1:13]
  p2 <- x[14:26]
  p3 <- x[27:39]
  p4 <- x[39:52]
  sum(p1)==1&sum(p2)==1&sum(p3)==1&sum(p4)==1
}

res <- replicate(100000, sim.zad16())
sum(res)/length(res)

# zad 17 a
sim.zad17a <- function() {
  floors <- sample(c(2:16), 7, replace=T)
  any(floors[2:7]==floors[1])
}

res <- replicate(10000, sim.zad17a())
sum(res)/length(res)


