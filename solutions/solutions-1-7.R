1+3
4*4

x <- c(1,2,3,4,6) # vector 1 2 3 4 6  
x

2:18 # 2 3 4 ... 18
18:2 # 18 ... 4 3 2

seq(5,15,2) # 5 to 15 step 2
seq(15, 5, -2) # 15 to 5 step -2
rep(5, 10) # 5 repeated 10 times
rep(c(1,2,3), 5)

sample( c(1:100), 5, replace=T) # 5random elems from 1 to 10


#zad1
sim.zad1 <- function() {
  x <-sample(c(1:8), 2, replace=T)
  x[1]==x[2]
}

res <- replicate(100000, sim.zad1())
sum(res)/length(res)

#zad2
sim.zad2 <- function() {
  socks <- c(1,1,2,2,3,3)
  x <- sample(socks, 2, replace=F)
  x[1]==x[2]
}

res <- replicate(100000, sim.zad2())
sum(res)/length(res)

#zad3
sim.zad3 <- function() {
  keys <- (1:4)
  x <- sample(keys, 4, replace=F) # that is just permutations
  x[4]==1
  # the correct key is 1
}

res <- replicate(100000, sim.zad3())
sum(res)/length(res)

#zad4
sim.zad4 <- function() {
  questions <- c(rep(0,3), rep(1, 17))
  x <- sample(questions, 2, replace=F)
  x[1]!=x[2] # sum(X)==1 also works
}

res <- replicate(100000, sim.zad4())
sum(res)/length(res)

# other way to write it
prob.exam <- function(Nrep) {
  res <- replicate(100000, sim.zad4())
  sum(res)/length(res)
}
prob.exam(100000)


#zad5
sim.zad5 <- function(k) {
  days <- c(1:365)
  x <- sample(days, k,replace=T)
  any(duplicated(x))
}

res <- replicate(100000, sim.zad5(25))
sum(res)/length(res)

#zad6                                          # 1 2 3 4 5 .. 20
sim.zad6 <- function() {                       # 3 2 5 1 7 .. 11
  x <- sample( c(1:20), 20, replace=F)
  d <- x - c(1:20)
  any(d==0)
}

res <- replicate(100000, sim.zad6())
sum(res)/length(res)

#zad7                                        
sim.zad7 <- function() {                      
  m1 <- sample( c(2,3), 1)
  m2 <- sample( c(1,2), 1)
  m3 <- sample( c(1,3), 1)
  m <- c(m1,m2,m3)
  !any(duplicated(m))   #  sum(m)==6  # length(unique(m))==3
}

res <- replicate(100000, sim.zad7())
sum(res)/length(res)


