library(KernSmooth)

x <- sample(x = 1:100, size = 10, replace = T)

dens <- bkde(x = x, bandwidth = 10)
plot(dens, xlab = "x", ylab = "Density Function", type = "l")

# NEW 

set.seed(124)
pos = c(rnorm(100, 2, 1), rnorm(50, 15, 3))
neg = rnorm(200,7,2)
all = c(neg,pos)

minx <- -5
maxx <- 25

plot(neg, rep(-0.5,200), col="blue",
     xlim=c(minx,maxx),
     ylim=c(-1,1),ylab="",
     xlab="pos")
points(pos, rep(0.5,150), col="red", xlim=c(minx,maxx))

# Estimate densities

bw <- 1
lb <- -5
ub <- 25

dp <- density(pos, bw = bw, from = lb, to = ub)
dn <- density(neg, bw = bw, from = lb, to = ub)
da <- density(all, bw = bw, from = lb, to = ub)

plot(x = dn, xlim = c(lb, ub), ylim = c(-0.05, 0.25), col = "blue")
lines(x = dp, col = "red")
lines(x = da, col = "green")


