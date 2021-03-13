####DSL medium class 1 addtional. Miscellaneous Distributions####
####1. Binominal Distribution####
x <- 1:100
plot(x = x, y = dbinom(x, prob = 0.1, size = 100), type = "b",
     ylab = "Probablity Density", pch = 19, ylim = c(0, 0.15))
points(x = x, y = dbinom(x, prob = 0.3, size = 100), type = "b", pch = 19, col = 2)
points(x = x, y = dbinom(x, prob = 0.5, size = 100), type = "b", pch = 19, col = 3)
points(x = x, y = dbinom(x, prob = 0.7, size = 100), type = "b", pch = 19, col = 4)
points(x = x, y = dbinom(x, prob = 0.9, size = 100), type = "b", pch = 19, col = 6)

label <- c("(100,0.1)", "(100,0.3)", "(100,0.5)", "(100,0.7)", "(100,0.9)")
legend(x = 35, y = 0.15, legend = label, lty = c(1,1,1,1,1), col = c(1,2,3,4,6))

####2. Poission Distribution####
x <- 1:100
plot(x = x, y = dpois(x, lambda = 10), type = "b",
     ylab = "Probablity Density", pch = 19, ylim = c(0, 0.15))
points(x = x, y = dpois(x, lambda = 30), type = "b", pch = 19, col = 2)
points(x = x, y = dpois(x, lambda = 50), type = "b", pch = 19, col = 3)
points(x = x, y = dpois(x, lambda = 70), type = "b", pch = 19, col = 4)
points(x = x, y = dpois(x, lambda = 90), type = "b", pch = 19, col = 6)

label <- c("lam = 10", "lam = 30", "lam = 50", "lam = 70", "lam = 90")
legend(x = 35, y = 0.15, legend = label, lty = c(1,1,1,1,1), col = c(1,2,3,4,6))

####3. Negative Binominal Distribution####
x <- 1:100
plot(x = x, y = dnbinom(x, prob = 0.1, size = 10), type = "b",
     ylab = "Probablity Density", pch = 19, ylim = c(0, 0.15))
points(x = x, y = dnbinom(x, prob = 0.3, size = 10), type = "b", pch = 19, col = 2)
points(x = x, y = dnbinom(x, prob = 0.5, size = 10), type = "b", pch = 19, col = 3)
points(x = x, y = dnbinom(x, prob = 0.7, size = 10), type = "b", pch = 19, col = 4)
points(x = x, y = dnbinom(x, prob = 0.9, size = 10), type = "b", pch = 19, col = 6)

label <- c("(10,0.1)", "(10,0.3)", "(10,0.5)", "(10,0.7)", "(10,0.9)")
legend(x = 35, y = 0.15, legend = label, lty = c(1,1,1,1,1), col = c(1,2,3,4,6))

####4. Gamma Distribution####
x <- 1:40
plot(x = x, y = dgamma(x, shape = 36, rate = 2), type = "b",
     ylab = "Probablity Density", pch = 19, ylim = c(0, 0.6))
points(x = x, y = dgamma(x, shape = 36, rate = 8), type = "b", pch = 19, col = 2)
points(x = x, y = dgamma(x, shape = 36, rate = 12), type = "b", pch = 19, col = 3)
points(x = x, y = dgamma(x, shape = 48, rate = 2), type = "b", pch = 19, col = 4)
points(x = x, y = dgamma(x, shape = 48, rate = 8), type = "b", pch = 19, col = 6)
points(x = x, y = dgamma(x, shape = 48, rate = 12), type = "b", pch = 19, col = 7)

label <- c("(24,2)", "(24,8)", "(24,12)", "(48,2)", "(48,8)", "(48,12)")
legend("topright", legend = label, lty = c(1,1,1,1,1,1), col = c(1,2,3,4,6,7))
