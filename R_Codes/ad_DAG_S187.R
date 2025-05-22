set.seed(122)

# coefficients
bSA <- 1
bSW <- 2
bSM <- 3
bAM <- 4
bAD <- 5
bMD <- 6
bWD <- 7

S <- rnorm(1000)
A <- rnorm(1000, bSA * S)
W <- rnorm(1000, bSW * S)
M <- rnorm(1000, bSM * S + bAM * A)
D <- rnorm(1000, bMD * M + bAD * A + bWD * W)

# -> find true total efect bWD = 7:
plot(W, D)
summary(lm(D ~ W)) # 25.1206

# use adjustment set {S}
summary(lm(D ~ W + S)) # # 6.8123 -> good

# use adjustment set {A, M}
summary(lm(D ~ W + A + M)) # 6.9960
