require(qtruncate)

################################################################
dtnorm <-dtruncate("norm")
dtnorm(x = 1:10, mean = 3, sd = 2, log = F)
dtnorm(x = 1:10, mean = 3, sd = 2, log = T)

dnorm(x = 1:10, mean = 3, sd = 2, log = F)
dnorm(x = 1:10, mean = 3, sd = 2, log = T)

dtnorm(x = 1:10, mean = 3, sd = 2, log = F, L = 2, U = 6)
dtnorm(x = 1:10, mean = 3, sd = 2, log = T, L = 2, U = 6)

###################################################################

ptnorm <-ptruncate("norm")
ptnorm(q = 1:10, mean = 3, sd = 2, L = 2, U = 6,log.p = F)
ptnorm(q = 1:10, mean = 3, sd = 2, log.p = T, L = 2, U = 6)

pnorm(q = 1:10, mean = 3, sd = 2, log.p = T, lower.tail = T)
pnorm(q = 1:10, mean = 3, sd = 2, log.p = T, lower.tail = F)

ptnorm(q = 1:10, mean = 3, sd = 2, log.p = T, lower.tail = T)
ptnorm(q = 1:10, mean = 3, sd = 2, log.p = T, lower.tail = F)



pnorm(q = 1:10, mean = 3, sd = 2, log.p = T, lower.tail = F)

ptnorm(q = 1:10, mean = 3, sd = 2, lower.tail = T)
ptnorm(q = 1:10, mean = 3, sd = 2, lower.tail = F)

ptnorm(q = 1:10, mean = 3, sd = 2, log = F, L = 2, U = 6, lower.tail = T)
ptnorm(q = 1:10, mean = 3, sd = 2, log = F, L = 2, U = 6, lower.tail = F)


qtnorm <-qtruncate("norm")
qtnorm(p = 1:9/10, mean = 3, sd = 2, log.p = F, lower.tail = T)
qtnorm(p = 1:9/10, mean = 3, sd = 2, log.p = F, lower.tail = F)

qnorm(p = 1:9/10, mean = 3, sd = 2, lower.tail = T)
qnorm(p = 1:9/10, mean = 3, sd = 2, lower.tail = F)
qnorm(p = 1:9/10, mean = 3, sd = 2, log.p = T)


qtweibull <-qtruncate("weibull")
qtweibull(p = 1:9/10, shape = 1, scale = 1.5, log.p = F, L = 3, U = 6, lower.tail = F)
x <- log(1:9/10)
qtweibull(p = x, shape = 1, scale = 1.5, log.p = T)

qtweibull(p = 1:9/10, shape = 1, scale = 1.5, log.p = F, L = 3, U = 6, lower.tail = F)
qweibull(p = 1:9/10, shape = 1, scale = 1.5, log.p = F, lower.tail = F)
qweibull(p = 1:9/10, shape = 1, scale = 1.5, log.p = F)

a <- qtweibull(p = c(-2.3025851, -1.6094379, -1.2039728, -0.9162907), shape = 1, scale = 1.5, log.p = T, L = 2, U = 6)

pweibull <- ptruncate("weibull")
pweibull(q = a, shape = 1, scale = 1.5, L = 2, U = 6, log.p = T)


qtweibull(p = c(-2.3025851, -1.6094379, -1.2039728, -0.9162907), shape = 1, scale = 1.5, log.p = T, L = 2, U = 6)
qtweibull(p = 1:9/10, shape = 1, scale = 1.5, log.p = F, L = 2, U = 6)

qtrunc(p = 1:9/10, spec = "weibull", a = 3, b = 6, shape = 1, scale = 1.5, lower.tail=F)


qtweibull(p = 1:9/10, shape = 1, scale = 1.5, lower.tail = F)
qtweibull(p = 1:9/10, shape = 1, scale = 1.5, lower.tail = T)

qweibull(p = 1:9/10, shape = 1, scale = 1.5, lower.tail = F)
qweibull(p = 1:9/10, shape = 1, scale = 1.5, lower.tail = T)



ptnorm(q = 1:10, mean = 3, sd = 2, lower.tail = T)
ptnorm(q = 1:10, mean = 3, sd = 2, log = F, L = 2, U = 6, lower.tail = T)
