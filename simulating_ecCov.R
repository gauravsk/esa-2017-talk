library(MASS)

env1 <- rnorm(100, 100, 20)
env_cor <- env1 + rnorm(100, 1, 10)
correlated_pair_es <- cbind(env1, env_cor)
alpha_intra <- 0.1
alpha_inter <- .05
# correlated_pair_cs <- cbind(1+alpha_intra*correlated_pair_es[,1] + alpha_inter*correlated_pair_es[,2], 
#                             1+alpha_intra*correlated_pair_es[,2] + alpha_inter*correlated_pair_es[,1])

correlated_pair_cs <- cbind(1+alpha_intra*rowSums(correlated_pair_es),
                            1+alpha_intra*rowSums(correlated_pair_es))

plot(correlated_pair_cs[,1]~correlated_pair_es[,1], pch = 19)
points(correlated_pair_cs[,2]~correlated_pair_es[,2], pch = 19, col = 2)


uncorrelated_pair_es <- cbind(env1, rnorm(100, 100, 25))
plot(uncorrelated_pair_es)
cor(uncorrelated_pair_es)
# uncorrelated_pair_cs <- cbind((1+alpha_intra*uncorrelated_pair_es[,1]) + alpha_inter*uncorrelated_pair_es[,2], 
#                               (1+alpha_intra*uncorrelated_pair_es[,2]) + alpha_inter*uncorrelated_pair_es[,1])

uncorrelated_pair_cs <- cbind(1+alpha_intra*rowSums(uncorrelated_pair_es),
                              1+alpha_intra*rowSums(uncorrelated_pair_es))

plot(uncorrelated_pair_cs[,1]~uncorrelated_pair_es[,1], pch = 19)
points(uncorrelated_pair_cs[,2]~uncorrelated_pair_es[,2], pch = 19, col = 2)

cov(uncorrelated_pair_es[,1], uncorrelated_pair_cs[,1])
cov(uncorrelated_pair_es[,2], uncorrelated_pair_cs[,2])

cov(correlated_pair_es[,1], correlated_pair_cs[,1])
cov(correlated_pair_es[,2], correlated_pair_cs[,2])
