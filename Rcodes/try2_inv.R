

# simulated assignment matrix
m1 <- cbind(c(2,3,1,5,10,12),c(20,19,17,6,3,4))
rownames(m1) <- paste0("car",1:6)
colnames(m1) <- paste0("bs",1:2)
print(m1)

# random assignment as a start point
v <- sample(c(1,0), nrow(m1),replace=T)

# function to calculate overall throughput
# equal interval strategy - if k cars are assigned to one bs, each gets the same amount of time for transmission
optf <- function(mm, par, print.each=F){
par.int <- round(par)
pnty <- sum((par.int-par)^2)
th1 <- 1/mm[,1] * par
th2 <- 1/mm[,2] * (1-par)
th1[which(th1==Inf)] <- 0
th2[which(th2==Inf)] <- 0
if(sum(par)==0) v1 <- 0
else v1 <- th1/sum(th1)
if(sum(par)==length(par))v2 <- 0
else v2 <- th2/sum(th2)
if(print.each)print(rowSums(mm*c(v1,v2)))
out <- -sum(mm*c(v1,v2)) + pnty
if(print.each) out <- -sum(mm*c(v1,v2))
out
}

# look at overall throughput of random assignment
# 1 means assigned to bs1; 0 means assigned to bs2
print(v)
print(abs(optf(m1, v)))

# naive solution, take the best bs as a car's assignment
v.max <- apply(m1, 1, which.max)
v.max[which(v.max==2)] <- 0
print(v.max)
print(abs(optf(m1, v.max)))


# numerical optimization to get assignment
res <- optim(par=v, optf, mm=m1, lower=0, upper=1,method="L-BFGS-B")
print(res$par)
print(abs(optf(m1, res$par)))

print(abs(optf(m1, res$par, print.each=T)))
