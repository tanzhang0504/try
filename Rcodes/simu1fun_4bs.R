
# function to calculate overall throughput
# equal interval strategy - if k cars are assigned to one bs, each gets the same amount of time for transmission
optf.gen <- function(mm, param){
# if param is assignment, 1...10 etc
	nc <- ncol(mm)
	#lam <- param[1]*sum(mm)
	#p1 <- matrix(param[-1],ncol=nc-1)
	p1 <- matrix(param,ncol=nc-1)
	p2 <- cbind(p1, 1-p1)
	pen <- sum((p2-round(p2))^2)
	#p2 <- cbind(p1, 1-rowSums(p1))
	aa <- sapply(1:nc,function(i)p2[which(p2[,i]!=0),i] ,simplify=F)
	bb <- sapply(aa, function(i) i * (1/sum(i)),simplify=F)
	cc <- sapply(1:nc, function(i)mm[which(p2[,i]!=0),i] * bb[[i]])
	-sum(unlist(cc)) #+ lam*pen
}


simu1.4bs <- function(){
# simulated assignment matrix
m1.r <- cbind(c(sample(80:100,3), sample(1:40,3)),
								c(sample(1:40,3),sample(3:60, 3)))/100
m2 <- matrix(sample(1:50, 12)/100, ncol=2)
m1 <- cbind(m1.r, m2)
rownames(m1) <- paste0("car",1:6)
colnames(m1) <- paste0("bs",1:4)
#print(m1)

# random assignment as a start point
v0 <- sample(c(1:4), nrow(m1) ,replace=T)
tmpmat <- matrix(0,nrow=nrow(m1),ncol=ncol(m1))
for(i in 1:4)tmpmat[which(v0==i),i]=1
v <- as.vector(tmpmat[,1:3])
tp.sample <- abs(optf.gen(m1, tmpmat[,1:3]))

# naive solution, take the best bs as a car's assignment
v.max <- apply(m1, 1, function(i)which.max(i)[1])
tmpmat.max <- matrix(0,nrow=nrow(m1),ncol=ncol(m1))
for(i in 1:4)tmpmat.max[which(v.max==i),i]=1
v.max.use <- as.vector(tmpmat.max[,1:3])
tp.max <- abs(optf.gen(m1, v.max.use))


res.alt <- optim(par=v, optf.gen, mm=m1, lower=0, upper=1,method="L-BFGS-B")
v.res.alt <- res.alt$par
tp.res.alt <- abs(optf.gen(m1, round(res.alt$par)))
mat.res0 <- matrix(res.alt$par,ncol=3)
mat.res <- cbind(mat.res0, 1-rowSums(mat.res0))
v.res <- apply(mat.res, 1, function(i)which.max(i)[1])

outmat <- c (tp.sample,tp.max,  tp.res.alt)
print(outmat)
v.mat <- cbind(v0,v.max,v.res)
out <- list(data=m1, outmat=outmat, v.mat=v.mat)
}




run100 <- sapply(1:100, function(i)simu1(),simplify=F)

cp100 <- sapply(run100, function(i)i$outmat)

pdf("simu_equal_t_cdf_4bs_6c.pdf")
plot(ecdf(cp100[1,]),xlim=c(0,1.7), xlab="throughput", ylab="cdf", main="equal t")
plot(ecdf(cp100[2,]),add=T, col="blue")
plot(ecdf(cp100[3,]),add=T, col="red")
#plot(ecdf(cp100[4,]),add=T, col="grey")
legend("topleft",lwd=2, c("input order","max","opt"),col=c("black","blue","red"))
dev.off()








