
# function to calculate overall throughput
# equal interval strategy - if k cars are assigned to one bs, each gets the same amount of time for transmission
optf <- function(mm, param){
if(sum(param)==0)v1 <- rep(0, length(param))
else v1 <- param/sum(param)
if(sum(param)==length(param))v2 <- rep(0, length(param))
else v2 <- (1-param)/(length(param)-sum(param))
-sum(mm*c(v1,v2))
}

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


simu1 <- function(){
# simulated assignment matrix
#m1.r <- matrix(sample(1:100, 12)/10 , ncol=2)
m1.r <- cbind(c(sample(80:100,3), sample(1:40,3)),
								c(sample(1:40,3),sample(3:60, 3)))/100
#ww <- which.min(rowMeans(m1.r))
#ww <- 4
m1 <- m1.r
#m1[ww,] <- sample(1:100,2)/100
rownames(m1) <- paste0("car",1:6)
colnames(m1) <- paste0("bs",1:2)
#print(m1)

# random assignment as a start point
v <- sample(c(1,0), nrow(m1),replace=T)
tp.sample <- abs(optf(m1, v))

# naive solution, take the best bs as a car's assignment
v.max <- apply(m1, 1, which.max)
v.max[which(v.max==2)] <- 0
tp.max <- abs(optf(m1, v.max))


# numerical optimization to get assignment
res <- optim(par=v, optf, mm=m1, lower=0, upper=1,method="L-BFGS-B")
v.res <- res$par
tp.res <- abs(optf(m1, round(res$par)))
 
# alternative

res.alt <- optim(par=v, optf.gen, mm=m1, lower=0, upper=1,method="L-BFGS-B")
v.res.alt <- res.alt$par
tp.res.alt <- abs(optf(m1, round(res.alt$par)))

outmat <- c (tp.sample,tp.max, tp.res, tp.res.alt)
print(outmat)
v.mat <- cbind(v,v.max, v.res, v.res.alt)
out <- list(data=m1, outmat=outmat, v.mat=v.mat)
}




run100 <- sapply(1:100, function(i)simu1(),simplify=F)

cp100 <- sapply(run100, function(i)i$outmat[1:4])

#pdf("simu_equal_t_cdf_2bs_6c.pdf")
plot(ecdf(cp100[1,]),xlim=c(0,1.7), xlab="throughput", ylab="cdf", main="equal t")
plot(ecdf(cp100[2,]),add=T, col="blue")
plot(ecdf(cp100[3,]),add=T, col="red")
plot(ecdf(cp100[4,]),add=T, col="grey")
legend("topleft",lwd=2, c("input order","max","opt"),col=c("black","blue","red"))
#dev.off()








