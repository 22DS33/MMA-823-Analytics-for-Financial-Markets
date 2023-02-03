

require(xts)
require(YieldCurve)
data(FedYieldCurve)


mat.Fed<-c(3/12, 0.5, 1,2,3,5,7,10)
par(mfrow=c(2,3))

for( i in c(1,2,3,370,371,372) ){
   plot(mat.Fed, FedYieldCurve[i,], type="o",
         xlab="Maturities structure in years", ylab="Interest rates values")
   title(main=paste("Federal Reserve yield curve observed at",
                     time(FedYieldCurve[i], sep=" ") ))
   grid()
}


maturity =c(0.25, 0.5, 1, 2, 3, 5, 7, 10)


# 3D graph
persp(1982:2012, mat.Fed, as.matrix(FedYieldCurve[seq(2,nrow(FedYieldCurve),by=12),]),
      theta=30, xlab="Year", ylab="Maturity (in years)",
      zlab="Interest rates (in %)",ticktype = "detailed",shade=.2,expand=0.3)



# Principal Component Analysis

M <- as.matrix(FedYieldCurve)
pca.rates <- princomp(M, scale=TRUE)

summary(pca.rates)
summary(pca.rates)

factor.loadings <- pca.rates$loadings[,1:3]
matplot(maturity,factor.loadings,type="l", lwd=c(2,1,1),
          lty=c(1,1,2),xlab = "Maturity (in years)", ylab = "Factor loadings")



vtime=seq(1981+11/12,length=nrow(M),by=1/12)


plot(vtime,pca.rates$scores[,1],type="l",xlab="Year")
plot(vtime,pca.rates$scores[,2],type="l",xlab="Year")
plot(vtime,pca.rates$scores[,3],type="l",xlab="Year")




