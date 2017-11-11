
###########################################
# Use this function for criterion
###########################################

mdl.pca<- function(evals, n)
{
  MDLR=MDLC=WKR=WKC=NULL
  m<- length(evals)
  for(i in 1:(m-1))
  {
    MDLR[i]<- 0.5*n*sum(log(evals[1:i])) + 0.5*n*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( m*i - 0.5*i*(i-1) )*log(n)
    MDLC[i]<- 0.5*n*sum(log(evals[1:i])) + 0.5*n*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( i*(2*m - i) )*log(n)
    WKR[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( m*i - 0.5*i*(i-1) )*log(n)
    WKC[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( i*(2*m - i) )*log(n)
  }
  return(list(mdlr=MDLR, mdlc=MDLC, wkr=WKR, wkc=WKC, evals=evals))
}


mdlR.pca<- function(evals, n)
{
  MDLR=MDLC=WKR=WKC=NULL
  m<- length(evals)
  for(i in 1:(m-1))
  {
    # MDLR[i]<- -0.5*(n-m-2)*sum(log(evals[1:i])) - 0.5*(n-m-2)*(m-i)*log(mean(evals[(i+1):m])) + 0.5*log(( m*i - 0.5*i*(i-1) )*n)
    MDLR[i]<- -0.5*(m-1)*sum(log(evals[1:i])) - 0.5*(m-1)*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( m*i - 0.5*i*(i-1) )*log(n)
        
    MDLC[i]<- -0.5*(n-m-2)*sum(log(evals[1:i])) - 0.5*(n-m-2)*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( i*(2*m - i) )*log(n)
    WKR[i]<- (n-m-2)*sum(log(evals[(i+1):m])) - (n-m-2)*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( m*i - 0.5*i*(i-1) )*log(n)
    WKC[i]<- (n-m-2)*sum(log(evals[(i+1):m])) - (n-m-2)*(m-i)*log(mean(evals[(i+1):m])) + 0.5*( i*(2*m - i) )*log(n)
  }
  return(list(mdlr=MDLR, mdlc=MDLC, wkr=WKR, wkc=WKC, evals=evals))
}



aic.pca<- function(evals, n)
{
  MDLR=MDLC=WKR=WKC=NULL
  m<- length(evals)
  for(i in 1:(m-1))
  {
    MDLR[i]<- n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 2*( m*i - 0.5*i*(i-1) )
    MDLC[i]<- n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 2*( i*(2*m - i) )
    WKR[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 2*( m*i - 0.5*i*(i-1) )
    WKC[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + 2*( i*(2*m - i) )
  }
  return(list(mdlr=MDLR, mdlc=MDLC, wkr=WKR, wkc=WKC, evals=evals))
}

bic.pca<- function(evals, n)
{
  MDLR=MDLC=WKR=WKC=NULL
  m<- length(evals)
  for(i in 1:(m-1))
  {
    MDLR[i]<- n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + ( m*i - 0.5*i*(i-1) )*log(n)
    MDLC[i]<- n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + ( i*(2*m - i) )*log(n)
    WKR[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + ( m*i - 0.5*i*(i-1) )*log(n)
    WKC[i]<- -n*sum(log(evals[1:i])) + n*(m-i)*log(mean(evals[(i+1):m])) + ( i*(2*m - i) )*log(n)
  }
  return(list(mdlr=MDLR, mdlc=MDLC, wkr=WKR, wkc=WKC, evals=evals))
}

#####################################
## data analysis
####################################

library(vegan)
n<-44
evals<- eigen( cov( t(varespec) ) )$values
plot( mdl.pca(evals, n)$mdlr )
plot( mdl.pca(evals, n)$wkr )

n<-21
evals<- eigen( cov( varechem ) )$values
plot( mdl.pca(evals, n)$mdlr )
plot( mdl.pca(evals, n)$wkr )


n<-30
evals<- eigen( cov( t(dune) ) )$values
plot( mdl.pca(evals, n)$mdlr )
plot( mdl.pca(evals, n)$wkr )
evec<- eigen( cov( t(dune) ) )$vectors[,1:3]
pca.scores<- t(dune)%*%evec%*%diag(1/sqrt(evals[1:3]))

cor(pca.scores, dune)

z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)

pc1 <- pca.scores[,1]
pc2 <- pca.scores[,2]
pc3 <- pca.scores[,3]

# pc1=x, pc2=y, pc3=z

scatterplot3d(pc1, pc2, pc3, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="scatterplot3d - 1", pch=20)
scatterplot3d(pc1, pc2, pc3, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="scatterplot3d - 1", pch=colnames(dune))

########################################################
## some data analysis from psychometric study
#######################################################


# Bechtoldt 6-factor, Bechtoldt.1, Bechtoldt.2
# Holzinger 4-factor
# Harman 4-factor
require(psych)

n<-212
evals<- eigen(Bechtoldt.1)$values
kpmdl<- mdl.pca(evals, n)$mdlr
wkmdl<- mdl.pca(evals, n)$wkr

postscript("~/Bechtoldt.eps", horizontal=FALSE, height=2.5, width=5.5)
par(mfrow=c(1,2), mar=c(1.5,1.75,1.5,1)+0.5, oma=c(1.5,1.5,0,0))

plot( 1:16, kpmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[KP](k)) , cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

plot( 1:16, wkmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[WK](k)), cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

dev.off()


n<-213
evals<- eigen(Bechtoldt.2)$values
plot( mdl.pca(evals, n)$mdlr )
plot( mdl.pca(evals, n)$wkr )


n<-213
evals<- eigen(Bechtoldt.2)$values
kpmdl<- mdl.pca(evals, n)$mdlr
wkmdl<- mdl.pca(evals, n)$wkr

postscript("~/Bechtoldt2.eps", horizontal=FALSE, height=2.5, width=5.5)
par(mfrow=c(1,2), mar=c(1.5,1.75,1.5,1)+0.5, oma=c(1.5,1.5,0,0))

plot( 1:16, kpmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[KP](k)) , cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

plot( 1:16, wkmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[WK](k)), cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

dev.off()



n<-355
evals<- eigen(Holzinger)$values
kpmdl<- mdl.pca(evals, n)$mdlr
wkmdl<- mdl.pca(evals, n)$wkr

postscript("~/Holzinger.eps", horizontal=FALSE, height=2.5, width=5.5)
par(mfrow=c(1,2), mar=c(1.5,1.75,1.5,1)+0.5, oma=c(1.5,1.5,0,0))

plot( 1:13, kpmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[KP](k)) , cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

plot( 1:13, wkmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[WK](k)), cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

dev.off()


n<- 145
evals<- eigen(Harman74.cor$cov)$values
kpmdl<- mdl.pca(evals, n)$mdlr
wkmdl<- mdl.pca(evals, n)$wkr

postscript("~/Harman.eps", horizontal=FALSE, height=2.5, width=5.5)
par(mfrow=c(1,2), mar=c(1.5,1.75,1.5,1)+0.5, oma=c(1.5,1.5,0,0))

plot( 1:23, kpmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[KP](k)) , cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

plot( 1:23, wkmdl, type="o", cex=0.5, cex.axis=0.8, xlab="", ylab="")
mtext( side=2, bquote( MDL[WK](k)), cex=0.75, line=1.9  )
mtext( side=1, bquote( k ) , cex=0.8, line=1.8  )

dev.off()




