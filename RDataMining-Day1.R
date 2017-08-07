
###########################################################################################################################
# This file has been prepared and used by Atikur R. Khan for "Data Mining with R Workshop"
# at Population Gallery, RU on 7-10 August 2017.
# Day 1: R Basics, Day 2: R for Forecasting, Day 3: R for Marketing Demography, Day 4: R for Social Media Analytics 
###########################################################################################################################

# 1. Installation of R
# 2. Installation of RStudio
# 3. Packages and Libraries


#########################################################################
#### Basic operations
#########################################################################

x = 2
y = 3
x + y # addition
x - y # subtraction
x * y # multiplication
x / y # division


x^2 # square
x^(1/2) # square root



x > y # TRUE or FALSE
x < y 
x == y
x == (y - 1)
x >= y
x <= y
x >= (y - 1)
(x - y) > 0
abs(x - y) > 0



z = log(x) # natural logarithm
exp(z)     # exponentiation, anti-logarithm

# cos(x) where x is in redian, not degrees

cos(0*pi/180) # cos for 0 degrees
cos(90*pi/180) # result for cos for 90 degrees

#########################################################
### Vectors and Matrices
#########################################################
x = 1: 10
length(x)
x[1] # indexing starts at 1
x[10]
is.vector(x) # is this a vector

plot(x, x^2, type="l", xlab = "x", ylab = "x^2")

plot(x, x^2, type="l", xlab = "x", ylab = bquote(x^2))

x = 1:100; plot(cos(x), sin(x), type="l", xlab = bquote(cos(x)), ylab = bquote(sin(x)), main = "cos(x) vs sin(x)" )

y = runif(3,0,1); y

x = runif(6,0,1)
X = matrix(x, 3,2); X
t(X) # transpose of matrix
is.matrix(X)
is.vector(X)
is.vector(x)

y; X

t(X)%*%X # X'X

library(MASS)
ginv(t(X)%*%X) # inverse of X'X

t(X)%*%y # X'y

beta = ginv(t(X)%*%X) %*% t(X)%*%y # regression coefficient
beta

############################################################
### Loops and conditional statements
############################################################

x = 1:5
length(x)

for (i in 1:length(x))
{
  print( x[i] )
}




for (i in 1:length(x))
{
  if( x[i] > 3 ) print( x[i] )
}


for (i in 1:length(x))
{
  if( x[i] > 3 ) 
  {
       print( x[i] )
  }
  else
  {
    print( x[i] +2 )
  }
}

a = 2
b = 3
ifelse( (a + b) > 3, a, b  )

ifelse( (a + b) < 3, a, b  )

u = runif(1, 0, 1)
v = rnorm(1, 0, 1)
ifelse( (a + b) > 3, u, v  )
ifelse( (a + b) < 3, u, v  )

ifelse( (a + b) < 3, runif(1,0,1), rnorm(1,0,1)  )



for (i in 1:length(x))
{
  ifelse( x[i] > 3, print( x[i]), print( x[i]+2 )  )
}


for (i in 1:length(x))
{
  if( x[i] > 3 ) 
  {
    print( x[i] )
  }
  else
  {
    print( x[i] +2 )
  }
}

###########################################################
### Function
###########################################################

qt = function(x, level)
{
  return( quantile(x,probs=level) )
}

xx = rnorm(10,0,1)
qt(xx, c(0.025, 0.975) )


qtt975 = function(x)
{
  return( quantile(x,probs=0.975) )
}
xxdt = matrix( rnorm(50,0,1),10,5  )

apply(xxdt, 2, qtt975 )


qtt025 = function(x)
{
  return( quantile(x,probs=0.0255) )
}
xxdt = matrix( rnorm(50,0,1),10,5  )

apply(xxdt, 2, qtt025 )

plot( apply( xxdt,2,mean  ), ylim=c( min( xxdt ), max( xxdt ) )   )
lines( apply(xxdt, 2, qtt025 ), type="p", col=2    )
lines( apply(xxdt, 2, qtt975 ), type="p", col=2    )





############################################################
### Data Frame
############################################################

y = runif(10,0,1)
x = rbinom(10,1,0.5)
cbind(x,y)

xydf = data.frame( x = factor(x),  score = y)
is.data.frame(xydf)
colnames(xydf)
levels(xydf$x)
xydf

levels(xydf$x) = c("M", "F")
colnames(xydf) = c("Gender", "Score")
xydf

#########################################################################
#### Loading data, processing, and fitting linear model
#########################################################################

sugar<-read.csv("E:/RDataMining/sugar.csv",header=T)
structure(sugar)
is.data.frame(sugar)
colnames(sugar)

head(sugar)

sugar$Cane.Growing.Region

Region1 = (sugar$Cane.Growing.Region==1)*1
Region2 = (sugar$Cane.Growing.Region==2)*1
Region3 = (sugar$Cane.Growing.Region==3)*1
Region4 = (sugar$Cane.Growing.Region==4)*1

sugar$reg1 = Region1
sugar$reg2 = Region2
sugar$reg3 = Region3
sugar$reg4 = Region4
sugar$logreciepts = log(sugar$Receipts)
sugar$logharvest = log(sugar$Sugar.Cane.Harvest)
sugar$logcosts = log(sugar$Costs)
sugar$area = sugar$Sugar.Cane.Area


head(sugar)

y = sugar$logreciepts
n<-length(y)

X<-as.matrix(cbind(rep(1,n),sugar$reg2, sugar$reg3, sugar$reg4, sugar$area, sugar$logharvest,sugar$logcosts))
n<- nrow(X)
d<- ncol(X)

n; d
dim(X)

XX = t(X)%*%X
XX.INV = solve(XX)
Xy = t(X)%*%y
Beta = XX.INV%*%Xy
Beta

linear.fit<- lm( sugar$logreciepts ~ sugar$reg2 + sugar$reg3 + sugar$reg4 + sugar$area + sugar$logharvest + sugar$logcosts)
beta.ls<- linear.fit$coefficients
beta.ls

summary(linear.fit)

beta.ls.var<- diag(  ( summary(linear.fit)$sigma )^2*summary(linear.fit)$cov.unscaled )

summary(linear.fit)$coefficients

round( summary(linear.fit)$coefficients,4 )

write.csv( round( summary(linear.fit)$coefficients,4 ), file = "E:/RDataMining/sugar_lm_results.csv"    )

#########################################################################
###  COMPUTING diagonal matrix D  with  elements d_i
#########################################################################

XX<-t(X)%*%X
XX.INV<- solve(XX)
D.diag<- apply( X,1, function( z ){  di.dnum<- sqrt( t(z)%*%XX.INV%*%z ); di.num<- min(  di.dnum, sqrt(d/n)   );  di.num/di.dnum    }    )

# di.dnum<- sqrt( t(X[1,])%*%XX.INV%*%X[1,] ); di.num<- min(  di.dnum, sqrt(d/n)   );  di.num/di.dnum  # verifies that D.diag is correct. 
# di.dnum<- sqrt( t(X[338,])%*%XX.INV%*%X[338,] ); di.num<- min(  di.dnum, sqrt(d/n)   );  di.num/di.dnum


#########################################################################
### LS: Use linear model fit to get least squares estimates
#########################################################################

linear.fit<- lm(y~ Region2 + Region3 + Region4 + Area + log.Harvest + log.Costs)
beta.ls<- linear.fit$coefficients
beta.ls.var<- diag(  ( summary(linear.fit)$sigma )^2*summary(linear.fit)$cov.unscaled )


# res<- linear.fit$residuals
# sigma<- median( abs ( res - median(res) ) )/0.6745
# psi.1<- sign(res)*pmin( abs(res/sigma), 1.345) 
# psi.w<- psi.1/(res/sigma) 
# weights<- psi.w*(D.diag/sigma) # primary weights, W_0= diag(weights)

#########################################################################
#### IRWLS: Use LS estimates "beta.ls" as initial estimate 
#########################################################################

re.weight<- function(z, D)
{
  sigma<- median( abs ( z - median(z) ) )/0.6745
  psi.1<- sign(z)*pmin( abs(z/sigma), 1.345) 
  psi.w<- psi.1/(z/sigma) 
  weights<- psi.w*(D/sigma) 
  return(weights)
}

est.iter<- function(D, max.iter, accu.level, beta.initial, X, y)
{
  d<- length(beta.initial)
  BETA= matrix(NA, d, max.iter )
  ACCU=NULL
  beta.pre<- beta.initial
  
  for(i in 1:max.iter)
  {
    res<- drop( y - X%*%beta.pre )
    weights<- re.weight(res, D)
    W<- diag(weights)
    beta.post<- solve( t(X)%*%W%*%X )%*%t(X)%*%W%*%y 
    accuracy<- sqrt( sum( (beta.post - beta.pre)*(beta.post - beta.pre) )/sum(beta.pre^2) )
    BETA[,i]<- beta.post
    ACCU[i]<- accuracy
    if( accuracy <= accu.level )
    {   
      sigma<- median( abs ( res - median(res) ) )/0.6745
      beta.irwls.var<- diag(   solve( t(X)%*%W%*%X )%*% (  t(X)%*% W%*% W%*% X    ) %*% solve( t(X)%*%W%*%X )*sigma^2 )
      est.out<- list( beta.initial=beta.initial, beta.irwls=beta.post, beta.irwls.var=beta.irwls.var, conv.iter=i, weights=weights, d.weights=D.diag, sigma=sigma,residuals=res, accuracy=accuracy )
      return(est.out)
    }    
    beta.pre<- beta.post
  }
  if(accuracy > accu.level) return("Does not converge")
}
 # use LSE as initial estimate
linear.fit<- lm(y~ Region2 + Region3 + Region4 + Area + log.Harvest + log.Costs)
beta.ls<- linear.fit$coefficients
beta.ls.var<- diag(  ( summary(linear.fit)$sigma )^2*summary(linear.fit)$cov.unscaled )

# use LSE as initial estimate
iter.out<- est.iter(D=D.diag, max.iter=50, accu.level=10^(-10), beta.initial=beta.ls, X, y)
beta.irwls.var<- iter.out$beta.irwls.var
beta.irwls<- iter.out$beta.irwls


###############################################################
#### Generalized Linear Models
###############################################################

HERS <- read.csv("E:/RDataMining/hersdata.csv", na.strings="")

hers.data<- data.frame( HERS$age, HERS$raceth, HERS$nonwhite,
                        HERS$smoking, HERS$drinkany, HERS$exercise,
                        HERS$diabetes, HERS$statins, HERS$medcond )
colnames(hers.data)<- c("age", "raceth", "nonwhite", "smoking",
                        "drinkany", "exercise", "diabetes",
                        "statins", "medcond")
nrow(hers.data)
ncol(hers.data)
hers.clean<- na.omit(hers.data)
summary(glm(hers.clean$medcond ~ hers.clean$age + hers.clean$diabetes
            + hers.clean$exercise + hers.clean$drinkany,
            family=binomial(link="logit")))$coef




DebTrivedi<- load("C:/Users/atikur/Desktop/tryme/DebTrivedi.rda")
mydata <- DebTrivedi[, c(1, 6:8, 13, 15, 18)]
colnames(mydata)
summary( glm(ofp~ hosp + health + numchron + gender + school
             + privins, mydata, family=poisson) )












