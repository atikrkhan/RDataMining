###########################################################################
### Cross Tabulation and Frequency Tables
###########################################################################

xtabs(~ cyl + gear, mtcars) # cross tabs of variables cyl and gear for data frame mtcars
head(mtcars,3) # displays first three rows in the data frame
?mtcars  # what is mtcars, is used  to open help/description file regarding mtcars      

xtabs(~ cyl + gear + vs, mtcars) # three way cross tab

# Frequency tables

install.packages("plyr")
library(plyr)
count(mtcars, c('cyl', 'gear', 'vs'))

# frequency table with percentage

y = count(mtcars, 'gear')
prop = function(x)
{
  x$prop = x$freq/sum(x$freq)
  x$perc = x$freq/sum(x$freq)*100
  
  return(x)
}
prop(y)

####################################################################
### Pie Chart, Bar diagram, histogram, density plots
####################################################################

box.data = read.csv(file="E:/RDataMining/boxplotdata.csv", header=T)
library(ggplot2)
library(gridExtra)
ggplot(aes(y = HBR, x = District, fill = Company[4]), data = box.data) + geom_boxplot() + theme_bw() + theme( panel.background = element_blank()) + theme(legend.position = "none") + scale_fill_manual(name = "This is my title", values = "white", labels="Company")
ggplot(aes(y = HBR, x = District, fill = Company[4]), data = box.data) + geom_boxplot() + theme_bw() + theme( panel.background = element_blank()) + theme(legend.position = "none") + scale_fill_manual(name = "This is my title", values = "green", labels="Company")

text<- "1. Dhaka, 2. Rajshahi, 3. Sylhet, 4. Sirajganj, 5. Chittagong, 6. Tangail, 7. Bogra, 8. Rangpur, 9. Rangamati"
p.hbra<- ggplot(aes(y = HBR, x = DistrictCode, fill = Company), data = box.data) + geom_boxplot() + theme_bw() + theme( panel.background = element_blank()) + facet_grid(.~Company) + guides(fill=FALSE) + xlab(text) + ylab("HBRA Counts")
bp.hbra<- p.hbra+theme(axis.text=element_text(size=10),axis.title.x=element_text(size=5.25), axis.title.y=element_text(size=7))
#p+theme(axis.text=element_text(size=10),axis.title.y=element_text(size=7))

#pdf("boxplot_company_district_hbra.pdf",width=6.5,height=4)
bp.hbra 
#dev.off()




####################################################################
### Principal component analysis
####################################################################
?prcomp
require(graphics)
?USArrests
head(USArrests,3)
prcomp(USArrests, scale = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))

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

adelgedata<- read.csv(file="E:/RDataMining/adelges_data.csv", header=FALSE)

n<-40
evals<- eigen(adelgedata)$values
plot( mdl.pca(evals, n)$mdlr )
plot( mdl.pca(evals, n)$wkr )




###################################################################
#### Clustering
###################################################################

carsdata<- read.table("E:/RDataMining/cars.tab", sep="\t", header=TRUE)
carsdata = read.delim("E:/RDataMining/cars.tab",stringsAsFactors=FALSE)
cars.use = carsdata[,-c(1,2)]
medians = apply(cars.use,2,median)
mads = apply(cars.use,2,mad)
cars.use = scale(cars.use,center=medians,scale=mads)
cars.dist = dist(cars.use)
cars.hclust = hclust(cars.dist)
plot(cars.hclust,labels=cars$Car,main='Default from hclust')


# 
library(cluster)
data(votes.repub)

agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)

op <- par(mfrow=c(2,2))
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)


data(animals)
aa.a  <- agnes(animals) # default method = "average"
aa.ga <- agnes(animals, method = "gaverage")
op <- par(mfcol=1:2, mgp=c(1.5, 0.6, 0), mar=c(.1+ c(4,3,2,1)),
          cex.main=0.8)
plot(aa.a,  which.plot = 2)
plot(aa.ga, which.plot = 2)
par(op)


###################################################################
#### Generalized Linear Models
###################################################################




###################################################################
### Generalized Linear Mixed Models
###################################################################























