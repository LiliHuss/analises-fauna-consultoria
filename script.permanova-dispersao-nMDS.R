#Analises-fauna-consultoria
#Banco de análises básicas de fauna para trabalhos de consultoria

library(vegan)
names(data2)
str(data2)
especies<-const[,3:18]
abiot<- const[,2]
fam.matrix<-as.matrix(const[,3:71])#response variables in a sample x species matrix
fam.mat<-sqrt(fam.matrix)#square root transform
fam.prop<-decostand(fam.matrix, method="total") #proportion transformation

#Quantify pairwise compositional dissimilarity between sites based on species occurances.
#Bray-Curtis dissimilarity (abundance weighted)
#Jaccard (presence/absence)
#Gowers (non-continuous variables)

#Dissimilarity: 0 = sites are indentical, 1 = sites do not share any species
#Create a dissimilarity matrix:

fam.dist<-vegdist(fam.mat, method='bray')

#perMANOVA
#Do rainy or dry season collects differ in family composition?

set.seed(36) #reproducible results

fam.div<-adonis2(fam.dist~Estacao, data=const, permutations = 999, method="bray")
fam.div

###################################################
#Multivariate dispersion
#The average distance to group centroid. 
#Used as a measure of multivariate beta diversity.

dispersion<-betadisper(fam.dist, group=const$Estacao)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#####################################################
### NMDS
#Non-metric multi-dimensional scaling. Unconstrained ordination.
#The goal of NMDS is to represent the original position of communities 
#in multidimensional space as accurately as possible using a reduced number
#of dimensions that can be easily visualized. 
#NMDS uses rank orders to preserve distances among objects thus can 
#accomodate a variety of data types.

famMDS<-metaMDS(fam.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
famMDS #metaMDS takes either a distance matrix or your community matrix (then requires method for 'distance=')
#Stress: similarity of observed distance to ordination distance.
#< 0.15 to indidates acceptable fit

stressplot(famMDS)

#####################################################
#GRAFICO

library(ggplot2)

#pull points from MDS
NMDS1 <- famMDS$points[,1] #also found using scores(birdMDS)
NMDS2 <- famMDS$points[,2]
fam.plot<-cbind(esp?cies, NMDS1, NMDS2, abiot)

#plot ordination
p<-ggplot(fam.plot, aes(NMDS1, NMDS2, color=abiot))+
  geom_point(position=position_jitter(.1), shape=3)+#separates overlapping points
  stat_ellipse(type='t',size =1)+ #draws 95% confidence interval ellipses
  theme_minimal()
p

#####################################################
#Fit vectors to ordination
#Which environmental variables are correlated with the ordination?

library(dplyr)

fit<-envfit(famMDS, const[, 3:71])
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)

arrow$especies <- rownames(arrow)
arrow.p<-filter(arrow, P <= 0.05)

p<-ggplot(data=fam.plot, aes(NMDS1, NMDS2))+
  geom_point(data=fam.plot, aes(NMDS1, NMDS2, shape=abiot),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=abiot), alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=especies, lty=especies, color=especies), arrow=arrow(length=unit(.5, "cm")*arrow.p$R)) ##add arrows (scaled by R-squared value)

p
#######################################################################
#Analisando diferencas entre os pontos amostrados 

setwd("C:/Users/PROEX/Downloads")
const2<-read.table("insecta_2.txt", h=T)
summary(const2)
str(const2)
const2$Coleta<-as.factor(const2$Coleta)
const2$Estacao<-as.factor(const2$Estacao)
library(vegan)
names(const2)
str(const2)
especies2<-const2[,3:71]
abiot2<- const2[,2]
fam.matrix<-as.matrix(const2[,3:71])#response variables in a sample x species matrix
fam.mat<-sqrt(fam.matrix)#square root transform
fam.prop<-decostand(fam.matrix, method="total") #proportion transformation

#Quantify pairwise compositional dissimilarity between sites based on species occurances.
#Bray-Curtis dissimilarity (abundance weighted)
#Jaccard (presence/absence)
#Gower?s (non-continuous variables)

#Dissimilarity: 0 = sites are indentical, 1 = sites do not share any species
#Create a dissimilarity matrix:

fam.dist<-vegdist(fam.mat, method='bray')

#perMANOVA
#Do rainy or dry season collects differ family composition?

set.seed(36) #reproducible results

fam.div<-adonis2(fam.dist~Estacao, data=const2, permutations = 999, method="bray")
fam.div

###################################################
#Multivariate dispersion
#The average distance to group centroid. 
#Used as a measure of multivariate beta diversity.

dispersion<-betadisper(fam.dist, group=const2$Estacao)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#####################################################
#NMDS
#Non-metric multi-dimensional scaling. Unconstrained ordination.
#The goal of NMDS is to represent the original position of communities 
#in multidimensional space as accurately as possible using a reduced number
#of dimensions that can be easily visualized. 
#NMDS uses rank orders to preserve distances among objects thus can 
#accomodate a variety of data types.

famMDS<-metaMDS(fam.mat, distance="bray", k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
famMDS #metaMDS takes either a distance matrix or your community matrix (then requires method for 'distance=')
#Stress: similarity of observed distance to ordination distance.
#< 0.15 to indidates acceptable fit

stressplot(famMDS)
nmds<-metaMDS(especies2, distance = "bray")
plot(nmds, type = "t")
#####################################################
### GRAFICO


library(ggplot2)

#pull points from MDS
NMDS1 <- famMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- famMDS$points[,2]
fam.plot<-cbind(esp?cies2, NMDS1, NMDS2, abiot2)

#plot ordination
p<-ggplot(fam.plot, aes(NMDS1, NMDS2, color=abiot2))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p


#####################################################
#Fit vectors to ordination
#Which environmental variables are correlated with the ordination?

library(dplyr)

fit<-envfit(famMDS, const2[, 3:71])
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)

arrow$especies2 <- rownames(arrow)
arrow.p<-filter(arrow, P <= 0.05)

p<-ggplot(data=fam.plot, aes(NMDS1, NMDS2))+
  geom_point(data=fam.plot, aes(NMDS1, NMDS2, shape=abiot2),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=abiot2), alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=esp?cies2, lty=esp?cies2, color=esp?cies2), arrow=arrow(length=unit(.5, "cm")*arrow.p$R)) ##add arrows (scaled by R-squared value)

p

#######################################################################################
#Boxplot visual 
setwd("C:/Users/PROEX/Downloads")
const2<-read.table("insecta_2.txt", h=T)
summary(const2)
str(const2)
const2$Coleta<-as.factor(const2$Coleta)
const2$Estacao<-as.factor(const2$Estacao)
library(vegan)
names(const2)
str(const2)
plot(const2$Abund~const2$Estacao, xlab="Sazonalidade", ylab="Abundancia de Insetos", pch=16, col=c("blue", "orange"))
