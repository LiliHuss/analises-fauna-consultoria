#Analises-fauna-consultoria
#Banco de análises básicas de fauna para trabalhos de consultoria

#Métricas de fauna por Unidade Amostral
#Dados
setwd("")
dir()
data <- read.csv("arquivo-amostras-estacao-area-campanha-riqueza.csv", h=T)

attach(data)
names(data)
dim(data)

especies <- data[,4:ncol(data)]

#Indice de Simpson - Mais interpretavel 1-D
#D=somatorio(pi^2)
library(vegan)

dim(data)

#Dividindo dados por estação
dados.separados<-split(data, Estacao)

dados.seca<-dados.separados$Seca
dados.chuvosa<-dados.separados$Chuvosa

summary(dados.seca)
summary(dados.chuvosa)

#Transformando os dados em fatores:
dados.seca$Areas<-as.factor(dados.seca$Areas)
dados.seca$Estacao<-as.factor(dados.seca$Estacao)
dados.seca$Campanha<-as.factor(dados.seca$Campanha)

dados.chuvosa$Areas<-as.factor(dados.chuvosa$Areas)
dados.chuvosa$Estacao<-as.factor(dados.chuvosa$Estacao)
dados.chuvosa$Campanha<-as.factor(dados.chuvosa$Campanha)

dados.seca<-aggregate(.~ Areas, dados.seca, sum)
dados.chuvosa<-aggregate(. ~ Areas, dados.chuvosa, sum)

dados.seca
dados.chuvosa

dim(dados.seca)
dim(dados.chuvosa)

#Somando os pi
soma.seca<-colSums(dados.seca[,4:ncol(dados.seca)])
soma.chuvosa<-colSums(dados.chuvosa[,4:ncol(dados.chuvosa)])
soma.total<-colSums(data [,4:ncol(data)])

soma.total
soma.seca
soma.chuvosa

#Indices de diversidade
?diversity

#Calculo de Riqueza
specnumber(soma.seca)
specnumber(soma.chuvosa)
specnumber(soma.total)

#Indice de Shannon
diversity(soma.seca, index="shannon")
diversity(soma.chuvosa, index="shannon")
diversity(soma.total, index = "shannon")

#Indice de Simpson 1-D
diversity(soma.seca, index="simpson")
diversity(soma.chuvosa, index="simpson")
diversity(soma.total, index = "simpson")

#Indice de Simpson Inv
diversity(soma.seca, index="inv")
diversity(soma.chuvosa, index="inv")
diversity(soma.total, index = "inv")

#Pielou equitabilidade
diversity(soma.seca)/log(specnumber(soma.seca))
diversity(soma.chuvosa)/log(specnumber(soma.chuvosa))
diversity(soma.total)/log(specnumber(soma.total))

#Dendrograma areas
dir()

data2<- read.csv("arquivo-amostras-estacao-area-campanha-riqueza.csv", h=T, row.names=1)

dim(data2)

data2

dendrograma1<-vegdist(data2 [,4:ncol(data2)], method = "bray")

dendrograma1

graphic<-hclust(dendrograma1, method = "ward.D2")

cutting<-cutree(graphic, k = 2)

plot(graphic, xlab = "Pontos Amostrais", ylab = "Distancia", hang = -1)

rect.hclust(graphic, k=2, border = "green")

#Perfis de Diversidade - Renyi e Hill
perfil.seca<-renyi(soma.seca, hill=T)
perfil.chuvosa<-renyi(soma.chuvosa, hill=T)

#Depois que gerar o grafico, tem que clicar onde voce quer que fique a legenda. O R fica esperando você clicar, enquanto nao clicar ele nao sai da tela. 

plot.default(perfil.seca, type = "l", col="red", lty=2, ylab="Perfil de Diversidade", xlab="Coletas", bty="l", ylim=c(0,20), xlim=c(0, 11))
points(perfil.seca, col="red", pch=16)
points(perfil.chuvosa, type = "l", col="green", lty=2)
points(perfil.chuvosa, col="green", pch=16)
legend(locator(1), c("Seca", "Chuvosa"), col = c("red","green"), fill = c(2,3), bty="n")

################################################################
##Curva do coletor
#pontos amostrais
dim(data2)

plot(specaccum(data2[,4:ncol(data2)], method = "rarefaction", ci.type = "polygon", ci.col="#00000050", lwd=1, permutations = 100000), xlab="Pontos Amostrais", ylab="Numero de especies", bty="l", ylim = c(1, 20), xaxt = 'n',xaxs="r",yaxs="i")
axis(side=1,at=seq(1,4), las=1)

plot(specaccum(data2[,4:ncol(data2)], method = "collector"), xlab="Pontos Amostrais", ylab="Numero de especies", bty="l", ylim = c(1,20), xaxt = 'n',xaxs="r",yaxs="i")
axis(side=1,at=seq(1,4), las=1)

#curvas de rarefacao por estacoes
pool1<-specaccum((dados.seca[,4:ncol(dados.seca)]),method="rarefaction", permutations = 10000)

pool1.1<-specaccum((dados.chuvosa[,4:ncol(dados.chuvosa)]),method="rarefaction", permutations = 10000)

plot(pool1, ci.type="polygon", ci.col="#00000050", lwd=1, xlab="Pontos Amostrais", ylab="Numero de Especies", bty="l", xlim=c(1,4), ylim=c(0,20),  xaxt = 'n',xaxs="i",yaxs="i")
plot(pool1.1, ci.type="polygon", ci.col="#FF000050", lwd=1, add=T)
axis(side=1,at=seq(1,4), las=1)
legend(locator(1), c("Seca", "Chuvosa"), fill=c(1,2), bty="n") 

#################################################################
##Curva de acumulo de especies - rarefacao ou simplesmente curva de rarefacao

#Pontos amostrais unicos

pool2 <- poolaccum(data2[,1:ncol(data2)])
summary(pool2)
plot(pool2, display = c("S", "jack1", "boot"), ylab="Riqueza", xlab="Pontos Amostrais", col="black")

#Testando a funcao Diversityresult do package DiversityR

library(vegan)
library(BiodiversityR)

diversityresult(data2[,4:ncol(data2)], y=NULL, index="richness", method="each site",sortit=FALSE, digits=3)
diversityresult(data2[,4:ncol(data2)], y=NULL, index="Shannon", method="each site",sortit=FALSE, digits=3)
diversityresult(data2[,4:ncol(data2)], y=NULL, index="Simpson", method="each site",sortit=FALSE, digits=3)
diversityresult(data2[,4:ncol(data2)], y=NULL, index="inverseSimpson", method="each site",sortit=FALSE, digits=3)
diversityresult(data2[,4:ncol(data2)], y=NULL, index="Jevenness", method="each site",sortit=FALSE, digits=3)
diversityresult(data2[,4:ncol(data2)], y=NULL, index="Logalpha", method="each site",sortit=FALSE, digits=3)

###################################################################
# NMDS para ver diferencas na composicoes da fauna entre as estacoes 

setwd("C:/Users/thiag/Desktop/R")
const<-read.table("insecta_NMDS.txt", h=T)
summary(const)
str(const)
const$Coleta<-as.factor(const$Coleta)
const$Estacao<-as.factor(const$Estacao)