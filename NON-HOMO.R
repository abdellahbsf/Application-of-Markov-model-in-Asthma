%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lss=list()
ls=list()
library(msm)
d=read.table(file.choose(),header=TRUE,sep="\t")
ds=d[complete.cases(d[ ,3]),]
for(x in 0:1){
d=ds
k=d[,3]==x
h=c()
z=c()
for (i in 1:length(k)){
if(k[i]==TRUE) h[i]=i
else z[i]=i
}
z=na.omit(z)
z=-as.vector(z)
d=d[z,]
t=statetable.msm(state, ptnum, data = d)
s <- c("controle", "semi-controle","inacceptable")


M <- matrix(data = c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),t[1,3]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,]),t[2,3]/sum(t[2,]),t[3,1]/sum(t[3,]),t[3,2]/sum(t[3,]),t[3,3]/sum(t[3,])), byrow = TRUE,
nrow = 3,dimnames = list(s,s)) 

ls[[x+1]]=M
lss[[x+1]]=t
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deux saison:
library(markovchain)
showClass("markovchainList")
#define a markovchainList


mcA<-new("markovchain",name="MCA", transitionMatrix=ls[[1]])

mcB<-new("markovchain", name="MCB",
transitionMatrix=ls[[2]])

mcList<-new("markovchainList",markovchains=list(mcA, mcB), 
name="Non - homogeneous Markov Chain")

ps <- rmarkovchain(100, mcList,"data.frame")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Non-Homogeneous:::
lss=list()
ls=list()
library(msm)
d=read.table(file.choose(),header=TRUE,sep="\t")
ds=d[complete.cases(d[ ,3]),]
for(x in 0:3){
d=ds
k=d[,3]==x
h=c()
z=c()
for (i in 1:length(k)){
if(k[i]==TRUE) h[i]=i
else z[i]=i
}
z=na.omit(z)
z=-as.vector(z)
d=d[z,]
df=data.frame(y=d[,1])
duplicated2 <- function(x){
  auxi <- do.call("paste",c(x,sep="_"))
  y <- table(auxi)
  y <- y>1
  res <- y[match(auxi,names(y))]
  dimnames(res) <- NULL
  return(res)
}
k=duplicated2(df)
h=c()
z=c()
for (i in 1:length(k)){
if(k[i]==TRUE) h[i]=i
else z[i]=i
}
z=na.omit(z)
z=-as.vector(z)
d=d[z,]

t=statetable.msm(state, ptnum, data = d)
t
s <- c("controle", "semi-controle","inacceptable")
st <- c("controle", "semi-controle")
if(dim(t)[1]==3)

M <- matrix(data = c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),t[1,3]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,]),t[2,3]/sum(t[2,]),t[3,1]/sum(t[3,]),t[3,2]/sum(t[3,]),t[3,3]/sum(t[3,])), byrow = TRUE,
nrow = 3,dimnames = list(s,s))
else 

M <- matrix(data = c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,])), byrow = TRUE,
nrow = 2,dimnames = list(st,st))
ls[[x+1]]=M
lss[[x+1]]=t
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%tout les saison:
library(markovchain)
showClass("markovchainList")
#define a markovchainList


mcA<-new("markovchain",name="MCA", transitionMatrix=ls[[1]])

mcB<-new("markovchain", name="MCB",
transitionMatrix=ls[[2]])

mcC<-new("markovchain", name="MCC",
  	transitionMatrix=ls[[3]])
mcD<-new("markovchain", name="MCD",
  	transitionMatrix=ls[[4]])

mcList<-new("markovchainList",markovchains=list(mcA, mcB, mcC,mcD), 
name="Non - homogeneous Markov Chain")


ps <- rmarkovchain(10, mcList,"data.frame")
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%tout les saison:
library(markovchain)
showClass("markovchainList")
#define a markovchainList


mcA<-new("markovchain",name="MCA", transitionMatrix=ls[[1]])

mcB<-new("markovchain", name="MCB",
transitionMatrix=ls[[2]])

mcC<-new("markovchain", name="MCC",
  	transitionMatrix=ls[[3]])
mcD<-new("markovchain", name="MCD",
  	transitionMatrix=ls[[4]])

mcList<-new("markovchainList",markovchains=list(mcA, mcB, mcC,mcD), 
name="Non - homogeneous Markov Chain")


ps <- rmarkovchain(3, mcList,"data.frame")
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ù
sequence=ps$values
seqM=createSequenceMatrix(sequence,sanitize=FALSE)
mcFitMLE=markovchainFit(data=sequence)




for(j in 1:4){
for(i in dim(lss[[j]])[1]){
lss[[j]][i,]=as.vector( lss[[j]][i,])

}
if(dim(lss[[j]])[1]==3)lss[[j]]=rbind(lss[[j]][1,],lss[[j]][2,],lss[[j]][3,])

else lss[[j]]=rbind(lss[[j]][1,],lss[[j]][2,])

}

verifyHomogeneity(inputList=lss,verbose=TRUE)

ps$values=as.vector(ps$values)
verifyEmpiricalToTheoretical(data=ps$values,object=mc)

for(j in 1:2){
for(i in dim(lss[[j]])[1]){
lss[[j]][i,]=as.vector( lss[[j]][i,])

}
lss[[j]]=rbind(lss[[j]][1,],lss[[j]][2,],lss[[j]][3,])
colnames(lss[[j]])=rownames(lss[[j]])=c("controle", "semi-controle","inacceptable")

}






