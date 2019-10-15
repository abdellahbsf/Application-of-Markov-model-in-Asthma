%%%%%%%%%%%%%discret%%%%%%%%%%%%%

t=as.matrix(t)
s <- c("controle", "semi-controle","inacceptable")
myFit<-markovchainFit(data=sDays,confidencelevel = .9,method = "mle")

matrix <- matrix(data = c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),t[1,3]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,]),t[2,3]/sum(t[2,]),t[3,1]/sum(t[3,]),t[3,2]/sum(t[3,]),t[3,3]/sum(t[3,])), byrow = TRUE,
nrow = 3,dimnames = list(s,s))
mcAsthma <- new("markovchain", states = s,
byrow = TRUE, transitionMatrix = matrix,
name = "Asthma")
%%%%%%%%%%%%%%Simulation:
sDays <- rmarkovchain(n = 453, object = mcAsthma, t0 = "controle")


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Discret:
d=read.table(file.choose(),header=TRUE,sep="\t")
library("markovchain")
library(msm)
t=statetable.msm(state, ptnum, data = ds)
t
rownames(t) <- colnames(t) <- c("controle", "semi-controle","inacceptable")
mc <- new("markovchain", states = c("controle", "semi-controle","inacceptable"),
 transitionMatrix = matrix(data =  c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),t[1,3]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,]),t[2,3]/sum(t[2,]),t[3,1]/sum(t[3,]),t[3,2]/sum(t[3,]),t[3,3]/sum(t[3,])),byrow = TRUE, nrow = 3),
 name = "Asthma")
print(mc)
steadyStates(mc)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%continue%%%%%%%%%%%%%%
d=read.table(file.choose(),header=TRUE,sep="\t")
library(msm)
t=statetable.msm(state, ptnum, data = d)
t
Q <- rbind(c(0, 0.03333,0.0715385), c(.280 , 0, 0.320), c(.23050,0.3677, 0))
rownames(Q) <- colnames(Q) <- c("controle", "semi-controle","inacceptable")
ini matrix default:
Q.crude <- crudeinits.msm(state ~ timeC, ptnum, data=d, qmatrix=Q)
asthma.msm <- msm(state ~ timeC, subject = ptnum, data = d, qmatrix =Q,exacttime=TRUE,method = "BFGS", control = list(fnscale = 4000, maxit = 10000))
asthma.msm2 <- msm(state ~ timeC, subject = ptnum, data = d,qmatrix =Q, gen.inits=TRUE,pci=1/4,method = "BFGS", control = list(fnscale = 4000, maxit = 10000))
asthma.msm3 <- msm(state ~ timeC, subject = ptnum, data = d,qmatrix =Q, gen.inits=TRUE,covariates = ~ IMC+all_pous,method = "BFGS", control = list(fnscale = 4000, maxit = 10000))
asthma.msm4 <- msm(state ~ timeC, subject = ptnum, data = d,qmatrix =Q, gen.inits=TRUE,covariates = ~ all_pous,method = "BFGS", control = list(fnscale = 4000, maxit = 10000))
qmatrix.msm(asthma.msm3, covariates=list(IMC=1,all_pous=0))
lrtest.msm(asthma.msm, asthma.msm4)
plot.prevalence.msm(asthma.msm2, mintime=0, maxtime=2)
pearson.msm(asthma.msm3, timegroups=2,
 transitions=c(1,2,3,4,5,6,7,8,9))
%%%%matrix transition:
pmatrix.msm(asthma.msm, t =0.24657, ci = "normal",covariates=list(sexe=1))
covariates <- list( list (reveil=1, duirne=0), list (reveil=0, duirne=1), list(reveil=1, duirne=1))
time=c(0,1/4)
z=pmatrix.piecewise.msm(asthma.msm2,0,1,time,covariates)
%%%%%Survival plot:
library(msm)
plot.survfit.msm(asthma.msm2, main = "asthma.msm: covariates", mark.time = FALSE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ù
d=read.table(file.choose(),header=TRUE,sep="\t")
library(msm)
t=statetable.msm(state1, ptnum, data = d)
t
Q <- rbind(c(0,0.4615385),  c(0.0677, 0))
rownames(Q) <- colnames(Q) <- c("controle","inacceptable")

%%%%%%%ini matrix default:
Q.crude <- crudeinits.msm(state1 ~ tstart, ptnum, data=d, qmatrix=Q)
%%%%%%%%%%%%%%%%%%
asthma.msm <- msm(state1 ~ tstart, subject = ptnum, data = d, qmatrix =Q,exacttimes=TRUE)
asthma.msm1 <- msm(state1 ~ tstart, subject = ptnum, data = d,qmatrix =Q, gen.inits=TRUE)

%%%ù
pmatrix.msm(asthma.msm1, t = 90, ci = "normal")
