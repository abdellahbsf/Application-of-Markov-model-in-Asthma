d=read.table(file.choose(),header=TRUE,sep="\t")

library(msm)

Q <- rbind(c(0, 0.03333,0.0715385), c(.280 , 0, 0.320), c(.23050,0.3677, 0))

rownames(Q) <- colnames(Q) <- c("controle", "semi-controle","inacceptable")

ini matrix default:
Q.crude <- crudeinits.msm(state ~ timeC, ptnum, data=d, qmatrix=Q)

asthma.msm1 <- msm(state ~ timeC, subject = ptnum, data = d, qmatrix =Q,exacttime=TRUE,
                 method = "BFGS", control = list(fnscale = 4000, maxit = 10000))

Call:
msm(formula = state ~ timeC, subject = ptnum, data = d, qmatrix = Q,     exacttimes = TRUE, method = "BFGS", control = list(fnscale = 4000,         maxit = 10000))

Maximum likelihood estimates

Transition intensities
                              Baseline                  
controle - controle           -0.3353 (-0.52588,-0.2138)
controle - semi-controle       0.2644 ( 0.15928, 0.4389)
controle - inacceptable        0.0709 ( 0.02665, 0.1887)
semi-controle - controle       1.4472 ( 1.02901, 2.0355)
semi-controle - semi-controle -1.8406 (-2.49064,-1.3603)
semi-controle - inacceptable   0.3934 ( 0.20452, 0.7567)
inacceptable - controle        0.9745 ( 0.66348, 1.4314)
inacceptable - semi-controle   0.9750 ( 0.66385, 1.4319)
inacceptable - inacceptable   -1.9495 (-2.55840,-1.4855)

-2 * log-likelihood:  282.0642 
[Note, to obtain old print format, use "printold.msm"]

asthma.msm2 <- msm(state ~ timeC, subject = ptnum, data = d,qmatrix =Q, gen.inits=TRUE,covariates = ~ IMC,
                  method = "BFGS", control = list(fnscale = 4000, maxit = 10000))

Call:
msm(formula = state ~ timeC, subject = ptnum, data = d, qmatrix = Q,     gen.inits = TRUE, covariates = ~IMC, method = "BFGS", control = list(fnscale = 4000,         maxit = 10000))

Maximum likelihood estimates
Baselines are with covariates set to their means

Transition intensities with hazard ratios for each covariate
                              Baseline                       
controle - controle            -0.5105 ( -5.337613, -0.04882)
controle - semi-controle        0.3258 (  0.009118, 11.64235)
controle - inacceptable         0.1847 (  0.001620, 21.04405)
semi-controle - controle        1.0089 (  0.055478, 18.34909)
semi-controle - semi-controle  -3.2847 (-12.471682, -0.86509)
semi-controle - inacceptable    2.2757 (  0.352238, 14.70306)
inacceptable - controle         1.3788 (  0.012718,149.48369)
inacceptable - semi-controle   10.4645 (  3.462212, 31.62856)
inacceptable - inacceptable   -11.8433 (-37.504618, -3.73989)
                              IMC                           
controle - controle                                         
controle - semi-controle       1.87377 (3.948e-04,8.894e+03)
controle - inacceptable        0.09252 (2.269e-05,3.773e+02)
semi-controle - controle       0.14269 (9.641e-04,2.112e+01)
semi-controle - semi-controle                               
semi-controle - inacceptable   3.12322 (4.725e-02,2.065e+02)
inacceptable - controle       20.18695 (2.821e-04,1.445e+06)
inacceptable - semi-controle   0.39607 (4.242e-02,3.698e+00)
inacceptable - inacceptable                                 

-2 * log-likelihood:  229.4619 
[Note, to obtain old print format, use "printold.msm"]

