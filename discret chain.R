d=read.table(file.choose(),header=TRUE,sep="\t") #importing the dataset 

library("markovchain")     #calling markovchain library that allow us to work with 
                            the built_in function "new".

library(msm)               #calling msm library that allow us to work with 
                            the built_in function "statetable.msm" to calculat
                            the number of transition from a state to another
                            for each patient in the cohort.
                           
t=statetable.msm(state, ptnum, data = d) #state is a categoricale variable takes three different states (1,2 and 3)
                                          ptnum is the identity of our patient.
t
    to
from   1   2   3
   1 124  15   4
   2  33  20   9
   3  26  26  13

rownames(t) <- colnames(t) <- c("controle", "semi-controle","inacceptable") #rename the columns and the rows of the matrix
t
               to
from            controle semi-controle inacceptable
  controle           124            15            4
  semi-controle       33            20            9
  inacceptable        26            26           13

mc <- new("markovchain", states = c("controle", "semi-controle","inacceptable"),
 transitionMatrix = matrix(data =  c(t[1,1]/sum(t[1,]),t[1,2]/sum(t[1,]),t[1,3]/sum(t[1,]),
t[2,1]/sum(t[2,]),t[2,2]/sum(t[2,]),t[2,3]/sum(t[2,]),t[3,1]/sum(t[3,]),t[3,2]/sum(t[3,]),t[3,3]/sum(t[3,])),byrow = TRUE, nrow = 3),
 name = "Asthma")

mc

Asthma 
 A  3 - dimensional discrete Markov Chain defined by the following states: 
 controle, semi-controle, inacceptable 
 The transition matrix  (by rows)  is defined as follows: 
               controle semi-controle inacceptable
controle      0.8671329     0.1048951   0.02797203
semi-controle 0.5322581     0.3225806   0.14516129
inacceptable  0.4000000     0.4000000   0.20000000

steadyStates(mc)

      controle semi-controle inacceptable
[1,] 0.7891542     0.1551083   0.05573746