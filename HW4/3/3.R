rm(list = ls())


#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#biocLite("RBGL")

library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)

####### Specify the DAG ##########

g <- list(~A,~B,~C|A,~D|A:B,~E|B,~F|C:A:E,~G|D:E,~H|F:G)
dag <- dagList(g)
plot(dag)

####### Inquire about d-seperation ########

dSep(as(dag,"matrix"),"C","G",cond = NULL)
dSep(as(dag,"matrix"),"C","E",cond = NULL)
dSep(as(dag,"matrix"),"C","E",c("G"))
dSep(as(dag,"matrix"),"A","G",c("D","E"))
dSep(as(dag,"matrix"),"A","G",c("D"))

