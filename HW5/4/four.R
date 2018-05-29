rm(list = ls())

### Loading Required Libraries ###

library(glasso)
library(gRbase)
library(gRim)
library(gRain)
library(graph)


x11()
pairs(state)
data("state")

state <- data.frame(state.x77)


par(mfrow=c(3,3))
hist(state[,1], main = "Population") #Population
hist(state[,2], main = "Income") #Income
hist(state[,3], main = "Illiteracy") #Illiteracy
hist(state[,4], main = "Life Expectancy") #Life Expectancy
hist(state[,5], main = "Murder") #Murder
hist(state[,6], main = "HS Grad") #HS Grad
hist(state[,7], main = "Frost") #Frost
hist(state[,8], main = "Area") #Area


par(mfrow=c(3,3))
boxplot(state[,1]) #Population
boxplot(state[,2]) #Income
boxplot(state[,3]) #Illiteracy
boxplot(state[,4]) #Life Expectancy
boxplot(state[,5]) #Murder
boxplot(state[,6]) #HS Grad
boxplot(state[,7]) #Frost
boxplot(state[,8]) #Area 

### Preprocess Data ###

par(mfrow=c(2,2))
state[,1] <- log(state[,1])
hist(state[,1],main = "Population")
state[,2] <- log(state[,2])
hist(state[,2])
state[,3] <- log(state[,3])
hist(state[,3], main = "Illiteracy")
state[,4] <- sqrt(state[,4])
hist(state[,4], main = "Life Expectancy")
state[,8] <- log(state[,8])
hist(state[,8], main = "Area")

life <- sqrt(state[,4])
hist(life)


### Partial Correlation ###

state.cv<- cov.wt(state, method = "ML")
state.pc <- cov2pcor(state.cv$cov)
x11()
heatmap(state.pc)


### Taking the covariance matrix ###

state.cov <- state.cv$cov

s.lasso <- glasso(state.cov, rho = 10)
my.edges <- s.lasso$wi != 0
diag(my.edges) <- FALSE
my.edges

### convert for plotting ###

g.lasso<- as(my.edges, "graphNEL")
g.lasso
nodes(g.lasso) <- names(state)
glass.net <- cmod(g.lasso, data = state)

x11()
plot(glass.net)
?state.x77

### Estimate over a range of rho's ###

rhos <- c(1,2,3,4,5 )
s.lasso <- glassopath(state.cov, rho = rhos)
graphics.off()
for (i  in (rhos)){
  print(i)
  my.edges <- s.lasso$wi[,,i] != 0
  diag(my.edges) <- FALSE
  g.lasso <- as(my.edges, "graphNEL") # convert for plotting
  nodes(g.lasso) <- names(state)
  glass.net <- cmod(g.lasso, data = state)
  x11()
  plot(glass.net)
}

#####  SOM  #####

data_train_matrix <- as.matrix(scale(state))
som_grid <- somgrid(xdim = 5, ydim=10, topo="hexagonal")
som_model <- supersom(data_train_matrix, grid=som_grid, rlen=100, alpha=c(0.05,0.01), keep.data = TRUE)
x11()
plot(som_model, type = "property", property = getCodes(som_model, 1)[,1], main=names(som_model$data)[7])
x11()
plot(som_model$changes)
