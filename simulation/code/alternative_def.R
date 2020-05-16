library("extraDistr") #rtlambda 
library("truncnorm") #rtruncnorm

create_alternative_sample<-function(dist, size){
  #Close to normal dist
  if(dist == "tukey(0.1)"){
    return(rtlambda(n = size, lambda = 0.1))
  }
  if(dist == "tukey(0.2)"){
    return(rtlambda(n = size, lambda = 0.2))
  }
  if(dist == "tukey(5)"){
    return(rtlambda(n = size, lambda = 5))
  }
  if(dist == "t(10)"){
    return(rt(n = size, df = 10))
  }
  if(dist == "laplace(0,10)"){
    return(rlaplace(n = size, mu = 0, sigma = 10))
  }
  
  
  #Sym Long Tail
  if(dist == "t(1)"){ #Cachy
    return(rt(n = size, df = 1))
  }
  if(dist == "t(2)"){ 
    return(rt(n = size, df = 2))
  }
  if(dist == "t(4)"){ 
    return(rt(n = size, df = 4))
  }
  if(dist == "t(7)"){ 
    return(rt(n = size, df = 7))
  }
  if(dist == "tukey(10)"){ 
    return(rtlambda(n = size, lambda = 10))
  }
  
  #Sym Short Tail
  if(dist == "uniform(0,1)"){ 
    return(runif(n = size, min = 0, max = 10)) 
  }
  if(dist == "beta(1.3,1.3)"){ #alpa=1.3, beta=1.3
    return(rbeta(n = size, shape1 = 1.3, shape2 = 1.3))
  }
  if(dist == "beta(1.5,1.5)"){ #alpa=1.5, beta=1.5
    return(rbeta(n = size, shape1 = 1.5, shape2 = 1.5))
  }
  if(dist == "tukey(1.5)"){
    return(rtlambda(n = size, lambda = 1.5))
  }
  if(dist == "truncatednormal(2,2)"){
    return(rtruncnorm(n = size, mean = -2, sd = 2))
  }
  
  
  #Asym Long Tail
  if(dist == "Weibull(0.5,1)"){ # shape = k
    return(rweibull(n = size, shape = 0.5, scale = 1))
  }
  if(dist == "Weibull(2,1)"){ # shape = k
    return(rweibull(n = size, shape = 2, scale = 1))
  }
  if(dist == "lognormal(0,1)"){
    return(rlnorm(n = size, meanlog = 0, sdlog = 1))
  }
  if(dist == "chisquared(4)"){
    return(rchisq(n = size, df = 4))
  }
  if(dist == "chisquared(10)"){
    return(rchisq(n = size, df = 10))
  }
  
  #Asym Short Tail
  if(dist == "beta(2,1)"){ #alpa=2, beta=1
    return(rbeta(n = size, shape1 = 2, shape2 = 1))
  }
  if(dist == "beta(3,2)"){
    return(rbeta(n = size, shape1 = 3, shape2 = 2))
  }
  if(dist == "lognormal(0,0.15)"){
    return(rlnorm(n = size, meanlog = 0, sdlog = 0.15))
  }  
  if(dist == "lognormal(0,0.25)"){
    return(rlnorm(n = size, meanlog = 0, sdlog = 0.25))
  } 
  if(dist == "lognormal(0,0.35)"){
    return(rlnorm(n = size, meanlog = 0, sdlog = 0.35))
  }
  
  stop(paste("Not handled dist:", dist, sep = " "))
}


dist_ctn <- list("tukey(0.1)", "tukey(0.2)", "tukey(5)", "t(10)", "laplace(0,10)")
dist_slt <- list("t(1)", "t(2)", "t(4)", "t(7)", "tukey(10)")
dist_sst <- list("uniform(0,1)", "beta(1.3,1.3)", "beta(1.5,1.5)", "tukey(1.5)", "truncatednormal(2,2)")
dist_alt <- list("Weibull(0.5,1)", "Weibull(2,1)", "lognormal(0,1)", "chisquared(4)", "chisquared(10)")
dist_ast <- list("beta(2,1)", "beta(3,2)", "lognormal(0,0.15)", "lognormal(0,0.25)", "lognormal(0,0.35)")
dist_alternatives = c(dist_ctn, dist_slt, dist_sst, dist_alt, dist_ast)

