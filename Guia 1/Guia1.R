##Ejercicio 2


##Función para graficar distintas distribuciones binomiales

graficarBinomial<-function(n,p){
  bin<-c()
  for(i in 0:n){
    var<-dbinom(i,n,p,FALSE)
    bin<-c(bin,var)
  }
  barplot(height = bin,names = c(0:n),ylim=c(0,1),main = paste("n =",n,", p =",p))
}

enes<-c(5,6,10,50)
pes<-c(0.5,0.9,0.1)

##Funcion que hacer los barplots usando los valores de arriba

ejercicio2<-function(){
  par(mfrow = c(4,3),mar=c(2,2,2,2))
  for (n in enes) {
    for (i in pes) {
      graficarBinomial(n,i)
    }
  }
}

##Ejercicio 3


##Función para graficar distintas distribuciones Poisson

lambdas<-c(0.1,0.5,1,5,10)

graficarPoisson<-function(l){
  bin<-c()
  for(i in 0:30){
    proba<-dpois(i,l,FALSE)
    bin<-c(bin,proba)
  }
  barplot(height = bin,names=c(0:30),ylim=c(0,1),main = paste("Poisson de parametro lambda ",l))
}


##Funcion que hacer los barplots usando los valores de arriba

ejercicio3<-function(){
  par(mfrow=c(3,2))
    for (i in lambdas) {
      graficarPoisson(i)
    }
}

##Segun la sugerencia del apunte de Bianco y Martinez deberia haber una relacion
##de tipo np<=20 de manera que tomando n = 30 y p = 0.167 bastaria y np = 5
superponerBarPlot<-function(){
  
}

ejercicio4<-function(){
  par(mfrow=c(2,2))
  graficarPoisson(5)
  graficarBinomial(30,0.167)
  
}

graficarGamma<-function(lambda,alfa){
  curve(dgamma(x,shape = alfa,scale = lambda), xlim = c(0,20))
}

ejercicio5A<-function(){
  par(mfrow=c(3,3))
  for (lambda in (1:9)) {
    graficarGamma(lambda,1)
  }
}
ejercicio5B<-function(){
  par(mfrow=c(3,3))
  for (alfa in (1:9))ejer {
    graficarGamma(1,alfa)
  }
}

cauchy<-function(x){
  return(1/(pi*(1+x^2)))
}
  

ejercicio7A<-function(){
  par(mfrow=c(1,1))
  curve(cauchy(x), from = -20 ,to =20) 
}
terminoEsperanzaCauchy<-function(x){
  return(x*cauchy(x))
}

ejercicio7B<-function(){
  return(integrate(terminoEsperanzaCauchy,lower = -Inf, upper = Inf))
}
##Da 0 pero esta mal!!!.
