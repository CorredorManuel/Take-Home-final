library(readxl)
datos <- read_excel("datos.xlsx")
data <- log(datos$NEQUI)


myLag <- function(x, n){
  if(n >= length(x))
    return(rep(NA,n))
  else if(n < length(x) & n > 0) 
    c(rep(NA,n), x[1:(length(x)-n)]) 
}
lagit <- function(x,y){#ESTA FUNCION GUARDA LOS LAGS EN COLUMNAS
  cbind(x, sapply(y, function(z) myLag(x,z)))
}

# definir la daa a usar
matriz <- as.matrix(segundo)
valores <- length(matriz)
# paso a paso dickey fuller aumentado tao tao
Y <- matrix()
Y_D <- matrix()
X <- matrix()
dY_D <- matrix()

coeficientes <- 3
filas <- 3   # 7 aumentada 
verificarse <- matrix()
modelo <- matrix(data = NA,nrow =filas ,ncol =1)
resultado <- cbind()
for(i in 1:filas){
  Y <- matriz[(1:valores-1)]
  for(k in 2:valores){
    Y_D[k-1] <-  matriz[k,1]-matriz[k-1,1]
  }
  p <- filas-coeficientes#-1   #numero de filas menos 3 para tao tao 
  #dY_D  <- lagit(Y_D,1:p)
  dY_D <- Y_D # en este caso cuando p =0
  #X <- cbind(1,Y,seq(from=1,to=valores-1),dY_D[,(2:(p+1))])
  X <- cbind(1,seq(from=1,to=valores-1),Y)
  #X <- X[-c(1:p),]
  #Y_D <- Y_D[-c(1:p)]
  modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D
  indice <- dim(modelo)[1]
  #ultimo beta hacer prueba T
  df <- dim(X)[1]-dim(X)[2]
  ygorro <- (X%*%modelo[1:indice])
  egorro <- c(Y_D-ygorro)
  #VERIFICAR RUIDO BLANCO
  N <- length(egorro)
  vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
  varcov <- vare*solve(t(X)%*%X)
  verificarse[i] <- modelo[i,1]/sqrt(varcov[i,i])
  verificarse
  if(verificarse[3]>-3.45){ #alta potencia
    resultado <-  "gamma es 0"
  }else{
    resultado <- "gamma no es 0"
  }
} 
cbind(resultado)




########

-2.87>-3.45
-3.45>-2.87

########### rudio blanco


d <- 1
for(i in 1:(N/4)){
  Q2 <- (N-d-1)*(N-d+1)*(sum(egorro**2))/(N-d-1-i)
}

Q2



######
k <- (N/4)
ValorCritico <- qchisq(p = 0.95,df = k-1,FALSE)
if( ValorCritico >Q2 ) {
  "no se rechaza la hipotesis nula"
} else {
  "se rechaza la hipotesis nula"
}














base <-ggplot() + xlim(-8, 8)+
  geom_function(aes(colour = "t, df = N-K"), fun = dt, args = list(df = df))+
  stat_function(fun = dnorm, geom = "polygon", color = "black", fill = "green", alpha = 0.5)+
  geom_vline(aes(xintercept=izquierda),linetype="dashed",color="red")+
  geom_vline(aes(xintercept=derecha),linetype="dashed",color="red")+
  geom_vline(aes(xintercept=verificarse[length(verificarse)]),linetype="dashed",color="blue")+ #aca modifique la posicion
  geom_density(fill="black", alpha = .5)+
  theme(legend.position = c(0.2, 0.9),
        legend.background = element_rect(fill = "white"))

base + labs(title = 'Curva de distribución',subtitle = 'verificacion de hipotesis',color='valor t estadistico')























library(quantmod)
library(tseries)

library(RCurl)
require(tseries)
library(urca)

segundo <-  diff(data)
adf.test(data,k=2)
adf.test(segundo,k=0)


dwd=ur.df(data,type="trend",lags=0)
summary(dwd)
















### funcion de diferencia

d_rezagos <- function(data,d){
  d1 <- matrix(data=NA,nrow = T,ncol = 1)
  for(i in 2:T){
    d1[i-1] <- data[i]-data[i-1]
    d1 <- d1[1:(T-d)]
  }
  d1
}

