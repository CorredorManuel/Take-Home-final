library(ggplot2)
library(ggthemes)
library(gridExtra)

histograma <- function(data,proceso){
  palabra <- switch (proceso,
                     tao = "",
                     taomiu = "miu",
                     taotao = "tao",
  )
  data <- as.data.frame(data )
  round(data,4)
  ggplot(data , aes(x = data )) +
    geom_histogram(aes(y = ..density..), bins= 30, colour= "black",
                   fill = "white") +
    geom_vline(aes(xintercept=mean(data )),linetype="dashed",color="navy")+
    geom_density(fill="blue", alpha = .5) +
    labs(title=paste("               Histograma Tao",palabra),
         
         x="Observaciones",y="Frecuecia" )+
    
    
    theme_economist()+theme(axis.text = element_text(angle=0))
  
  
}





##############################

TaoTest <- function(valores,repeticiones,modelod){
  
  matriz1 <- matrix(data = NA,nrow = valores,ncol = repeticiones)
  for( j in 1:repeticiones){
    set.seed(j)
    e <- rnorm(n=valores)
    N <- length(e)
    phi1 <- 1
    X1 <- matrix(data = NA,nrow = N,ncol = 1)
    for(i in 2:N){
      X1[1] <- 0
      X1[i] <- phi1*(X1[i-1]) +e[i]
    }
    matriz1[,j] <- X1
  }
  Y <- matrix()
  Y_D <- matrix()
  X <- matrix()
  filas <- if(modelod=="tao"){filas =1}else if(modelod=="taomiu"){filas =2}else{filas =3}
  verificarse <- matrix()
  modelo <- matrix(data = NA,nrow =filas ,ncol =repeticiones)
  for(i in 1:repeticiones){
    Y <- matriz1[(1:valores-1),i]
    for(k in 2:valores){
      Y_D[k-1] <-  matriz1[k,i]-matriz1[k-1,i]
    }
    X <- switch (modelod,
                 tao = cbind(Y),
                 taomiu = cbind(1,Y),
                 taotao = cbind(1,seq(from=1,to=valores-1),Y),
    )
    X <- as.matrix(X)
    modelo[,i] <- solve(t(X)%*%X)%*%t(X)%*%Y_D
    indice <- dim(modelo)[1]
    ygorro <- (X%*%modelo[1:indice,i])
    egorro <- c(Y_D-ygorro)
    #K <- 1
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    verificarse[i] <- modelo[indice,i]/sqrt(varcov[indice,indice])
  }
  return(verificarse)
}

# Creación de dos funciones que crean una matriz de lags
myLag <- function(x, n){
  if(n >= length(x))
    return(rep(NA,n))
  else if(n < length(x) & n > 0) 
    c(rep(NA,n), x[1:(length(x)-n)]) 
}
lagit <- function(x,y){#ESTA FUNCION GUARDA LOS LAGS EN COLUMNAS
  cbind(x, sapply(y, function(z) myLag(x,z)))
}

#Creación de funcion que calcula las actocorrelaciones parciales
ACFfun00 <- function(Z,K){
  media <- 0
  N <- length(Z)
  ACF <- matrix(data = NA,nrow = (K+1),ncol = 1)#creación vector vacio
  for(k in 0:K){
    Zk <- Z[(k+1):N] # valor eliminados al inicio
    Zklag <- Z[1:(N-k)] #valores eliminados al final
    numerador <- sum((Zk-media)*(Zklag-media))
    denominador <- sum((Z-media)^2)
    ACF[k+1] <- numerador/denominador
  }
  plot(ACF,type="h")
  #return(ACF)
}


PACFfun <- function(Z,K){
  N <- length(Z)
  ACF <- ACF[-c(1)] #con esto se soluciona el problema  de la grafica que inica en 1
  PACF <- matrix(data = NA,nrow = (K),ncol = 1)
  for (k in 1:K) {
    denominador <- matrix(data = NA,nrow = k,ncol = k)
    for(i in 1:k){
      for(j in 1:k){
        if(i==j){
          denominador[i,j] <- 1
        }else{
          denominador[i,j] <- ACF[abs(i-j)]  
        }
      }
    }
    numerador <- denominador
    numerador[,k] <- ACF[1:k]
    
    PACF[k] <- det(numerador)/det(denominador)
    
  }
  plot(PACF,type="h")
  #return(PACF)
  #PACF
}

#funcion que encuentra el orden P
p_encontrase <- function(pmax,data){
  matriz  <- data
  identificacion <- matrix(nrow = pmax,ncol = 2)
  for (p in 1:pmax){
    # Creación de variables requeridas
    Y <- matrix() 
    Y_D <- matrix()
    X <- matrix()
    dY_D <- matrix()
    
    valores <- length(matriz)
    #p <- 46 #orden P ha evaluar
    filas <- 3 #coefientes TauTau "intercepto,tendencia y gamma
    modelo <- matrix(data = NA,nrow =(filas+p) ,ncol =1) #matriz para guardar coeficientes
    Y <- matriz[(1:valores-1)]
    for(k in 2:valores){ #calculo de delta de Y
      Y_D[k-1] <-  matriz[k]-matriz[k-1]
    }
    dY_D  <- lagit(Y_D,1:(p)) #Aplicacion de la funcion de Lags
    dY_D <- dY_D[,2:(p+1)]
    #X <-  cbind(1,seq(from=1,to=valores-1),Y,dY_D)# Creacion matriz de diseño
    X <- cbind(1,seq(from=1,to=valores-1),Y,dY_D)# Creacion matriz de diseño
    X <- X[-c(1:p),]# Ajuste de la matriz eliminando valorres NA
    Y_D <- Y_D[-c(1:p)]
    modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D #Regresión del modelo
    indice <- dim(modelo)[1]
    df <- dim(X)[1]-dim(X)[2]
    ygorro <- (X%*%modelo[1:indice])
    egorro <- c(Y_D-ygorro)
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    
    verificarse <- c()
    significancia <- c()
    rfinal <- c()
    for(i in 1:(filas+p)){
      verificarse[i] <- modelo[i,1]/sqrt(varcov[i,i])
      verificarse 
      izquierda <- qt(0.025,N-indice)#cola izquierda
      derecha <- qt(0.025,N-indice,lower.tail = FALSE)
      if(verificarse[i]>izquierda & verificarse[i]< derecha) { #alta potencia
        significancia[i] <-  "No se puede rechazar que el coeficiente sea 0, no es significativo"
      }else{
        significancia[i] <- "Se puede rechazar que el coeficiente sea O,es significativo"
      }
    }
    resultado1 <- c()
    k <- trunc(N/4) # 100 poner un valor para correr mayor a 50
    Rho_residuales <- ACFfun00(egorro,k)
    Q23 <- matrix (NA,p,2)
    for (i in 1:p){
      b<-Box.test(matriz, lag =i, type="Ljung")
      Q23[i,] <- c(b$statistic , b$p.value )
    }
    colnames(Q23) <-  c("statistic", "p.value")
    k <- trunc(N/4)
    ValorCritico <- qchisq(p = 0.95,df = p,FALSE)
    if( ValorCritico >Q23[p] ) {
      resultado1 <- "no se rechaza la hipotesis nula,son rudio blanco"
    } else {
      resultado1 <- "se rechaza la hipotesis nula,no son ruido blanco"
    }
    identificacion[p,1] <- cbind(significancia[indice])
    identificacion[p,2] <- cbind(resultado1)
    
  }
  
  identificacion1 <- which(identificacion[,1]=="Se puede rechazar que el coeficiente sea O,es significativo")
  identificacion2 <- which(identificacion[,2]=="no se rechaza la hipotesis nula,son rudio blanco")
  revisar <- match(identificacion1 ,identificacion2) #se filtran por la ubicación de los valores que se repiten y se escoge el menor
  
  
  if( is.na(revisar[1])){
    print("NO Existe ningun P")
  }else{
    print("El orden P de la parte aumentada que genera rudio Blanco, que tiene significancia y que es parsimonioso es el:")
    print(revisar[1])
  }
  

}


#esta grafica verifica visualmente si el p obtenido es significante 
base <-ggplot() + xlim(-8, 8)+
  geom_function(aes(colour = "t, df = N-K"), fun = dt, args = list(df = df))+
  stat_function(fun = dnorm, geom = "polygon", color = "black", fill = "green", alpha = 0.5)+
  geom_vline(aes(xintercept=izquierda),linetype="dashed",color="red")+
  geom_vline(aes(xintercept=derecha),linetype="dashed",color="red")+
  geom_vline(aes(xintercept=verificarse[length(verificarse)]),linetype="dashed",color="blue")+ #aca modifique la posicion
  geom_density(fill="black", alpha = .5)+
  theme(legend.position = c(0.2, 0.9),
        legend.background = element_rect(fill = "white"))

final <-  base + labs(title = 'Curva de distribución t',subtitle = 'verificacion de significancia del coeficiente P de ADF',color='valor t estadistico')




d_rezagos <- function(data,d){
  d1 <- matrix(data=NA,nrow = T,ncol = 1)
  for(i in 2:T){
    d1[i-1] <- data[i]-data[i-1]
    d1 <- d1[1:(T-d)]
  }
  d1
}

