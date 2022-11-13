# Punto 3 identifique el orden de integracion de la serie 

# Bajo el esquema de Dickey fuller aumentado se requiere indentificar el orden P
# de la serie que genere Ruido Blanco y donde su coeficiente sea significante
# respetando que el modelo sea parsimonioso

library(readxl)
datos <- read_excel("libro1.xlsx")
matriz <- log(datos$PIB)

datos <- read_excel("datos.xlsx")
matriz <- diff(datos$NEQUI)

#matriz <-xt1  #serie diferenciada
#############matriz <-  SerieW_t####################### miguel luna
pmax <- 100 #especifico un P maximo para la verificación
identificacion <- matrix(nrow = pmax,ncol = 2)
#IDENTIFIACIÓN ORDEN P
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
revisar <- match(identificacion1 ,identificacion2)
print("el orden P de la parte aumentada que genera rudio Blanco, que tiene significancia y que es parsimonioso es")
p_final <- print(revisar[1])
}


revisar <- p_encontrase(100,matriz)
#View(identificacion)  ##se revisa si el orden p genera Ruido Blanco y si es significante
### verificación visual de la significancia del orden P "en este caso se encuentra siginifica en el orden p= 46" 

######
  base <-ggplot() + xlim(-8, 8)+
    geom_function(aes(colour = "t, df = N-K"), fun = dt, args = list(df = df))+
    stat_function(fun = dnorm, geom = "polygon", color = "black", fill = "green", alpha = 0.5)+
    geom_vline(aes(xintercept=izquierda),linetype="dashed",color="red")+
    geom_vline(aes(xintercept=derecha),linetype="dashed",color="red")+
    geom_vline(aes(xintercept=verificarse[length(verificarse)]),linetype="dashed",color="blue")+ #aca modifique la posicion
    geom_density(fill="black", alpha = .5)+
    theme(legend.position = c(0.2, 0.9),
          legend.background = element_rect(fill = "white"))
  
  base + labs(title = 'Curva de distribución t',subtitle = 'verificacion de significancia del coeficiente P de ADF',color='valor t estadistico')
  
  
## se concluye que para esta base de datos no existe ningun p que genere ruido Blanco aunque se tenga significancia 
## en un orden p muy grande lo que podria llevarnos a conclusiones espureas.

#si revisamos el grafico de las autocorrelaciones parciales podriamos inferir que se trata de un proceso AR(1)
  
  # añadir grafico 
  pacf(matriz) 
  
#De acuerdo con el resultado, ningún rezago igual a 2 o mayor es significativo. Se deduce que el rezago que 
#genera ruido blanco y es significativo es p=1. No requiere de parte aumentada.

#####
  
# ETAPA 1
    # Creación de variables requeridas
    Y <- matrix() 
    X <- matrix()
    Y_D<- matrix()
    valores <- length(matriz)
    filas <- 3 #coefientes TauTau "intercepto,tendencia y gamma)
    modelo <- matrix(data = NA,nrow =(filas) ,ncol =1) #matriz para guardar coeficientes
    Y <- matriz[(1:valores-1)]
    for(k in 2:valores){ #calculo de delta de Y
      Y_D[k-1] <-  matriz[k]-matriz[k-1]
    }
    X <- cbind(1,seq(from=1,to=valores-1),Y)# Creacion matriz de diseño
    modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D #Regresión del modelo
    indice <- dim(modelo)[1]
    df <- dim(X)[1]-dim(X)[2]
    ygorro <- (X%*%modelo[1:indice])
    egorro <- c(Y_D-ygorro)
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    
    verificarse <- c()
    for( i in 1:indice){
    verificarse[i]<- modelo[i,1]/sqrt(varcov[i,i])
    }
    
#Ahora se prueba la significancia del gamma con ayuda de los valores críticos simulados del TauTau.
#Simulaciones de TauTau, se realizan por medio de una función que importo del script Funciones.R
    
    source("Funciones.R")
    #se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
    verificarse3 <- TaoTest(100,50000,"taotao") #calculo con intercepto y tendencia
    Valor_tautau <- quantile(verificarse3,probs = c(0.01,0.05,0.1))
  

    if( verificarse[3]<Valor_tautau[2]) {#se toma la siginficancia al 5% del estadisitico
       "Gamma es estadisticamente diferente de cero, es significativo"
    } else {
      "Gamma es estadisticamente  cero, no es significativo"
    }
    
#ETAPA 2
    
# como gamma es estadísticamente igual a cero debemos verificar el coeficiente relacionado con la tendencia
# donde se realiza la prueba Phi3
# teniendo en cuenta el valor phi3 generado por Dickey y Fuller (1981) se realiza una verificación para
# concluir si estadisticamente el coeficiente es 0 
    
    library(urca)
    #summary(ur.df(y=matriz, type = "trend",lags=1)) ###con este summary se puede obtener el valor phi3
    valor_phi3 <- 6.49


    if( verificarse[3]&verificarse[2]>valor_phi3) {# esta es una prueba tipo F por lo tanto la cola derecha es la zona de rechazo
      "Gamma y el Coeficiente relacionado con la tendencia no es cero, es significativo"
    } else {
      "Gamma y el Coeficiente relacionado con la tendencia es cero, no es significativo"
    }

#ETAPA 3   
    
   # matriz <- datos$NEQUI #
    # Creación de variables requeridas
    Y <- matrix() 
    X <- matrix()
    Y_D<- matrix()
    valores <- length(matriz)
    filas <- 2 #coefientes TauTau "intercepto,tendencia y gamma)
    modelo <- matrix(data = NA,nrow =(filas) ,ncol =1) #matriz para guardar coeficientes
    Y <- matriz[(1:valores-1)]
    for(k in 2:valores){ #calculo de delta de Y
      Y_D[k-1] <-  matriz[k]-matriz[k-1]
    }
    X <- cbind(1,Y)# Creacion matriz de diseño
    modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D #Regresión del modelo
    indice <- dim(modelo)[1]
    df <- dim(X)[1]-dim(X)[2]
    ygorro <- (X%*%modelo[1:indice])
    egorro <- c(Y_D-ygorro)
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    verificarse <- c()
    for( i in 1:indice){
      verificarse[i]<- modelo[i,1]/sqrt(varcov[i,i])
    }
    

    
#En este caso debemos realizar una prueba TauMiu para el coeficiente Gamma estimado
#con la que se determinara si este es estadisticamente igual a 0 o no
    
    source("Funciones.R")
    #se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
    verificarse2 <- TaoTest(matriz1,50000,"taomiu") #calculo con solo intercepto 
    Valor_taumiu <- quantile(verificarse2,probs = c(0.01,0.05,0.1))
    
    if( verificarse[2]<Valor_taumiu[2]) {#se toma la siginficancia al 5% del estadisitico
      "Gamma es estadisticamente diferente de cero, es significativo"
    } else {
      "Gamma es estadisticamente  cero, no es significativo"
    }

#Como Gamma es igual a cero debemos verificar si el intercepto es significativo con una prueba phi1
    
    
    #matriz <- datos$NEQUI #
    library(urca)
    #summary(ur.df(y=matriz, type = "drift",lags=1)) #veirifacion de valor phi1 con comando
    valor_phi1 <- 4.71
    
    if( verificarse[2]&verificarse[1]>valor_phi1) {
      "Gamma y el Intercepto no son cero estadisitcamente, es significativo"
    } else {
      "Gamma y el Intercepto son cero estadisitcamente, no es significativo"
    }
    
    
    
    
    
# verificacion de significancia del intercepto    
    
    #matriz <- datos$NEQUI #
    # Creación de variables requeridas
    Y <- matrix() 
    X <- matrix()
    Y_D<- matrix()
    valores <- length(matriz)
    filas <- 1 #coefiente
    modelo <- matrix(data = NA,nrow =(filas) ,ncol =1) #matriz para guardar coeficientes
    Y <- matriz[(1:valores-1)]
    for(k in 2:valores){ #calculo de delta de Y
      Y_D[k-1] <-  matriz[k]-matriz[k-1]
    }
    X <- cbind(1,Y)# Creacion matriz de diseño
    X <- X[,1]
    modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D #Regresión del modelo
    indice <- dim(modelo)[1]
    df <- dim(X)[1]-dim(X)[2]
    ygorro <- (X%*%modelo)
    egorro <- c(Y_D-ygorro)
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    verificarse <- c()
    for( i in 1:indice){
      verificarse[i]<- modelo[i,1]/sqrt(varcov[i,i])
    }
    
    

    
    valor_t <- qt((1-0.05),(N/4))
    if( verificarse>valor_t) {
      "Intercepto no es cero estadisitcamente, es significativo"
    } else {
      "Intercepto es cero estadisitcamente, no es significativo"
    }
#como el intercepto no es significativo debo continuar a la siguiente etapa donde verifico gamma con un valor Tau
    
    
    #matriz <- datos$NEQUI #
    # Creación de variables requeridas
    Y <- matrix() 
    X <- matrix()
    Y_D<- matrix()
    valores <- length(matriz)
    filas <- 1 #coefiente
    modelo <- matrix(data = NA,nrow =(filas) ,ncol =1) #matriz para guardar coeficientes
    Y <- matriz[(1:valores-1)]
    for(k in 2:valores){ #calculo de delta de Y
      Y_D[k-1] <-  matriz[k]-matriz[k-1]
    }
    X <- cbind(Y)# Creacion matriz de diseño
    modelo[,1] <- solve(t(X)%*%X)%*%t(X)%*%Y_D #Regresión del modelo
    indice <- dim(modelo)[1]
    df <- dim(X)[1]-dim(X)[2]
    ygorro <- (X%*%modelo)
    egorro <- c(Y_D-ygorro)
    N <- length(egorro)
    vare <- (1/(N-1))*sum(t(egorro)%*%egorro)
    varcov <- vare*solve(t(X)%*%X)
    verificarse <- c()
    for( i in 1:indice){
      verificarse[i]<- modelo[i,1]/sqrt(varcov[i,i])
    }
    
    source("Funciones.R")
    #se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
    verificarse1 <- TaoTest(matriz1,50000,"tao") #calculo sin nada
    Valor_tau <- quantile(verificarse1,probs = c(0.01,0.05,0.1))
    
    if( verificarse<Valor_taumiu[2]) {#se toma la siginficancia al 5% del estadisitico
      "Gamma es estadisticamente diferente de cero, es significativo"
    } else {
      "Gamma es estadisticamente  cero, no es significativo"
    }    
    

    
    
  xt1 <- diff(matriz)

  