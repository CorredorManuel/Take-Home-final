library(ggplot2)
library(ggthemes)
library(gridExtra)

histograma <- function(data,proceso){
  palabra <- switch (proceso,
                     tao = "",
                     taomiu = "miu",
                     taotao = "tau",
  )
  data <- as.data.frame(data )
  round(data,4)
  ggplot(data , aes(x = data )) +
    geom_histogram(aes(y = ..density..), bins= 30, colour= "black",
                   fill = "white") +
    geom_vline(aes(xintercept=mean(data )),linetype="dashed",color="navy")+
    geom_density(fill="blue", alpha = .5) +
    labs(title=paste("                                Histograma Tau",palabra),
         
         x="Observaciones",y="Frecuecia" )+
    
    
    theme_economist()+theme(axis.text = element_text(angle=0))
  
  
}




TaoTest <- function(matriz,repeticiones,modelod){
  Y <- matrix()
  Y_D <- matrix()
  X <- matrix()
  filas <- if(modelod=="tao"){filas =1}else if(modelod=="taomiu"){filas =2}else{filas =3}
  verificarse <- matrix()
  modelo <- matrix(data = NA,nrow =filas ,ncol =repeticiones)
  for(i in 1:repeticiones){
    Y <- matriz[(1:valores-1),i]
    for(k in 2:valores){
      Y_D[k-1] <-  matriz[k,i]-matriz[k-1,i]
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