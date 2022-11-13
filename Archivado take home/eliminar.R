library(readxl)
datos <- read_excel("datos.xlsx")


funci_p <- function(ar4,n,p){
  diff_ar4 <- c()
  for(i in 2:n){
    diff_ar4[i-1] <- ar4[i]-ar4[i-1]
  }
  po <- diag(0,(n-1),(p-1))
  x_ar4 <- cbind(1,(2:n),ar4[c(-n)])
  
  for(i in 1:(p-1)){
    for(j in 1:((n-1)-i)){
      po[(j+i),i] <- diff_ar4[j]
    }
  }
  
  x_diseño <- cbind(x_ar4,po)
  #hasta aca puedo replicar
  
  
  compro <- c()
  for(i in 1:(p)){
    x_diseño <- cbind(x_ar4,po)
    lelo <- x_diseño[(i:(n-1)),(1:(3+(i-1)))]
    ene <- nrow(lelo)
    dif_ar4_2 <- diff_ar4[i:(n-1)]
    betas_p <- solve(t(lelo)%*%lelo)%*%t(lelo)%*%cbind(dif_ar4_2)
    ene_betas <- length(betas_p)
    ygorro_p <- lelo%*%betas_p
    egorro_p <- dif_ar4_2-ygorro_p
    vare_p <- sum(t(egorro_p)%*%egorro_p)*(1/(ene-ene_betas))
    varcov_p <- vare_p*solve(t(lelo)%*%lelo)
    prueba_t <- betas_p[(i+2),1]/sqrt(varcov_p[(i+2),(i+2)])
    prueba_t_cuantil <- c(qt(0.025,ene-ene_betas), qt(0.025,ene-ene_betas, lower.tail = FALSE))
    
    acf_res <- cbind(1)
    for(j in 1:floor(ene/4)){
      int1 <- cbind(egorro_p[1:(ene-j)])
      int2 <- cbind(egorro_p[(j+1):ene])
      m <- 0
      var <- sum((egorro_p-m)**2)
      cov <- t(int1-m)%*%(int2-m)
      acf_res[j+1] <- cov/var
      
    }
    Q_ljun <- sum((acf_res**2)/(ene-1-1-floor(ene/4)))*(ene-1-1)*(ene-1+1)
    valor_cri<- qchisq(0.95,((floor(ene/4))-1))
    
    
    if (prueba_t<prueba_t_cuantil[2]&prueba_t>prueba_t_cuantil[1]&valor_cri<Q_ljun){
      compro[i]<- "No es significativo y/o no produce ruido blanco"
    }else{
      compro[i]<- "Es significativo y produce ruido blanco"
    }
    
  }
  
  kk<-which (compro =="Es significativo y produce ruido blanco")
  kk<-kk[-1]
  p_optimo<-kk[which.min(kk)]
  print (paste("p óptimo es", p_optimo))
  
  
}

xt <- log(datos$NEQUI)
xt <- (datos$NEQUI)
ar4 <- xt

funci_p(xt1,249,100)



