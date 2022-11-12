library(ggplot2)
library(ggthemes)
library(gridExtra)
library(stargazer)
library(lmtest)
######### Simulación de una relación Espúrea #########

# se definen la cantidad de repeticiones y el tamaño de cada vector
repeticiones <- 1000
valores <- 500
# se definen las variables que guardaran informacion dentro del For loop
coeficiente <- c() 
coeficiente2<-c()
durbin1 <- c()
durbin2 <- c()
r2 <- c()
r2ajustado <- c()
#for que realiza todas las repeticiones 
for( j in 1:repeticiones){
  set.seed(j) # se fija una semilla para los errores de la varaiable X
  ex <- rnorm(n=valores,mean = 0,sd = 5) 
  set.seed(j+1) # se fija una semilla para los errores de la varaiable Y
  ey <- rnorm(n=valores,mean = 0,sd = 5)
  N <- length(ex)
  alpha <- 1 #valores indicados
  beta <- 1 #valores indicados
  Xt <- matrix(data = NA,nrow = N,ncol = 1) #matriz que guarde la simulación X
  yt <- matrix(data = NA,nrow = N,ncol = 1) #matriz que guarde la simulación Y
  for(i in 2:N){
    Xt[1] <- 0 # valor Inicial 0 
    Xt[i] <- alpha*(Xt[i-1]) +ex[i]
    yt[1] <- 0  # valor Inicial 0 
    yt[i] <- beta*(yt[i-1]) +ey[i]
  }
  X <- cbind(1,Xt)
  # Estimación de la regresión requerida
  betas <- solve(t(X)%*%X)%*%t(X)%*%yt
  coeficiente[j]<- betas[1,1]
  coeficiente2[j]<- betas[2,1]
  ygorro <- X%*%betas
  egorro <- yt-ygorro

### Calculo del estadístico Durbin-Watson.
  rho_vec <- cbind() 
  for(k in 1:valores){
    rho_vec[k] <- egorro[k+1]*egorro[k]}
  Num <- sum(cbind(na.omit(rho_vec)))
  rho_vec2 <- cbind() 
  for(l in 1:valores-1){
    rho_vec2[l] <- egorro[l]^2}
  Den <- sum(cbind(na.omit(rho_vec2)))
  rho_est <- Num/Den
  durbin1[j] <- 2*(1-rho_est)
  
  #verificacion Durbin-Watson comando 
  modelo <- lm(yt~X)
  valorsw <- dwtest(modelo)
  durbin2[j] <- valorsw$statistic

  #Calculo del R^2
  sec <- (ygorro-mean(yt))**2
  stc <- (yt-mean(yt))**2
  r2[j] <- sum(sec)/sum(stc)
  
  # calculo del r2 ajustado
  k <- 1
  r2ajustado[j] <- 1-((N-1)/(N-(k+1)))*(1-r2[j])

  
}
# se juntan lo datos por columnas 
datos_finales <- cbind(coeficiente,coeficiente2,durbin1,r2,r2ajustado)
# Bono!! R2 cercanoa 0.8
r2[298]
r2[889]

###### funcion que grafica Histogramas con posibilidad de añadirle un tiutlo
histograma <- function(data,titulo){
  data <- as.data.frame(data )
  round(data,4)
  ggplot(data , aes(x = data )) +
    geom_histogram(aes(y = ..density..), bins= 30, colour= "black",
                   fill = "white") +
    geom_vline(aes(xintercept=mean(data )),linetype="dashed",color="navy")+
    geom_density(fill="blue", alpha = .5) +
    labs(title=paste("                      Histograma del ",titulo),
         x="Observaciones",y="Frecuecia" )+
    theme_economist()+theme(axis.text = element_text(angle=0))
}

histograma(coeficiente,"coeficiente 1")
histograma(coeficiente2,"coeficiente 2")

#el resultado del durbin con comando directo no es exactamente igual ya que 
#en el proceso manual estamos siguiendo la aproximación cuando "t" va al infinito

histograma(durbin1,"durbin proceso Manual")
histograma(durbin2,"durbin  con comando")



#cuantiles de las distribuciones anteriormente calculadas
quantile(coeficiente,probs = c(0.01,0.05,0.1))
quantile(coeficiente2,probs = c(0.01,0.05,0.1))
quantile(durbin1,probs = c(0.01,0.05,0.1))



####### Durbin verificación
## Tomando los valores de la tabla de Durbin Watson para una serie con 100 datos 
#un alha = 5% y K=1 tenemos los siguientes valores
alpha <- 0.05
dl <- 1.65
du <- 1.69
du1 <- 4-du
dl1 <- 4-dl
salida_bg<-c(dl ,du,mean(durbin1),du1,dl1)
names(salida_bg)<-c("lim.Inf.izq","lim.Sup.izq","durbin","lim.Inf.der",
                    "lim.Supr.der ")
stargazer(salida_bg,title = "Posición del Durbin",
          type ="text",digits = 2)

#Con un nivel de significancia del 5% y un K = 1 podemos confirmar que segun 
#los limites inferiores y superiores del Durbin Watson, existe evidencia para
#rechazar la hipótesis nula: No existe autocorrelación. Por lo que el modelo 
#presenta incumplimiento del supuesto básico de no autocorrelación.
# especificamente estariamos en la zona de Autocorrelacion Positiva



# con alpha y beta mayores a 1 la regresion no se puede llevar a cabo debido
# a que la matriz no es invertible


## con alpha y beta menores a uno el histograma es similar a =1 




















