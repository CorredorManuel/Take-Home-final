
# Punto 3 identifique el orden de integracion de la serie 

# Bajo el esquema de Dickey fuller aumentado se requiere indentificar el orden P
# de la serie que genere Ruido Blanco y donde su coeficiente sea significante
# respetando que el modelo sea parsimonioso
source("Funciones.R")
library(urca)
library(readxl)
datos <- read_excel("datos.xlsx")
matriz <- datos$NEQUI
N <- length(matriz)
#matriz1 <- diff(datos$NEQUI)
#matriz2 <- d_rezagos(datos$NEQUI,1)

revisar <- p_encontrase(100,matriz) 
#por medio de la funcion creada para encontrar el orden P del modelo ADF se confirma que
#no existe un p optimo mayor a 2, por lo que se asume que el p optimo =1

#la función "Etapas" realiza el calculo del estadistico tipo "t" de preferencia,
#para posteriormente verificarlo con los valores criticos de las distribuciones Tau
#cada etapa se esta ligada con el proceso de ADF
etapa1 <- Etapas(matriz,"etapa1")


#Ahora se prueba la significancia del gamma con ayuda de los valores críticos simulados del TauTau.
#Simulaciones de TauTau, se realizan por medio de una función que importo del script Funciones.R

#source("Funciones.R")
#se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
verificarse3 <- TaoTest(100,50000,"taotao") #calculo con intercepto y tendencia
Valor_tautau <- quantile(verificarse3,probs = c(0.01,0.05,0.1))


if( etapa1[3]<Valor_tautau[2]) {#se toma la siginficancia al 5% del estadisitico
  "Gamma es estadisticamente diferente de cero, es significativo"
} else {
  "Gamma es estadisticamente  cero, no es significativo"
}

#ETAPA 2

# como gamma es estadísticamente igual a cero debemos verificar el coeficiente relacionado con la tendencia
# donde se realiza la prueba Phi3
# teniendo en cuenta el valor phi3 generado por Dickey y Fuller (1981) se realiza una verificación para
# concluir si estadisticamente el coeficiente es 0 


#summary(ur.df(y=matriz, type = "trend",lags=1)) ###con este summary se puede obtener el valor phi3
valor_phi3 <- 6.49


if( etapa1[3]&etapa1[2]>valor_phi3) {# esta es una prueba tipo F por lo tanto la cola derecha es la zona de rechazo
  "Gamma y el Coeficiente relacionado con la tendencia no es cero, es significativo"
} else {
  "Gamma y el Coeficiente relacionado con la tendencia es cero, no es significativo"
}


#ETAPA 3   
#En este caso debemos realizar una prueba TauMiu para el coeficiente Gamma estimado
#con la que se determinara si este es estadisticamente igual a 0 o no

etapa3 <- Etapas(matriz,"etapa3")

#se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
verificarse2 <- TaoTest(100,50000,"taomiu") #calculo con solo intercepto 
Valor_taumiu <- quantile(verificarse2,probs = c(0.01,0.05,0.1))

if( etapa3[2]<Valor_taumiu[2]) {#se toma la siginficancia al 5% del estadisitico
  "Gamma es estadisticamente diferente de cero, es significativo"
} else {
  "Gamma es estadisticamente  cero, no es significativo"
}

#Como Gamma es igual a cero debemos verificar si el intercepto es significativo con una prueba phi1


#summary(ur.df(y=matriz, type = "drift",lags=1)) #veirifacion de valor phi1 con comando
valor_phi1 <- 4.71

if( etapa3[2]&etapa3[1]>valor_phi1) {
  "Gamma y el Intercepto no son cero estadisitcamente, es significativo"
} else {
  "Gamma y el Intercepto son cero estadisitcamente, no es significativo"
}


# verificacion de significancia del intercepto  


intercepto <- intercepto_s(matriz)

  valor_t <- qt((1-0.05),(N/4))
  if( intercepto>valor_t) {
    "Intercepto no es cero estadisitcamente, es significativo"
  } else {
    "Intercepto es cero estadisitcamente, no es significativo"
  }
  
  
#como el intercepto no es significativo debo continuar a la siguiente etapa donde verifico gamma con un valor Tau
  
  
  
#ETAPA 4 
#En este caso debemos realizar una prueba Tau para el coeficiente Gamma estimado
#con la que se determinara si este es estadisticamente igual a 0 o no

etapa4 <- Etapas(matriz,"etapa4")


  #se realizan 50.000 repeticiones para obtener estadisticos altamente efectivos 
  verificarse1 <- TaoTest(100,50000,"tao") #calculo sin nada
  Valor_tau <- quantile(verificarse1,probs = c(0.01,0.05,0.1))
  
  if( etapa4<Valor_tau[2]) {#se toma la siginficancia al 5% del estadisitico
    "Gamma es estadisticamente diferente de cero, es significativo"
  } else {
    "Gamma es estadisticamente  cero, no es significativo"
  }    
  


  
#Usando el estadistico Tau se pudo verificar que la serie NEQUI tiene raiz unitaria 
#debido a que no se encontro significancia en el estadistico tipo t proveniente del Gamma

# Ahora se debe realizar una diferenciacion a la serie y verificar de nuevo el proceso ADF
  


# se realiza una diferenciación
matriz <- diferencia(matriz,1)
revisar <- p_encontrase(100,matriz) 

# se encuentra que el P mas pequeño que es sigmificante y genera ruido blanco es el 39
#verificación 
etapa1.1 <- Etapas1(matriz,"etapa1",39)
dimension1 <-length(etapa1.1)
if( etapa1.1[dimension1]<Valor_tautau[2]) {#se toma la siginficancia al 5% del estadisitico
  "Gamma es estadisticamente diferente de cero, es significativo"
} else {
  "Gamma es estadisticamente  cero, no es significativo"
}

#se encuentra que el estadistico tipo t generado del coeficiente gamma es significantivo por lo tanto 
#el estadistico es de alta potencia

#verificación de la variable diferenciada sin agregar un orden p a la regresión, 
etapa1.2 <- Etapas(matriz,"etapa1")
if( etapa1.2[3]<Valor_tautau[2]) {#se toma la siginficancia al 5% del estadisitico
  "Gamma es estadisticamente diferente de cero, es significativo"
} else {
  "Gamma es estadisticamente  cero, no es significativo"
}

#se encuentra que el estadistico tipo t generado del coeficiente gamma es significantivo por lo tanto 
#el estadistico es de alta potencia

#como se tuvo que realizar una diferenciacion de la serie para encontrar la significacia del estadistico
#se concluye queel orden de integración de esta serie es I(1) por lo que se tiene Raiz Unitaria



























