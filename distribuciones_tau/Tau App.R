#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(stargazer)
# Define UI for application that draws a histogram
interfaz <- fluidPage(
  
  # Application title
  titlePanel("Modelos Tau"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("datos",
                  "cantidad de datos:",
                  min = 10,
                  max = 500,
                  value = 100),
      sliderInput("repeticiones",
                  "Cantidad de repeticiones:",
                  min = 10,
                  max = 50000,
                  value = 100),
      helpText("Nota: para un numero de repeticiones muy grande puede tomar algunos segundos en actualizar la grafica"),
    ),
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Header + table of distribution ----
      h4("Valores Criticos de las distribuciones"),
      tableOutput("view"),
      h4("               Grafica tau"),
      plotOutput("grafiquita"),
      h4("               Grafica taumiu"),
      plotOutput("grafiquita1"),
      h4("               Grafica tautau"),
      plotOutput("grafiquita2")

    )
  )
)


# Define server logic required to draw a histogram

back <- function(input, output) {
  
  
  output$view <- renderTable({
    
    
    repeticiones <- input$repeticiones
    valores <- input$datos
    matriz <- matrix(data = NA,nrow = valores,ncol = repeticiones)
    
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
      matriz[,j] <- X1
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
    
    verificarse1 <- TaoTest(matriz,repeticiones,"tao")
    verificarse2 <- TaoTest(matriz,repeticiones,"taomiu")
    verificarse3 <- TaoTest(matriz,repeticiones,"taotao")
    
    
    a <- round(quantile(verificarse1,probs = c(0.01,0.05,0.1)),4)
    a <- c("tau",a)
    b <- round(quantile(verificarse2,probs = c(0.01,0.05,0.1)),4)
    b <- c("taumiu",b)
    c <- round(quantile(verificarse3,probs = c(0.01,0.05,0.1)),4)
    c <- c("tautau",c)
    
    tabulacion <- as.matrix(rbind(a,b,c))
    
    tabulacion
    
    
  }
  )
  output$grafiquita <- renderPlot({
    
    
    repeticiones <- input$repeticiones
    valores <- input$datos
    matriz <- matrix(data = NA,nrow = valores,ncol = repeticiones)

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
      matriz[,j] <- X1
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
    verificarse1 <- TaoTest(matriz,repeticiones,"tao")

    
    source("~/Documents/GitHub/Take Home final/Histograma.R")
    proceso1 <- "tao"
    g1 <- histograma(verificarse1,proceso = proceso1)
    
    g1


    
  }
  )
  output$grafiquita1 <- renderPlot({
    
    
    repeticiones <- input$repeticiones
    valores <- input$datos
    matriz <- matrix(data = NA,nrow = valores,ncol = repeticiones)
    
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
      matriz[,j] <- X1
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

    verificarse2 <- TaoTest(matriz,repeticiones,"taomiu")

    
    source("~/Documents/GitHub/Take Home final/Histograma.R")

    
    proceso2 <- "taomiu"
    g2 <- histograma(verificarse2,proceso = proceso2)
    

    
    g2
    
    #grid.arrange(g1,g2,g3)
    
  }
  )
  output$grafiquita2 <- renderPlot({
    
    
    repeticiones <- input$repeticiones
    valores <- input$datos
    matriz <- matrix(data = NA,nrow = valores,ncol = repeticiones)
    
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
      matriz[,j] <- X1
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
    
    verificarse3 <- TaoTest(matriz,repeticiones,"taotao")
    source("~/Documents/GitHub/Take Home final/Histograma.R")
    proceso3 <- "taotao"
    g3 <- histograma(verificarse3,proceso = proceso3)
    g3
    
  }
  )

  
}
# Run the application 
shinyApp(ui = interfaz, server = back)



