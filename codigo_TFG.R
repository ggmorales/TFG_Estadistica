library(shiny)

ui <- fluidPage(
  titlePanel("Construya su árbol de decisión"),
  fileInput("datos","Cargue su base de datos: ",accept=".xlsx"),
  mainPanel("Recuerde que en su base de datos de excel el separador decimal debe ser la coma, y las variables cualitativas deben estar etiquetadas y no codificadas por números. Además, la variable respuesta debe corresponderse con la última columna de su base de datos."),
  mainPanel("."),
  mainPanel("."),
  numericInput("p_entrenamiento","Indique la proporción que quiere dedicar a entrenamiento de su base de datos en formato decimal: ",value=NULL),
  numericInput("p_testeo","Indique la proporción que quiere dedicar a testeo de su base de datos en formato decimal: ",value=NULL),
  verbatimTextOutput("p_prediccion"),
  plotOutput("plot"),
  verbatimTextOutput("precision"),
  tableOutput("prediccion")
)

server <- function(input, output, session) {
  output$p_prediccion<-renderPrint({
    paste("La proporción dedicada a predicción de su base de datos será: ",1-input$p_entrenamiento-input$p_testeo)
  })
  output$plot<-renderPlot({
    library(readxl)
    library(caret)
    library(rpart)
    library(rpart.plot)
    
    # Leer la base de datos
    datos<-data.frame(read_excel(input$datos$datapath, 1))
    colnames(datos)[ncol(datos)]<-"respuesta"
    
    # Particion de la base de datos
    set.seed(1)
    registros_entrenamiento <- createDataPartition(datos$respuesta, p = input$p_entrenamiento, list = F)
    datos_entrenamiento <- datos[registros_entrenamiento,]
    
    # Entrenamiento
    entrenamiento<-function(datos_entrenamiento){
      n_filas<-nrow(datos_entrenamiento)
      arbol_decision <- rpart(respuesta ~ ., data = datos_entrenamiento, method = "class", cp=-1, minsplit=0.1*n_filas, parms = list(split = "information"))
      plot<-rpart.plot(arbol_decision, extra = 4, under= F, fallen.leaves= T, main = "Árbol de decisión para predecir categorías")
      return(list(arbol_decision,plot))
    }
    arbol_plot<-entrenamiento(datos_entrenamiento)
    print(arbol_plot[[2]])
  })
  output$precision<-renderPrint({
    library(readxl)
    library(caret)
    library(rpart)
    library(rpart.plot)
    
    
    # Leer la base de datos
    datos<-data.frame(read_excel(input$datos$datapath, 1))
    colnames(datos)[ncol(datos)]<-"respuesta"
    
    # Particion de la base de datos
    set.seed(1)
    registros_entrenamiento <- createDataPartition(datos$respuesta, p = input$p_entrenamiento, list = F)
    datos_entrenamiento <- datos[registros_entrenamiento,]
    
    # Variable temporal
    temp <- datos[-registros_entrenamiento,]
    registros_testeo <- createDataPartition(temp$respuesta, p = input$p_testeo/(1-input$p_entrenamiento), list = F)
    datos_testeo <- temp[registros_testeo,]
    
    # Entrenamiento
    entrenamiento<-function(datos_entrenamiento){
      n_filas<-nrow(datos_entrenamiento)
      arbol_decision <- rpart(respuesta ~ ., data = datos_entrenamiento, method = "class", cp=-1, minsplit=0.1*n_filas, parms = list(split = "information"))
      return(arbol_decision)
    }
    arbol_plot<-entrenamiento(datos_entrenamiento)
    
    # Testeo
    testeo<-function(datos_testeo,arbol){
      
      n_columnas<-ncol(datos_testeo)
      
      #Aplicamos nuestro arbol al conjunto de testeo
      testeo <- predict(arbol, datos_testeo[,-n_columnas], type = "class")
      
      #Creamos la matriz de confusion para ver que % de registros ha clasificado bien el modelo
      matriz_confusion <- table(datos_testeo[,n_columnas], testeo)
      
      #Calculamos la precision global del modelo sumando los registros de la diagonal principal
      #de la matriz de confusion que son los bien clasificados, y dividimos entre todos los registros de la base de datos,
      #multiplicamos por 100 para que salga porcentaje, y lo rendondeamos a 2 decimales
      precision <- round((sum(diag(matriz_confusion))/nrow(datos_testeo))*100,2)
      return(precision)
    }
    paste("El algoritmo clasifica correctamente el ",testeo(datos_testeo,arbol_plot),"% de los registros",sep="")
  })
  output$prediccion<-renderTable({
    library(readxl)
    library(caret)
    library(rpart)
    library(rpart.plot)
    
    # Leer la base de datos
    datos<-data.frame(read_excel(input$datos$datapath, 1))
    colnames(datos)[ncol(datos)]<-"respuesta"
    
    # Particion de la base de datos
    set.seed(1)
    registros_entrenamiento <- createDataPartition(datos$respuesta, p = input$p_entrenamiento, list = F)
    datos_entrenamiento <- datos[registros_entrenamiento,]
    
    # Variable temporal
    temp <- datos[-registros_entrenamiento,]
    registros_testeo <- createDataPartition(temp$respuesta, p = input$p_testeo/(1-input$p_entrenamiento), list = F)
    datos_testeo <- temp[registros_testeo,]
    datos_prediccion <- temp[-registros_testeo,]
    n_columnas<-ncol(datos)
    datos_prediccion <- datos_prediccion[,-n_columnas]
    
    # Entrenamiento
    entrenamiento<-function(datos_entrenamiento){
      n_filas<-nrow(datos_entrenamiento)
      arbol_decision <- rpart(respuesta ~ ., data = datos_entrenamiento, method = "class", cp=-1, minsplit=0.1*n_filas, parms = list(split = "information"))
      return(arbol_decision)
    }
    arbol_plot<-entrenamiento(datos_entrenamiento)
    
    # Prediccion
    prediccion<-function(datos_prediccion,arbol){
      prediccion <- data.frame("respuesta"=predict(arbol, datos_prediccion, type = "class"))
      return(cbind(datos_prediccion,prediccion))
    }
    datos_prediccion<-prediccion(datos_prediccion,arbol_plot)
    print(datos_prediccion)
  })
}

shinyApp(ui, server)

