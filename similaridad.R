#cargar librerias necesarias, instalar antes
require(RCurl)
require(XML)
require(textrank)
library(dplyr)
library(tidytext)
library(tm)
library(DT)

# cargamos un rastreo de screaming frog (al azar uno de los perdedores de esta semana en Sistrix)
# necesitamos tener un dataframe con las URLs a analizar
df <- read.csv("internal_html.csv", stringsAsFactors = FALSE)
# si usais la rana con version menor de 12.2 cambiad la linea anterior por esta:
#df <- read.csv("internal_html.csv",skip = 1, stringsAsFactors = FALSE)

# nos quedamos solo con la columan que tiene las URLs y que sean indexables
web <- subset(df, Indexability == "Indexable")
web <- select(web, Address)

# opcional: filtrar solo paginas por Regex (opcional), el ejemplo de abajo solo coje las que acaban en .html
#web <- subset(web, subset = !grepl(".html",web$Address) == TRUE, select = Address)

# carga de varias funciones
source("funciones.R")

# extraemos el texto de cada URL y lo limpiamos
urls <- length(web$Address)
for (i in 1:urls) {
  try (web$Contenido[[i]] <- htmlToText(web$Address[[i]]))
  cat(paste0("Exrayendo url ",i, " de ",urls,"\n"))
}

# tokenización de los textos
for (i in 1:length(web$Contenido)){
  web$tokens[i] <- as.character(web$Contenido[i])
  x <- as.character(web$Contenido[i])
  web$tokens[i] <- strsplit(x, " ", fixed = TRUE)
}

# preparamos una lista para empezar a comparar URLs
a <- 1
lista <- list()

# recorremos cada URL y la comparamos con el resto mediante el Índice Jaccard (obviamos repetir URLs y que se compare consigo misma)
for (n in 1:length(web$Contenido)){
  for (i in 1:length(web$Contenido)){
    if (n < i){
      lista[[a]] <- c(web$Address[n],web$Address[i],round(textrank_jaccard(web$tokens[[n]],web$tokens[[i]])*100))
      cat(paste0("Comparando URL ", n , " con URL ", i , "\n"))
    }
    a <- a+1
  }
}

# montamos el dataframe y renombramos las columnas
similaridad <- do.call(rbind.data.frame, lista)
names(similaridad)[1] <- "URL1"
names(similaridad)[2] <- "URL2"
names(similaridad)[3] <- "SIMILARIDAD"
similaridad$SIMILARIDAD <- as.numeric(similaridad$SIMILARIDAD)

# abrimos un datatable con filtros para ver mejor los datos
datatable(similaridad, filter = "top")

# diagrama de cajas y bigotes, todos los valores por encima del máximo (sobre 62% aprox.) seria los primeros a solucionar
boxplot(similaridad$SIMILARIDAD, main = "Resumen de similaridad")

# seleccionamos y filtramos datos para trabajar
alta.similaridad <- subset(similaridad, SIMILARIDAD>62)

# podeis pasarlo a csv para trabajarlo en excel o spreedsheets
write.csv(alta.similaridad, "", fileEncoding = "UTF-8")

# en el horno:
# clusteriazación de URLs automática
# agrupación de URLs por tipo de páginas (ej. fichas de produxcto, blog, etc)