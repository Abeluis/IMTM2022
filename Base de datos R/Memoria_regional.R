#### Madurez digital en las provincias de Concepción ##### 

#### 1 Instalación de librerias ####
# El comando es install.packages seguido por () y la libreria en ""

  install.packages("tidyverse")
  install.packages("dplyr")
  install.packages("car")
  install.packages("sjPlot")
  install.packages("modeest")
  install.packages("summarytools")
  install.packages("psych")
  install.packages("chilemapas")
  install.packages("ggplot2")
  install.packages("sf")
  install.packages("leaflet")
  install.packages("sp")
  install.packages("plotly")

   
#### 2 Cargar librerias ####
# El comando library seguido por () y la libreria en ""

  library("tidyverse")
  library("dplyr")
  library("car")
  library("sjmisc")
  library ("sjPlot")
  library("modeest")
  library("summarytools")
  library("psych")
  library("chilemapas")
  library("sf")
  library("ggplot2")
  library("leaflet")
  library("sp")
  library("plotly")
  
#### 3 Crear base de datos ##### 
# Municipalidades de la provincia 
  
  municipalidades <- c("Cañete", "Chiguayante", "Concepción", "Coronel", 
                       "Florida", "Hualpén", "Hualqui", "Lota", 
                       "Penco", "Quilleco", "San Pedro de la Paz", 
                       "Santa Juana", "Talcahuano", "Tome")
  provincia <- c("Arauco", "Concepcion")

#### 3. Objetivo general (IMTM) ####
#### Caracterizar la madurez digital de los municipios de la 
###  provincia de Concepción al año 2022 según el Índice de 
## madurez tecnológica Municipal.  
  
#### 3.1 Infraestructura Tecnológica (IT) ####
  seg_informatica <- c(0.75,0,0,0,0,0,0,1,1,0.75,0,0,0,1)
  num_servidores <-  c(0.06,0,0,0,0,0,0,0,0.13,0.2,0,0,0,0.20)
# Calcular IT con las variables 
  infraestructura_tecnologica <- (seg_informatica + 
                                    num_servidores) / 2
  
  
# Convertir infraestructura_tecnologica en IT dataframe  
  IT <- data.frame(municipalidades, seg_informatica, 
                   num_servidores, infraestructura_tecnologica)
  
#### 3.2 Recursos Humanos del área informática (RRHH) ####
  area_informatica     <- c(0,0,0,0,0,0,0,1,1,1,0,0,0,1)
  estudios_encargado   <- c(0.5,0,0,0,0,0,0,1,1,1,0,0,0,0.5)
  org_area_informatica <- c(1,0,0,0,0,0,0,0.4,1,0.2,0,0,0,0.4)

  recursos_humanos <- (area_informatica + estudios_encargado + 
                         org_area_informatica) / 3 
  
 
  RRHH <- data.frame(municipalidades, area_informatica, 
                     estudios_encargado, org_area_informatica, 
                     recursos_humanos) 
  
  
#### 3.3 Gestión Tecnológica Municipal (GTM) ####
  intranet_municipal    <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,1)
  procesos_internos     <- c(0.38,0,0,0,0,0,0,0.33,0.35,0.72,0,0,0,0.88)
  estrategia_desarrollo <- c(0,0,0,0,0,0,0,0,1,1,0,0,0,1)
  
  gestion_tecno_municipal <- (intranet_municipal + procesos_internos
                                    + estrategia_desarrollo) /3 
  
  GTM <- data.frame(municipalidades, intranet_municipal, procesos_internos, 
                    estrategia_desarrollo, gestion_tecno_municipal)
  
  
#### 3.4 Servicios Municipales en línea (SML) ####
  serv_municipales_enlinea <- c(0.22,0,0,0,0,0,0,0.16,0,0.16,0,0,0,0.27)
  

  SML <- data.frame(municipalidades, serv_municipales_enlinea)
  
  
  
#### 3.5 Calcular el IMTM #### 
  indice_2022 <- (infraestructura_tecnologica + recursos_humanos 
           + gestion_tecno_municipal + serv_municipales_enlinea) / 4
  
  IMTM_2022 <- data.frame(municipalidades, infraestructura_tecnologica, 
                     recursos_humanos, gestion_tecno_municipal,
                     serv_municipales_enlinea, indice_2022)
  
  
#### 5 Objetivo general #### 
  # analizar el nivel de madurez digital en los municipios
  ## de la provincia de Concepción al año 2022
  
  print(indice_2022)
  table(indice_2022)
  
#### 6.1 Objetivo específico 1 ####
### Estadística descriptiva del IMTM   
  
  min <- min(IMTM_2022$indice_2022, na.rm = TRUE)
  q1 <- quantile(IMTM_2022$indice_2022, probs = 0.25, na.rm = TRUE)
  media <- mean.default(IMTM_2022$indice_2022, na.rm = TRUE)
  media_rec <- mean.default(IMTM_2022$indice_2022, trim = 0.025, na.rm = TRUE)
  mediana <- median.default(IMTM_2022$indice_2022, na.rm = TRUE)
  moda <- mfv(IMTM_2022$indice_2022)
  var <- var(IMTM_2022$indice_2022, na.rm = TRUE)
  desvest <- sd(IMTM_2022$indice_2022, na.rm = TRUE)
  q3 <- quantile(IMTM_2022$indice_2022, probs = 0.75, na.rm = TRUE)
  max <- max(IMTM_2022$indice_2022, na.rm = TRUE)
  s <- skew(IMTM_2022$indice_2022)  
  c <- kurtosi(IMTM_2022$indice_2022)
  
  IMTM_2022_descr <- as.numeric(c(min, q1, media, media_rec, mediana, moda,
                                  var, desvest, q3, max, s, c))
  nombres <- c("Mínimo", "Q1", "Media", "Media recortada", "Mediana",
               "Moda", "Varianza", "Desviación Estándar", "Q3", 
               "Máximo", "Simetría", "Curtosis")
  
  Estadisticos_IMTM_2022 <- as.data.frame(rbind(nombres,IMTM_2022_descr))
  write.csv2(Estadisticos_IMTM_2022, file = "Estadisticos_IMTM_2022.csv")
  
  # Transformar variable indice_2022 a factor (contar con valores y etiquetas)
  IMTM_2022_select <- select(IMTM_2022, municipalidades, DIMENSION_IT = 
          infraestructura_tecnologica, DIMENSION_RRHH = recursos_humanos, 
          DIMENSION_GTM = gestion_tecno_municipal, DIMENSION_SML = 
            serv_municipales_enlinea, IMTM_2022_FACTOR = indice_2022)
  
   IMTM_2022_select <- mutate(IMTM_2022_select, indice_2022_factor = 
                               factor(IMTM_2022_select$IMTM_2022_FACTOR,
                                      labels = c("Muy bajo", "Bajo", "Alto", "Muy Alto")))
  
### Gráficos 
## Gráfico con ggplot 
  ggplot(IMTM_2015, aes(x = )) +
    geom_bar(width =  0.4, fill = rgb(0.1,1,0.5,0.7)) + 
    scale_x_discrete("Nivel IMTM") + # Configuracion de la etiqueta del eje x 
    scale_y_continuous("Frecuencia") +
    labs(title = "IMTM 2022",
         subtitle = "Provincia de concepción")
  
 
  
#### 6.2 Dimension IT 
### Medidas de tendencia  central: Promedio, media y moda
  mean(IMTM_2022$infraestructura_tecnologica)
  median(IMTM_2022$infraestructura_tecnologica)
  mfv(IMTM_2022$infraestructura_tecnologica)

### Cuartiles   
  quantile(IMTM_2022$infraestructura_tecnologica, 
           probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
### Medidas de dispersión: rango, varianza, desviación estándar 
## coeficiente de variación
# Rango max y min separado
  min(IMTM_2022$infraestructura_tecnologica, na.rm = TRUE)
  max(IMTM_2022$infraestructura_tecnologica, na.rm = TRUE)
# Varianza
  var(IMTM_2022$infraestructura_tecnologica, na.rm = TRUE)
# Desviación estandar
  sd(IMTM_2022$infraestructura_tecnologica, na.rm = TRUE) 
# Coeficiente de variacion
  sd(IMTM_2022$infraestructura_tecnologica)/
  mean(IMTM_2022$infraestructura_tecnologica)
  
## Forma de la distribucion 
# Simetria
  skew(IMTM_2022$infraestructura_tecnologica) 
# Curtosis 
  kurtosi(IMTM_2022$infraestructura_tecnologica)
# Coeficientes de simetria estandarizados
  skew(IMTM_2022$infraestructura_tecnologica)/sqrt(6/1443)  

## 
  
  
  
 
  # Continuar con Estadística descriptiva de las dimensiones del IMTM
  
  
  #### 6.2 Objetivo especifico 2 #### 
  #### Clasificar la madurez digital de los municipios 
  ###  de la provincia de Concepción al año 2022 
  ##   en categorías equivalentes a las tipologías del FIGEM. 
  
  
  
  #### 6.3 Objetivo específico 3 #### 
  ### Comparar la madurez digital entre los 
  ## municipios de la provincia de Concepción al año 2022.  
  
  
  
  #### 6.4 Objetivo especifico 4####
  #### Comparar la madurez digital de los municipios de la
  ### provincia de Concepción al año 2022 y los resultados 
  ## obtenidos en el “Estudio de evaluación del nivel de 
  # digitalización en municipios” en el año 2015. 
  
  #### IMTM 2015 #### 
  indice_2015 <- c(0.28, 0.66, 0.8, 0.5, 0.49, 0.31, 0.42, 0.26,
                   0.54, 0.45, 0.4, 0, 0.69, 0.45) 
  IMTM_2015 <- data.frame(municipalidades, indice_2015)
  
  median(indice_2015)
  sd(indice_2015)
  median(indice_2022)
  
  objetivo_4 <- (indice_2022 - indice_2015)
  objetivo_4 <- data.frame(municipalidades, indice_2015, 
                           indice_2022, objetivo_4)
  
    #### 4 Estadistica descriptiva basica ####
  descr_IT <- summary(IT)
  descr_RRHH <- summary(RRHH)
  descr_GTM <- summary(GTM)
  descr_SML <- summary(SML)
  
  descr_IMTM_2022 <- summary(IMTM_2022)
  print(IMTM_2022)
  
  #### Crear mapas ####
  
  
#### Mapa de la provincia de concepcion ####
  chilemapas::codigos_territoriales
 View(chilemapas::codigos_territoriales)
## El código de la región de concepción es 08
## El códgio de la provincia de concepción es 081
## 
 
View(chilemapas::generar_provincias(081))

 
