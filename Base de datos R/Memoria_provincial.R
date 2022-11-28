#### Madurez digital en las provincias de Concepción ##### 

#### 1 Instalación de librerias ####
# El comando es install.packages seguido por () y la libreria en ""

  install.packages("tidyverse")
  install.packages("dplyr")
  install.packages("car")
  install.packages("sjPlot")
  install.packages("chilemapas")
  instal
#### 2 Cargar librerias ####
# El comando library seguido por () y la libreria en ""

  library("tidyverse")
  library("dplyr")
  library("car")
  library ("sjPlot")
  library("chilemapas")
  
#### 3 Crear base de datos ##### 
# Municipalidades de la provincia 
  
  municipalidades <- c("Chiguayante", "Concepción", "Coronel", 
                       "Florida", "Hualpén", "Hualqui", "Lota", 
                       "Penco", "San Pedro de la Paz", 
                       "Santa Juana", "Talcahuano", "Tome")

#### 3. Objetivo general 1 ####
#### 3. Indice  de madurez tecnológica municipal (IMTM) ####
#### Caracterizar la madurez digital de los municipios de la 
###  provincia de Concepción al año 2022 según el Índice de 
## madurez tecnológica Municipal.  
  
#### 3.1 Infraestructura Tecnológica (IT) ####
  seg_informatica <- c(1,2,3,4,5,6,7,8,9,0,1,2)
  num_servidores <- c(0,0,0,0,0,0,0,0,0,0,0,0)
# Calcular IT con las variables 
  infraestructura_tecnologica <- (seg_informatica + 
                                    num_servidores) / 2 
  
# Convertir infraestructura_tecnologica en IT dataframe  
  IT <- data.frame(municipalidades, seg_informatica, 
                   num_servidores, infraestructura_tecnologica)
  
  #### 3.2 Recursos Humanos del área informática (RRHH) ####
  area_informatica <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  estudios_encargado <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  org_area_informatica <- c(0,0,0,0,0,0,0,0,0,0,0,0)

  recursos_humanos <- (area_informatica + estudios_encargado + 
                         org_area_informatica) / 3 
  
 
  RRHH <- data.frame(municipalidades, area_informatica, 
                     estudios_encargado, org_area_informatica, 
                     recursos_humanos) 
  
  
#### 3.3 Gestión Tecnológica Municipal (GTM) ####
  intranet_municipal <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  procesos_internos <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  estrategia_desarrollo <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  
  gestion_tecno_municipal <- (intranet_municipal + procesos_internos
                                    + estrategia_desarrollo) /3 
  GTM <- data.frame(intranet_municipal, procesos_internos, 
                    estrategia_desarrollo, gestion_tecno_municipal)
  
  
#### 3.4 Servicios Municipales en línea (SML) ####
  serv_municipales_enlinea <- c(0,0,0,0,0,0,0,0,0,0,0,0)
  

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
  
  print(IMTM_2022)
  table(IMTM_2022)
  
  #### 6.1 Objetivo específico 1 #### 
  
  resultados_0 <- c("Mediana", "Desviacion tipica") 
  median(infraestructura_tecnologica)
  sd(infraestructura_tecnologica)
  median(recursos_humanos)
  sd(recursos_humanos)
  median(gestion_tecno_municipal)
  sd(gestion_tecno_municipal)
  median(serv_municipales_enlinea)
  sd(serv_municipales_enlinea)
  median(indice_2022)
  sd(indice_2022)
  
  resultados_0_IT <- c(0,0)
  resultados_0_RRHH <- c(0.0)
  resultados_0_GTM <- c(0.0)
  resultados_0_SML <- c(0.0)
  resultados_0_IMTM <- c(0.0)
  Resultado_IMTM_2022 <- data.frame(resultados_0, resultados_0_IT, 
                                    resultados_0_RRHH, resultados_0_GTM,
                                    resultados_0_SML, resultados_0_IMTM)
  
  # Continuar con Estadística descriptiva de las dimensiones del IMTM
  
  
  #### 6.2 Objetivo especifico 2 #### 
  #### Clasificar la madurez digital de los municipios 
  ###  de la provincia de Concepción al año 2022 
  ##   en categorías equivalentes a las tipologías del FIGEM. 
  
  
  #### 6.4 Objetivo especifico 4####
  #### Comparar la madurez digital de los municipios de la
  ### provincia de Concepción al año 2022 y los resultados 
  ## obtenidos en el “Estudio de evaluación del nivel de 
  # digitalización en municipios” en el año 2015. 
  
  #### IMTM 2015 #### 
  indice_2015 <- c(0.68, 0.8, 0.5, 0.49, 0.31, 0.42, 0.26,
                   0.54, 0.4, 0, 0.69, 0.45) 
  IMTM_2015 <- data.frame(municipalidades, indice_2015)
  
  
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
  
  chilemapas::generar_regiones()  
  
Mapa <- chilemapas::generar_regiones(8)
  
  
  
  
                       
                       