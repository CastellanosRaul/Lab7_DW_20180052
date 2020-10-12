library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(writexl)
library(readxl)
library(ggmap)
energia <- read_csv("c1.csv")
energia <- energia[,-23:-28]

# Cifras character a numeric ----------------------------------------------

energia$Camion_5 <- str_sub(energia$Camion_5,2)
energia$Pickup <- str_sub(energia$Pickup,2)
energia$Moto <- str_sub(energia$Moto,2)
energia$factura <- str_sub(energia$factura,2)
energia$directoCamion_5 <- str_sub(energia$directoCamion_5,2)
energia$directoPickup <- str_sub(energia$directoPickup,2)
energia$directoMoto <- str_sub(energia$directoMoto,2)
energia$fijoCamion_5 <- str_sub(energia$fijoCamion_5,2)
energia$fijoPickup <- str_sub(energia$fijoPickup,2)
energia$fijoMoto <- str_sub(energia$fijoMoto,2)

energia$Camion_5 <- ifelse(energia$Camion_5 == "-", NA, energia$Camion_5)
energia$Pickup <- ifelse(energia$Pickup == "-", NA, energia$Pickup)
energia$Moto <- ifelse(energia$Moto == "-", NA, energia$Moto)
energia$factura <- ifelse(energia$factura == "-", NA, energia$factura)
energia$directoCamion_5 <- ifelse(energia$directoCamion_5 == "-", NA, energia$directoCamion_5)
energia$directoPickup <- ifelse(energia$directoPickup == "-", NA, energia$directoPickup)
energia$directoMoto <- ifelse(energia$directoMoto == "-", NA, energia$directoMoto)
energia$fijoCamion_5 <- ifelse(energia$fijoCamion_5 == "-", NA, energia$fijoCamion_5)
energia$fijoPickup <- ifelse(energia$fijoPickup == "-", NA, energia$fijoPickup)
energia$fijoMoto <- ifelse(energia$fijoMoto == "-", NA, energia$fijoMoto)

energia$Camion_5 <- as.numeric(energia$Camion_5)
energia$Pickup <- as.numeric(energia$Pickup)
energia$Moto <- as.numeric(energia$Moto)
energia$factura <- as.numeric(energia$factura)
energia$directoCamion_5 <- as.numeric(energia$directoCamion_5)
energia$directoPickup <- as.numeric(energia$directoPickup)
energia$directoMoto <- as.numeric(energia$directoMoto)
energia$fijoCamion_5 <- as.numeric(energia$fijoCamion_5)
energia$fijoPickup <- as.numeric(energia$fijoPickup)
energia$fijoMoto <- as.numeric(energia$fijoMoto)


# Limpieza tiempos ---------------------------------------------------------

energia$`5-30` <- ifelse(is.na(energia$`5-30`),FALSE,TRUE)
energia$`30-45` <- ifelse(is.na(energia$`30-45`),FALSE,TRUE)
energia$`45-75` <- ifelse(is.na(energia$`45-75`),FALSE,TRUE)
energia$`75-120` <- ifelse(is.na(energia$`75-120`),FALSE,TRUE)
energia$`120+` <- ifelse(is.na(energia$`120+`),FALSE,TRUE)

# Coordenadas -------------------------------------------------------------

energia<- unite(energia, Coordenada, c(Lat,Long),sep = ",")

# Estado de Resultados ----------------------------------------------------

Ventas <- sum(energia$factura)
Costos <- sum(energia$Camion_5,energia$Pickup,energia$Moto, na.rm = TRUE)
UtilidadBruta <- Ventas-Costos
ER <- as.data.frame(c(Ventas, Costos, UtilidadBruta), c("Ventas", "Costo por Servicio", "Utilidad Bruta"))
colnames(ER) <- "ER"

# Camion ------------------------------------------------------------------

Camion <- energia[,-c(4,5,11,12,14,15)]
Camion <- na.omit(Camion)
UC <- Camion %>% group_by(Cod) %>% summarise(CMin = min(Camion_5),
                                             CP = mean(Camion_5),
                                             CMax = max(Camion_5),
                                             .groups = 'drop')
# Pick Up -----------------------------------------------------------------

PickUp <- energia[,-c(3,5,10,12,13,15)]
PickUp <- na.omit(PickUp)
UP <- PickUp %>% group_by(Cod) %>% summarise(CMin = min(Pickup),
                                             CP = mean(Pickup),
                                             CMax = max(Pickup),
                                             .groups = 'drop')

# Motos -------------------------------------------------------------------

Motos <- energia[,-c(3,4,10,11,13,14)]
Motos <- na.omit(Motos)
UM <- Motos %>% group_by(Cod) %>% summarise(CMin = min(Moto),
                                             CP = mean(Moto),
                                             CMax = max(Moto),
                                             .groups = 'drop')


# Ubicacion de los postes -------------------------------------------------

Postes <- energia %>% group_by(Coordenada) %>% 
  summarise(Cantidad = sum(factura), .groups = 'drop') %>% 
  arrange(desc(Cantidad))

Postes$Porcentaje <- Postes$Cantidad/sum(Postes$Cantidad)*100
Postes <- read_excel("Postes.xlsx")

Postes$C <- 1:nrow(Postes)
Rango <- c(0,14848,74240)
Postes$Intervalo <- cut(Postes$C, breaks = Rango, right = FALSE)
Pareto <- Postes %>% group_by(Intervalo) %>% summarise(Cantidad = sum(Cantidad), .groups = 'drop')

Pareto$Cantidad <- format(Pareto$Cantidad, big.mark = ',')
dato <- as.data.frame(t(data.frame("0-14k", "14.001K-74.24K")))
Pareto <- cbind.data.frame(Pareto, dato)
Pareto <- Pareto[,-1]
colnames(Pareto) <- c("Cantidad", "Intervalo")

ggplot(Pareto, aes(Intervalo, Cantidad)) + 
  geom_bar(stat = "identity", fill = 'blue') +
  geom_text(aes(label = Cantidad), vjust = 1.6, color = 'white', size = 3.5)


# Centros -----------------------------------------------------------------
centros <- energia %>% group_by(origen, Fecha) %>% summarise(C = n(), Factura = sum(factura), .groups = 'drop')
centros$mes <- substr(centros$Fecha, 4,5)  

centros <- centros %>% group_by(origen, mes) %>% summarise(Cantidad = sum(C), .groups = 'drop')

ggplot(centros, aes(mes, Cantidad, fill = origen)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~origen) + 
  geom_text(aes(label = Cantidad),position = position_dodge(width = 0.5), vjust = -0.25) + 
  labs(title = "Graficas por cantidad mensual de servicios")


# Recorridos --------------------------------------------------------------
energia2 <- read_csv("c1.csv")
energia2 <- energia2[,8:9]

energia <- cbind.data.frame(energia, energia2)

facturas <- energia %>% select(Fecha, Cod, factura, Long, Lat, Coordenada)
cods <- split(facturas, facturas$Cod)
fac <- function(x){
  x <- x
  x$mes <- substr(x$Fecha, 4,5)
  x1 <- x %>% group_by(mes, Coordenada) %>% 
    summarise(Factura_Max = max(factura),.groups = 'drop') %>% 
    arrange(desc(Factura_Max))
  x2 <- x1[1,]
  return(x2)
}
cods_factura <- lapply(cods, fac)

cods_factura <- bind_rows(cods_factura, .id = "cods_factura")
cods_factura$Lat <- substr(cods_factura$Coordenada,1,10)
cods_factura$Long <- substr(cods_factura$Coordenada,13,23)
cods_factura$Lat <- as.numeric(cods_factura$Lat)
cods_factura$Long <- as.numeric(cods_factura$Long)
cods_factura$Lat <- round(cods_factura$Lat)
cods_factura$Long <- round(cods_factura$Long)

library(geosphere)
# https://stackoverflow.com/questions/37444722/r-calculating-distance-between-2-points-on-earth-using-package-geosphere
p1 <- c(0,0)
cods_factura$distancia <- 0
cods_factura[1,7] <- (distm(p1, c(89,14), fun = distHaversine))/1609
cods_factura[2,7] <- (distm(p1, c(-91,15), fun = distHaversine))/1609
cods_factura[3,7] <- (distm(p1, c(-89,15), fun = distHaversine))/1609
cods_factura[4,7] <- (distm(p1, c(-91,15), fun = distHaversine))/1609
cods_factura[5,7] <- (distm(p1, c(-91,16), fun = distHaversine))/1609
cods_factura[6,7] <- (distm(p1, c(-90,15), fun = distHaversine))/1609
cods_factura[7,7] <- (distm(p1, c(-90,15), fun = distHaversine))/1609
cods_factura[8,7] <- (distm(p1, c(90,15), fun = distHaversine))/1609
cods_factura[9,7] <- (distm(p1, c(-91,15), fun = distHaversine))/1609
cods_factura[10,7] <- (distm(p1, c(-89,16), fun = distHaversine))/1609

cods_factura <- cods_factura %>% select(mes, cods_factura, Coordenada, distancia, Factura_Max)
cods_factura <- cods_factura %>% arrange(desc(Factura_Max))

