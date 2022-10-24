library(tidyverse)
library(dplyr)
library(data.table)
library(lfactors)
library(stringi)
library(ggplot2)
library(sf)

############ Prepare shapefile
shapes_county <- st_read("maps/gadm36_PRT_2.shp")
shapes_county$NAME_2 <- toupper(stri_trans_general(shapes_county$NAME_2, "Latin-ASCII"))
shapes_county$NAME_1 <- toupper(stri_trans_general(shapes_county$NAME_1, "Latin-ASCII"))

shapes <- shapes_county[!(shapes_county$NAME_1 %in% c("AZORES","MADEIRA")),]

stri_trans_general(shapes_county$NAME_2, "ASCII-Latin")


#inserir GOIS

TB_maps <- select(TB1, c("ACES", "county", "incidence"))
GOIS.inc <- data.frame("Pinhal Interior Norte", "GOIS", as.double("7.05"))
names(GOIS.inc) <- c("ACES", "county", "incidence")
TB_maps2 <- rbind(TB_maps, GOIS.inc)

#mapa incidencia
dataset <- merge(shapes[,c("NAME_2","geometry")],
                 TB_maps2[,c("county", "incidence")],
                 by.x = "NAME_2", by.y = "county", all.x = TRUE, all.y = TRUE)
dataset <- dataset[!is.na(dataset$incidence),]
dataset <- dataset[!is.na(dataset$NAME_2), ]

ggplot(dataset) +
  geom_sf(aes(fill = incidence), color = NA) +
  scale_fill_gradient(low = "#D7DA8F",
                      high = "#333333",
                      guide = "colorbar")


#mapa atrasos
dataset2 <- merge(shapes[,c("NAME_2","geometry")],
                 TB1[TB1$included == 1,
                     c("county", "incidence", "delay_health", "delay_pat", "delay_global")],
                 by.x = "NAME_2", by.y = "county", all.x = TRUE, all.y = TRUE)
dataset2 <- dataset2[!is.na(dataset2$incidence),]
dataset2 <- dataset2[!is.na(dataset2$NAME_2), ]


ggplot(dataset2) +
  geom_sf(aes(fill = as.numeric(delay_global))) +
  scale_fill_gradient(low = "#A8DDE2",
                      high = "#000000",
                      guide = "colorbar")

ggplot(dataset2) +
  geom_sf(aes(fill = as.numeric(delay_pat))) +
  scale_fill_gradient(low = "#96DCA8",
                      high = "#000000",
                      guide = "colorbar")

ggplot(dataset2) +
  geom_sf(aes(fill = as.numeric(delay_health))) +
  scale_fill_gradient(low = "#E0A1A5",
                      high = "#000000",
                      guide = "colorbar")


#mapa atrasos por ACES
dataset3 <- merge(shapes[,c("NAME_2","geometry")],
                  TB_delayACES[TB_delayACES$included == 1,
                               c("county", "delay_health", "delay_pat", "delay_global")],
                  by.x = "NAME_2", by.y = "county", all.x = TRUE, all.y = TRUE)
dataset3 <- dataset3[!is.na(dataset3$NAME_2), ]


ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(delay_global))) +
  scale_fill_gradient(low = "#A8DDE2",
                      high = "#000000",
                      guide = "colorbar")

ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(delay_pat))) +
  scale_fill_gradient(low = "#96DCA8",
                      high = "#000000",
                      guide = "colorbar")

ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(delay_health))) +
  scale_fill_gradient(low = "#E0A1A5",
                      high = "#000000",
                      guide = "colorbar")

