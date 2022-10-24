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

TB_maps <- dplyr::select(TB1, c("ACES", "county", "incidence"))
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
  scale_fill_gradient("Incidência\npor ACES",
                      low = "#cce8c5",
                      high = "#b3373c",
                      guide = "colorbar") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank())

#mapa limpo dos ACES
ggplot(dataset) +
  geom_sf(aes(fill = log(incidence)), color = NA) +
  scale_fill_gradient("ACES",
                      low = "#e7e7e7",
                      high = "#000000",
                      guide = NULL) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank())

#mapa atrasos por ACES

TB_maps <- dplyr::select(TB_delayACES, c("ACES", "county",
                                "ACES_delay_health", "ACES_delay_pat", "ACES_delay_global"))
GOIS.delay <- data.frame("Pinhal Interior Norte", "GOIS", as.double("11.00"), as.double("23.00"), as.double("40.50"))
names(GOIS.delay) <- c("ACES", "county", "ACES_delay_health", "ACES_delay_pat", "ACES_delay_global")
TB_maps3 <- rbind(TB_maps, GOIS.delay)
FIGUEIRO.delay <- data.frame("Pinhal Interior Norte", "FIGUEIRO DOS VINHOS", as.double("11.00"), as.double("23.00"), as.double("40.50"))
names(FIGUEIRO.delay) <- c("ACES", "county", "ACES_delay_health", "ACES_delay_pat", "ACES_delay_global")
TB_maps3 <- rbind(TB_maps3, FIGUEIRO.delay)

dataset3 <- merge(shapes[,c("NAME_2","geometry")],
                  TB_maps3[,c("ACES", "county", "ACES_delay_health", "ACES_delay_pat", "ACES_delay_global")],
                  by.x = "NAME_2", by.y = "county", all.x = TRUE, all.y = TRUE)
dataset3 <- dataset3[!is.na(dataset3$ACES),]
dataset3 <- dataset3[!is.na(dataset3$NAME_2), ]


ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(ACES_delay_global)), color = NA) +
  scale_fill_gradient("Atraso Total\npor ACES",
                      low = "#A8DDE2",
                      high = "#000000",
                      guide = "colorbar") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
  )

ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(ACES_delay_pat)), color = NA) +
  scale_fill_gradient("Atraso do Utente\npor ACES",
                      low = "#96DCA8",
                      high = "#000000",
                      guide = "colorbar") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
  )

ggplot(dataset3) +
  geom_sf(aes(fill = as.numeric(ACES_delay_health)), color = NA) +
  scale_fill_gradient("Atraso dos Serviços de Saúde\npor ACES",
                      low = "#E0A1A5",
                      high = "#000000",
                      guide = "colorbar") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
  )


#IDEIAS PARA VISUALIZAÇÃO DE DADOS (AnÁLISE DESCRITIVA)
#density plot com ATRASOS POR ACES