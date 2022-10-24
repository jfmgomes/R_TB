library(tidyverse)
library(dplyr)
library(readr)
library(sp)
library(rgdal)
library(ggplot2)
library(data.table)
library(lfactors)
library(stringi)
library(compareGroups)
library(sf)



#importar dados
dadosTB <- readRDS("D:/OneDrive - Universidade de Coimbra/TUBERCULOSE/protocolo ENSP TB/Base de dados/dadosJG_2022.RDS")

POP2013 <- read_delim("D:/OneDrive - Universidade de Coimbra/TUBERCULOSE/protocolo ENSP TB/Base de dados/POP2013.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)


#filtrar as variáveis a utilizar
TB1 <- dadosTB %>%
  dplyr::select(ID, sex, age_diag,
         origin_country, country_inc, county, district, ACES,
         unemployment, health_job, alcohol,
         drugs, inmate, homeless,
         commu_resid, HIV, dialysis,
         cancer, inflam, respi, diabetes,
         symptoms_date, appoint_date, diagnosis_date,
         delay_pat, delay_health, delay_global,
         problem_pat, problem_health,
         centro_saude, rurality, benef,
         school, house, doc_rate
         )


#identificação dos NAs
levels(TB1$alcohol)[levels(TB1$alcohol)=='Unknown'] <- NA
levels(TB1$drugs)[levels(TB1$drugs)=='Unknown'] <- NA
levels(TB1$inmate)[levels(TB1$inmate)=='Unknown'] <- NA
levels(TB1$homeless)[levels(TB1$homeless)=='Unknown'] <- NA
levels(TB1$commu_resid)[levels(TB1$commu_resid)=='Unknown'] <- NA
levels(TB1$HIV)[levels(TB1$HIV)=='Unknown'] <- NA

 
#criar variável de incidência no país de origem              
high_incidence <- c("Angola", "Bangladesh", "Democratic Republic of the Congo",
                     "Ethiopia", "India", "Indonesia", "Mozambique", "Myanmar (Burma)",
                     "Pakistan", "Philippines", "Nigeria", "South Africa", "Thailand",
                     "Vietnam", "Central African Republic", "Congo - Brazzaville",
                     "Sierra Leone", "Zimbabwe", "Brazil", "China", "Russia",
                     "Cape Verde", "Ghana", "Guinea-Bissau", "Morocco",
                     "Romania", "São Tomé & Príncipe", "Timor-Leste",
                     "Ukraine")
 
TB1$country_inc <- ifelse(TB1$origin_country == "Portugal", 1,
                               ifelse(TB1$origin_country %in% high_incidence, 2,
                                      ifelse(TB1$origin_country == "Unknown", 4, 3)))

TB1$country_inc <- as.factor(TB1$country_inc)

#MERGE população
POP2013$population <- as.numeric(gsub(" ", "", POP2013$population))

TB1 <- merge(TB1, POP2013, by = "county", all.x = TRUE, all.y = TRUE)
TB1 <- TB1[!is.na(TB1$ID),]


#criar variável dos incluídos / excluídos
TB_semNA <- TB1[!is.na(TB1$delay_pat) & !is.na(TB1$delay_health),]

TB_total <- TB1[TB1$problem_pat %in% c("No inconsistencies",
                                       "Inconsistencies with symptoms onset and microscopy and/or culture")
                &
                  TB1$problem_health %in% c("No inconsistencies",
                                            "Inconsistencies with first appointment and microscopy and/or culture"), ]

TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)


#variável incidência
TB1 <- TB1 %>%
  group_by(ACES) %>%
  mutate(ACES_count = n())

TB1$incidence <- round(TB1$ACES_count / TB1$population_ACES * 100000 / 10, digits=2)

table_incidence <- TB1 %>% 
  group_by(ACES) %>% 
  summarise(Incidencia = mean(incidence)) %>% 
  arrange(Incidencia)
write.csv2(table_incidence, "table_incidence.csv", row.names=FALSE)


tibble_inci <- table_incidence %>%
  gt() %>%
  tab_header(title = md("**Incidência por ACES**"))
    
tibble_inci
  


  
  tbl_summary()  %>%
  modify_header(label = "") %>%
  bold_labels()
  
gtsummary_inci

#variável centro_saude_rate
TB1$centro_saude_rate <- round((TB1$centro_saude / TB1$population * 100000), digits=2)


#VARIÁVEIS ATRASO POR ACES
TB_delayACES <- TB1[TB1$included == 1,] %>%
  dplyr::select(ID, county, ACES, delay_pat, delay_health, delay_global, included) %>% 
  group_by(ACES) %>%
  mutate(ACES_delay_pat = median(as.numeric(delay_pat)),
         ACES_delay_health = median(as.numeric(delay_health)),
         ACES_delay_global = median(as.numeric(delay_global)))
