library(tidyverse)
library(dplyr)
library(readr)
library(sp)
library(rgdal)
library(ggplot2)



#importar dados
filename <- file.choose()
dadosTB <- readRDS(filename)

filename2 <- file.choose()
POP2013 <- read.csv(filename2, sep = ";")



#filtrar as variáveis a utilizar

TB1 <- dadosTB %>%
  select(ID, sex, age_diag,
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



view(TB1)
 names(dadosTB)
 
 

 
#criar variável de incidência               
 
table(as.character(dadosTB$origin_country))

 
 
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
 
 

#MERGE população
 

POP2013 <- read_delim("D:/OneDrive - Universidade de Coimbra/TUBERCULOSE/protocolo ENSP TB/Base de dados/POP2013.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)
View(POP2013) 

gsub(" ", "", POP2013$population)

glimpse(POP2013)
class(POP2013$Total)


POP2013$population <- as.numeric(gsub(" ", "", POP2013$population))

TB2 <- merge(TB1, POP2013, by = "county", all.x = TRUE, all.y = TRUE)
TB2 <- TB2[!is.na(TB2$ID),]

table(TB2$population, exclude = NA)

table(TB2$ACES, TB2$population_ACES)

view(TB2)

TB2[is.na(TB2$ID),]
TB1[TB1$county == "GOIS",]

#variável incidência


TB2 <- TB2 %>%
  group_by(ACES) %>%
  mutate(ACES_count = n())

table(TB2$ACES, TB2$ACES_count)

TB2$incidence <- round(TB2$ACES_count / TB2$population_ACES * 100000 / 10, digits=2)



#filtrar entradas com dados omissos e com inconsistências


TB3 <- TB2 %>% 
  drop_na(symptoms_date, appoint_date, diagnosis_date)

view(TB3)
unique(TB3$problem_pat)
unique(TB3$problem_health)

TB_pat <- TB3[TB3$problem_pat %in% c("No inconsistencies",
                                     "Inconsistencies with symptoms onset and microscopy and/or culture"), ]


TB_health <- TB3[TB3$problem_health %in% c("No inconsistencies",
                                           "Inconsistencies with first appointment and microscopy and/or culture"), ]


TB_total <- TB3[TB3$problem_pat %in% c("No inconsistencies",
                                       "Inconsistencies with symptoms onset and microscopy and/or culture")
                &
                  TB3$problem_health %in% c("No inconsistencies",
                                            "Inconsistencies with first appointment and microscopy and/or culture"), ]





#criar variável na base de dados dadosTB que identifica se está na base de dados dos incluidos TB_total


TB3$included <- ifelse(TB3$ID %in% TB_total$ID, 1, 2)



#ANÁLISE DESCRITIVA


table_incidence <- TB3 %>% 
  group_by(ACES) %>% 
  summarise(Incidencia = mean(incidence)) %>% 
  arrange(Incidencia) %>% 
  view()

table_atrasos <- TB3 %>% 
  group_by(ACES) %>% 
  summarise(Incidencia = me(incidence)) %>% 
  arrange(Incidencia) %>% 
  view()


#visualização mapa incidência TB / ACES 





shp = readOGR(dsn = ".", layer = "gadm36_PRT_3")

shp$ACES <- mutate(shp$ACES = [shp$NAME_2 %in% c("Cantanhede")])

class(shp$NAME_2)
shp$NAME_2 <- as.factor(shp$NAME_2)

shp <- shp %>% 
  mutate(ACES = if_else(shp$NAME_2 %in% "Cantanhede", "Baixo Mondego", "xxx"))

plot(shp, col = "green", bg = "lightblue", lwd = 1)

df <- fortify(shp)

ggplot()+
  geom_polygon(data = df, aes(x = long, y = lat, group = NAME_1))

df1 <- merge(df, TB3, by = "county", all.x = TRUE, all.y = TRUE)


#included vs excluded

library(compareGroups)

gsub(" ", "", POP2013$population)

table(TB3$alcohol, exclude = NULL)

levels(TB3$alcohol)[levels(TB3$alcohol)=='Unknown'] <- NA
levels(TB3$drugs)[levels(TB3$drugs)=='Unknown'] <- NA
levels(TB3$inmate)[levels(TB3$inmate)=='Unknown'] <- NA
levels(TB3$homeless)[levels(TB3$homeless)=='Unknown'] <- NA
levels(TB3$commu_resid)[levels(TB3$commu_resid)=='Unknown'] <- NA
levels(TB3$HIV)[levels(TB3$HIV)=='Unknown'] <- NA


TB4 <- TB3 %>% 
  drop_na(alcohol, drugs, inmate, homeless, commu_resid, HIV)


comp <- compareGroups(included ~ sex + age_diag +
                      country_inc + county + ACES +
                      unemployment + health_job + alcohol +
                      drugs + inmate + homeless +
                      commu_resid + HIV + dialysis +
                      cancer + inflam + respi + diabetes +
                      delay_pat + delay_health + delay_global +
                      centro_saude + rurality + benef +
                      school + house + doc_rate, data = TB4)
 
tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "fam_table7.csv", which.table="descr", sep=";",
           nmax = TRUE)

tabletest

?compareGroups
summarise(TB3)
