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
library(epiDisplay)
library(summarytools)
library(gtsummary)


#tabela ATRASOS POR ACES

table_delay <- TB1[TB1$included == 1,] %>% 
  group_by(ACES) %>% 
  summarise(delaypat_mean = mean(delay_pat),
            delaypat_sd = sd(delay_pat),
            delaypat_min = min(delay_pat),
            delaypat_max = max(delay_pat),
            delaypat_n = n(),
            delaypat_median = median(as.numeric(delay_pat)),
            delaypat_q1 = quantile(delay_pat, 0.25),
            delaypat_q3 = quantile( delay_pat, 0.75)) %>% 
  view()
write.csv2(table_delay, "table_delay_pat.csv", row.names=FALSE)

table_delay <- TB1[TB1$included == 1,] %>% 
  group_by(ACES) %>% 
  summarise(delayhealth_mean = mean(delay_health),
            delayhealth_sd = sd(delay_health),
            delayhealth_min = min(delay_health),
            delayhealth_max = max(delay_health),
            delayhealth_n = n(),
            delayhealth_median = median(as.numeric(delay_health)),
            delayhealth_q1 = quantile(delay_health, 0.25),
            delayhealth_q3 = quantile( delay_health, 0.75)) %>% 
  view()
write.csv2(table_delay, "table_delay_health.csv", row.names=FALSE)

table_delay <- TB1[TB1$included == 1,] %>% 
  group_by(ACES) %>% 
  summarise(delayglobal_mean = mean(delay_global),
            delayglobal_sd = sd(delay_global),
            delayglobal_min = min(delay_global),
            delayglobal_max = max(delay_global),
            delayglobal_n = n(),
            delayglobal_median = median(as.numeric(delay_global)),
            delayglobal_q1 = quantile(delay_global, 0.25),
            delayglobal_q3 = quantile( delay_global, 0.75)) %>% 
  view()
write.csv2(table_delay, "table_delay_global.csv", row.names=FALSE)

tbl_summary_2 <- TB1[TB1$included == 1,] %>% 
  dplyr::select(delay_pat, delay_health, ACES) %>%
  tbl_summary(
    by = ACES,
    label = c(delay_pat ~ "Atraso do Utente",
              delay_health ~ "Atraso dos Serviços de Saúde"),
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75}) / {min}-{max}"),
        missing_text = "Desconhecido") %>%
  modify_header(label = "**Variável**") %>%
  bold_labels()

tbl_summary_2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(tbl_summary_1, path = "gt_summary_desc_atrasos.docx")

tbl_summary_2

#análise descritiva variáveis independentes
TB_desc <- TB1[TB1$included == 1,] %>% 
  dplyr::select(sex, age_diag, country_inc, county, ACES,
         unemployment, health_job, alcohol,
         drugs, inmate, homeless,
         commu_resid, HIV, dialysis,
         cancer, inflam, respi, diabetes,
         delay_pat, delay_health, delay_global,
         centro_saude, rurality, benef,
         school, house, doc_rate,
         centro_saude_rate, population)


tab1(TB_desc$sex, cum.percent = FALSE)
tab1(TB_desc$country_inc, cum.percent = FALSE)
tab1(TB_desc$unemployment, cum.percent = FALSE)
tab1(TB_desc$health_job, cum.percent = FALSE)
tab1(TB_desc$alcohol, cum.percent = FALSE)
tab1(TB_desc$drugs, cum.percent = FALSE)
tab1(TB_desc$inmate, cum.percent = FALSE)
tab1(TB_desc$homeless, cum.percent = FALSE)
tab1(TB_desc$commu_resid, cum.percent = FALSE)
tab1(TB_desc$HIV, cum.percent = FALSE)
tab1(TB_desc$dialysis, cum.percent = FALSE)
tab1(TB_desc$cancer, cum.percent = FALSE)
tab1(TB_desc$inflam, cum.percent = FALSE)
tab1(TB_desc$respi, cum.percent = FALSE)
tab1(TB_desc$diabetes, cum.percent = FALSE)
tab1(TB_desc$rurality, cum.percent = FALSE)

#tab1(TB_desc$house, cum.percent = FALSE)
#tab1(TB_desc$doc_rate, cum.percent = FALSE)
#tab1(TB_desc$benef, cum.percent = FALSE)
#tab1(TB_desc$centro_saude_rate, cum.percent = FALSE)
#tab1(TB_desc$school, cum.percent = FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(house),
            sd = sd(house),
            min = min(house),
            max = max(house),
            n = n(),
            median = median(as.numeric(house)),
            q1 = quantile(house, 0.25),
            q3 = quantile(house, 0.75))
write.csv2(table_IND, "table_IND_house.csv", row.names=FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(doc_rate),
            sd = sd(doc_rate),
            min = min(doc_rate),
            max = max(doc_rate),
            n = n(),
            median = median(as.numeric(doc_rate)),
            q1 = quantile(doc_rate, 0.25),
            q3 = quantile(doc_rate, 0.75))
write.csv2(table_IND, "table_IND_doc_rate.csv", row.names=FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(benef),
            sd = sd(benef),
            min = min(benef),
            max = max(benef),
            n = n(),
            median = median(as.numeric(benef)),
            q1 = quantile(benef, 0.25),
            q3 = quantile(benef, 0.75))
write.csv2(table_IND, "table_IND_benef.csv", row.names=FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(centro_saude_rate),
            sd = sd(centro_saude_rate),
            min = min(centro_saude_rate),
            max = max(centro_saude_rate),
            n = n(),
            median = median(as.numeric(centro_saude_rate)),
            q1 = quantile(centro_saude_rate, 0.25),
            q3 = quantile(centro_saude_rate, 0.75))
write.csv2(table_IND, "table_IND_centro_saude_rate.csv", row.names=FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(school),
            sd = sd(school),
            min = min(school),
            max = max(school),
            n = n(),
            median = median(as.numeric(school)),
            q1 = quantile(school, 0.25),
            q3 = quantile(school, 0.75))
write.csv2(table_IND, "table_IND_school.csv", row.names=FALSE)

table_IND <- TB_desc %>%
  ungroup() %>%
  summarise(mean = mean(age_diag),
            sd = sd(age_diag),
            min = min(age_diag),
            max = max(age_diag),
            n = n(),
            median = median(as.numeric(age_diag)),
            q1 = quantile(age_diag, 0.25),
            q3 = quantile(age_diag, 0.75))
write.csv2(table_IND, "table_IND_age_diag.csv", row.names=FALSE)

#gtsummary test

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

TB1 <- TB1 %>% 
  mutate(age_group = dplyr::case_when
         (age_diag <= 18 ~ "< 18",
           age_diag > 18 & age_diag <= 64 ~ "18-64",
           age_diag > 64 ~ "> 64"),
         age_group = factor(
           age_group,
           level = c("< 18", "18-64", "> 64")
         )
  )

tbl_summary_1 <- TB1[TB1$included == 1,] %>% 
  dplyr::select(sex, country_inc, ACES,
                unemployment, health_job, alcohol,
                drugs, inmate, homeless,
                commu_resid, HIV, diabetes, dialysis,
                cancer, inflam, respi, rurality,
                age_diag, centro_saude_rate, benef,
                school, house, doc_rate,
                centro_saude_rate) %>%
  tbl_summary(
    label = c(sex ~ "Sexo",
              HIV ~ "Infeção por VIH",
              inflam ~ "Doença Inflamatória",
              respi ~ "Doença Respiratória",
              diabetes ~ "Diabetes",
              dialysis ~ "Diálise",
              cancer ~ "Cancro",
              country_inc ~ "Incidência no País de Origem",
              commu_resid ~ "Residência Comunitária",
              inmate ~ "Presidiário",
              unemployment ~ "Desempregado",
              health_job ~ "Profissional de Saúde",
              homeless ~ "Sem-abrigo",
              rurality ~ "Ruralidade",
              alcohol ~ "Consumo de Álcool",
              drugs ~ "Consumo de Drogas",
              age_diag ~ "Idade ao Diagnóstico",
              centro_saude_rate ~ "Centros de Saúde", 
              benef ~ "Beneficiários de Apoio Social",
              school ~ "Escolaridade",
              house ~ "Sobrelotação",
              doc_rate ~ "Médicos"),
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75}) / {min}-{max}",
      all_categorical() ~ "{n} ({p}%)"),
    type = all_dichotomous() ~ "categorical",
    missing_text = "Desconhecido"
  )  %>%
  modify_header(label = "**Variável**") %>%
  bold_labels()

tbl_summary_1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(tbl_summary_1, path = "gt_summary_desc.docx")
