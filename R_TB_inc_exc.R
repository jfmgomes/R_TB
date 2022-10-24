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


#TABELA COMPARAÇÃO EXCLUIDOS VARIÁVEIS SEM NAs
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)

comp <- compareGroups(included ~ country_inc + sex +
                        age_diag + ACES +
                        unemployment + health_job + dialysis +
                        cancer + respi + diabetes +
                        centro_saude_rate + rurality + benef +
                        school + house + doc_rate, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_semNA2.csv", which.table="descr", sep=";",
           nmax = TRUE)

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

gt_incexc_semNA <- TB1 %>% 
  dplyr::select(included, country_inc, sex,
                  age_diag, ACES,
                  unemployment, health_job, dialysis,
                  cancer, respi, diabetes,
                rurality, centro_saude_rate, benef,
                  school, house, doc_rate) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      respi ~ "Doença Respiratória",
      diabetes ~ "Diabetes",
      dialysis ~ "Diálise",
      cancer ~ "Cancro",
      country_inc ~ "Incidência no País de Origem",
      unemployment ~ "Desempregado",
      health_job ~ "Profissional de Saúde",
      rurality ~ "Ruralidade",
      age_diag ~ "Idade ao Diagnóstico",
      centro_saude_rate ~ "Centros de Saúde",
      benef ~ "Beneficiários de Apoio Social",
      school ~ "Escolaridade",
      house ~ "Sobrelotação",
      doc_rate ~ "Médicos"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
    ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**") %>% 
  modify_header(
    list(
      stat_1 ~ "**Incluídos**, N={n}",
      stat_2 ~ "**Excluídos**, N={n}"
    ))


gt_incexc_semNA %>%
  as_flex_table() %>%
  flextable::save_as_docx(tbl_summary_1, path = "gt_incexc_semNA.docx")

#TABELA COMPARAÇÃO EXCLUIDOS ALCOHOL
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$alcohol), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + alcohol, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_ALCOHOL.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_alcohol <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                alcohol) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      alcohol ~ "Álcool"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
      ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")

gt_incexc_alcohol

#TABELA COMPARAÇÃO EXCLUIDOS DRUGS
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$drugs), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + drugs, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_DRUGS.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_drugs <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                drugs) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      drugs ~ "Drogas"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")

gt_incexc_drogas

#TABELA COMPARAÇÃO EXCLUIDOS INMATE
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$inmate), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + inmate, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_INMATE.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_inmate <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                inmate) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      inmate ~ "Presidiário"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")

#TABELA COMPARAÇÃO EXCLUIDOS HOMELESS
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$homeless), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + homeless, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_HOMELESS.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_homeless <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                homeless) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      homeless ~ "Sem-abrigo"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")

#TABELA COMPARAÇÃO EXCLUIDOS COMMU_RESID
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$commu_resid), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + commu_resid, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_COMMU_RESID.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_commu <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                commu_resid) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      commu_resid ~ "Residência Comunitária"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")
#TABELA COMPARAÇÃO EXCLUIDOS HIV
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$HIV), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + HIV, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_HIV.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_hiv <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                HIV) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      HIV ~ "Infeção por VIH"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")

#TABELA COMPARAÇÃO EXCLUIDOS INFLAM
TB1$included <- ifelse(TB1$ID %in% TB_total$ID & TB1$ID %in% TB_semNA$ID, 1, 2)
TB1$included <- ifelse(is.na(TB1$inflam), 2, TB1$included)

comp <- compareGroups(included ~ sex + age_diag +
                        ACES + inflam, data = TB1)

tabletest <- createTable(comp, show.all = T, show.p.overall = T)

export2csv(tabletest, "table_INFLAM.csv", which.table="descr", sep=";",
           nmax = TRUE)

gt_incexc_inflam <- TB1 %>% 
  dplyr::select(included, sex, age_diag, ACES,
                inflam) %>% 
  tbl_summary(
    by = included,
    label = list(
      sex ~ "Sexo",
      age_diag ~ "Idade ao Diagnóstico",
      inflam ~ "Doença Inflamatória"),
    missing_text = "Desconhecido",
    type = all_dichotomous() ~ "categorical"
  ) %>%
  add_p() %>%
  bold_p() %>%
  bold_labels() %>% 
  modify_caption("**Comparação Incluídos vs Excluídos**") %>%
  modify_header(label ~ "**Variável**")


gt_incexc_NAs <-
  tbl_stack(
    tbls = list(gt_incexc_alcohol, gt_incexc_drugs, gt_incexc_inmate,
                gt_incexc_homeless, gt_incexc_commu,
                gt_incexc_hiv, gt_incexc_inflam)
  ) %>% 
  modify_header(
    list(
      stat_1 ~ "**Incluídos**, N={n}",
      stat_2 ~ "**Excluídos**, N={n}"
    ))

gt_incexc_NAs %>%
  as_flex_table() %>%
  flextable::save_as_docx(tbl_summary_1, path = "gt_incexc_NAs.docx")
