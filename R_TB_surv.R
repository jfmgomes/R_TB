library(survival)
library(survminer)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(broom)
library(pixiedust)
library(gtsummary)
library(gt)
library(ggplot2)

TB_surv <- TB1[TB1$included == 1,]


TB_surv <- TB_surv %>% 
  mutate(age_group = dplyr::case_when
         (age_diag <= 18 ~ "< 18",
           age_diag > 18 & age_diag <= 64 ~ "18-64",
           age_diag > 64 ~ "> 64"),
         age_group = factor(
           age_group,
           level = c("< 18", "18-64", "> 64")
         )
  )

summary(TB_surv$centro_saude_rate)

TB_surv <- TB_surv %>% 
  mutate(centro_saude_group = dplyr::case_when
         (centro_saude_rate <= 2 ~ "< 2",
           centro_saude_rate > 2 & centro_saude_rate <= 3 ~ "2-3",
           centro_saude_rate > 3 & centro_saude_rate <= 5 ~ "3-5",
           centro_saude_rate > 5 & centro_saude_rate <= 10 ~ "5-10",
           centro_saude_rate > 10 ~ "> 10"),
         centro_saude_group = factor(
           centro_saude_group,
           level = c("< 2", "2-3", "3-5", "5-10", "> 10")
         )
  )

summary(TB_surv$benef)
TB_surv <- TB_surv %>% 
  mutate(benef_group = dplyr::case_when
         (benef <= 25 ~ "< 25",
           benef > 25 & benef <= 31 ~ "25-31",
           benef > 31 & benef <= 37 ~ "31-37",
           benef > 37 ~ "> 37"),
         benef_group = factor(
           benef_group,
           level = c("< 25", "25-31", "31-37","> 37")
         )
  )

summary(TB_surv$school)
TB_surv <- TB_surv %>% 
  mutate(school_group = dplyr::case_when
         (school <= 40 ~ "< 40",
           school > 40 & school <= 47 ~ "40-47",
           school > 47 & school <= 52 ~ "47-52",
           school > 52 ~ "> 52"),
         school_group = factor(
           school_group,
           level = c("< 40", "40-47", "47-52","> 52")
         )
  )

summary(TB_surv$house)
TB_surv <- TB_surv %>% 
  mutate(house_group = dplyr::case_when
         (house <= 6 ~ "< 6",
           house > 6 & house <= 7 ~ "6-7",
           house > 7 & house <= 9 ~ "7-9",
           house > 9 ~ "> 9"),
         house_group = factor(
           house_group,
           level = c("< 6", "6-7", "7-9","> 9")
         )
  )

summary(TB_surv$doc_rate)
TB_surv <- TB_surv %>% 
  mutate(doc_rate_group = dplyr::case_when
         (doc_rate <= 6 ~ "< 6",
           doc_rate > 6 & doc_rate <= 7 ~ "6-7",
           doc_rate > 7 & doc_rate <= 8 ~ "7-8",
           doc_rate > 8 ~ "> 8"),
         doc_rate_group = factor(
           doc_rate_group,
           level = c("< 6", "6-7", "7-8","> 8")
         )
  )


#### Kaplan-Meier analysis ####
km_TB1 <- survfit(Surv(delay_health) ~ 1,
                  data=TB_surv,
                  type="kaplan-meier")
ggsurvplot(km_TB1, data=TB_surv, xlim=c(0,365), risk.table = TRUE, conf.int = TRUE,
           ggtheme = theme_minimal())

km_TB2 <- survfit(Surv(delay_pat) ~ 1,
                  data=TB_surv,
                  type="kaplan-meier")
ggsurvplot(km_TB2, data=TB_surv, xlim=c(0,365), risk.table = TRUE, conf.int = TRUE,
           ggtheme = theme_minimal())

km_TB3 <- survfit(Surv(delay_global) ~ 1,
                  data=TB_surv,
                  type="kaplan-meier")
ggsurvplot(km_TB3, data=TB_surv, xlim=c(0,365), risk.table = TRUE, conf.int = TRUE,
           ggtheme = theme_minimal())

fit <- list("Serviços de Saúde" = km_TB1, "Utente" = km_TB2, "Total" = km_TB3)
ggsurvplot_combine(fit, TB_surv, xlim=c(0,365), legend.title = "Legenda",
                   xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
                   ggtheme = theme_minimal())
#### end ####

#### logrank ATRASO DO UTENTE E DOS SERVIÇOS DE SAÚDE por variável independente ####

#logrank sex
km_pat_sex <- survfit(Surv(delay_pat) ~ sex,
                  data=TB_surv,
                  type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_sex, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_sex <- survfit(Surv(delay_health) ~ sex,
                         data=TB_surv,
                         type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_sex, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                    ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_sex.png", res, width = 10, height = 10)


#logrank age_diag
km_pat_age_group <- survfit(Surv(delay_pat) ~ age_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_age_group, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_age_group <- survfit(Surv(delay_health) ~ age_group,
                               data=TB_surv,
                               type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_age_group, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_age.png", res, width = 10, height = 10)


#logrank country_inc
km_pat_country_inc <- survfit(Surv(delay_pat) ~ country_inc,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_country_inc, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_country_inc <- survfit(Surv(delay_health) ~ country_inc,
                                 data=TB_surv,
                                 type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_country_inc, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_country.png", res, width = 10, height = 10)


#logrank ACES
km_pat_ACES <- survfit(Surv(delay_pat) ~ ACES,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_ACES, data=TB_surv, risk.table = FALSE, conf.int = FALSE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_ACES <- survfit(Surv(delay_health) ~ ACES,
                          data=TB_surv,
                          type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_ACES, data=TB_surv, risk.table = FALSE, conf.int = FALSE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_ACES.png", res, width = 10, height = 10)


#logrank unemployment
km_pat_unemployment <- survfit(Surv(delay_pat) ~ unemployment,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_unemployment, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_unemployment <- survfit(Surv(delay_health) ~ unemployment,
                                  data=TB_surv,
                                  type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_unemployment, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_unemployment.png", res, width = 10, height = 10)


#logrank health_job
km_pat_health_job <- survfit(Surv(delay_pat) ~ health_job,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_health_job, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_health_job <- survfit(Surv(delay_health) ~ health_job,
                                data=TB_surv,
                                type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_health_job, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_health_job.png", res, width = 10, height = 10)


#logrank alcohol
km_pat_alcohol <- survfit(Surv(delay_pat) ~ alcohol,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_alcohol, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_alcohol <- survfit(Surv(delay_health) ~ alcohol,
                             data=TB_surv,
                             type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_alcohol, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_alcohol.png", res, width = 10, height = 10)


#logrank drugs
km_pat_drugs <- survfit(Surv(delay_pat) ~ drugs,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_drugs, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_drugs <- survfit(Surv(delay_health) ~ drugs,
                           data=TB_surv,
                           type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_drugs, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_drugs.png", res, width = 10, height = 10)


#logrank inmate
km_pat_inmate <- survfit(Surv(delay_pat) ~ inmate,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_inmate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_inmate <- survfit(Surv(delay_health) ~ inmate,
                            data=TB_surv,
                            type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_inmate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_inmate.png", res, width = 10, height = 10)


#logrank homeless
km_pat_homeless <- survfit(Surv(delay_pat) ~ homeless,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_homeless, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_homeless <- survfit(Surv(delay_health) ~ homeless,
                              data=TB_surv,
                              type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_homeless, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_homeless.png", res, width = 10, height = 10)


#logrank commu_resid
km_pat_commu_resid <- survfit(Surv(delay_pat) ~ commu_resid,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_commu_resid, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_commu_resid <- survfit(Surv(delay_health) ~ commu_resid,
                                 data=TB_surv,
                                 type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_commu_resid, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_commu_resid.png", res, width = 10, height = 10)


#logrank HIV
km_pat_HIV <- survfit(Surv(delay_pat) ~ HIV,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_HIV, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_HIV <- survfit(Surv(delay_health) ~ HIV,
                         data=TB_surv,
                         type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_HIV, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_HIV.png", res, width = 10, height = 10)

#logrank dialysis
km_pat_dialysis <- survfit(Surv(delay_pat) ~ dialysis,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_dialysis, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_dialysis <- survfit(Surv(delay_health) ~ dialysis,
                              data=TB_surv,
                              type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_dialysis, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_dialysis.png", res, width = 10, height = 10)


#logrank cancer
km_pat_cancer <- survfit(Surv(delay_pat) ~ cancer,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_cancer, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_cancer <- survfit(Surv(delay_health) ~ cancer,
                            data=TB_surv,
                            type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_cancer, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_cancer.png", res, width = 10, height = 10)


#logrank inflam
km_pat_inflam <- survfit(Surv(delay_pat) ~ inflam,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_inflam, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_inflam <- survfit(Surv(delay_health) ~ inflam,
                            data=TB_surv,
                            type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_inflam, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_inflam.png", res, width = 10, height = 10)


#logrank respi
km_pat_respi <- survfit(Surv(delay_pat) ~ respi,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_respi, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_respi <- survfit(Surv(delay_health) ~ respi,
                           data=TB_surv,
                           type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_respi, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_respi.png", res, width = 10, height = 10)


#logrank diabetes
km_pat_diabetes <- survfit(Surv(delay_pat) ~ diabetes,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_diabetes, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_diabetes <- survfit(Surv(delay_health) ~ diabetes,
                              data=TB_surv,
                              type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_diabetes, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_diabetes.png", res, width = 10, height = 10)


#logrank centro_saude
km_pat_centro_saude_rate <- survfit(Surv(delay_pat) ~ centro_saude_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_centro_saude_rate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_centro_saude_rate <- survfit(Surv(delay_health) ~ centro_saude_group,
                                       data=TB_surv,
                                       type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_centro_saude_rate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_centro_saude.png", res, width = 12, height = 10)


#logrank rurality
km_pat_rurality <- survfit(Surv(delay_pat) ~ rurality,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_rurality, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_rurality <- survfit(Surv(delay_health) ~ rurality,
                              data=TB_surv,
                              type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_rurality, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_rurality.png", res, width = 10, height = 10)


#logrank benef
km_pat_benef <- survfit(Surv(delay_pat) ~ benef_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_benef, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_benef <- survfit(Surv(delay_health) ~ benef_group,
                           data=TB_surv,
                           type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_benef, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_benef.png", res, width = 10, height = 10)


#logrank school
km_pat_school <- survfit(Surv(delay_pat) ~ school_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_school, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_school <- survfit(Surv(delay_health) ~ school_group,
                            data=TB_surv,
                            type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_school, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_school.png", res, width = 10, height = 10)


#logrank house
km_pat_house <- survfit(Surv(delay_pat) ~ house_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_house, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_house <- survfit(Surv(delay_health) ~ house_group,
                           data=TB_surv,
                           type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_house, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_house.png", res, width = 10, height = 10)


#logrank doc_rate
km_pat_doc_rate <- survfit(Surv(delay_pat) ~ doc_rate_group,
                      data=TB_surv,
                      type="kaplan-meier")

splots <- list()
splots[[1]] <- ggsurvplot(km_pat_doc_rate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
           pval = TRUE, pval.method = FALSE, title = "Atraso do Utente",
           pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
           xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
           ggtheme = theme_minimal())

km_health_doc_rate <- survfit(Surv(delay_health) ~ doc_rate_group,
                              data=TB_surv,
                              type="kaplan-meier")

splots[[2]] <- ggsurvplot(km_health_doc_rate, data=TB_surv, risk.table = FALSE, conf.int = TRUE,
                    pval = TRUE, pval.method = FALSE, title = "Atraso dos Serviços de Saúde",
                    pval.coord = c(250,0.75), xlim=c(0,365), legend.title = "Legenda",
                    xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
                    ggtheme = theme_minimal())

res <- arrange_ggsurvplots(splots, print = FALSE,
                           ncol = 1, nrow = 2, risk.table.height = 0.4)
ggsave("km_doc_rate.png", res, width = 10, height = 10)

#### end ####

#### reorganização dos níveis dos grupos####
TB_surv <- TB_surv %>% 
  mutate(age_group = factor(
           age_group,
           level = c("18-64", "< 18", "> 64")
         )
  )

TB_surv <- TB_surv %>% 
  mutate(centro_saude_group = factor(
           centro_saude_group,
           level = c("> 10", "< 2", "2-3", "3-5", "5-10")
         )
  )

TB_surv <- TB_surv %>% 
  mutate(benef_group = factor(
           benef_group,
           level = c("< 25", "25-31", "31-37","> 37")
         )
  )

TB_surv <- TB_surv %>% 
  mutate(school_group = factor(
           school_group,
           level = c("> 52", "< 40", "40-47", "47-52")
         )
  )

TB_surv <- TB_surv %>% 
  mutate(house_group = factor(
           house_group,
           level = c("< 6", "6-7", "7-9","> 9")
         )
  )

TB_surv <- TB_surv %>% 
  mutate(doc_rate_group = factor(
           doc_rate_group,
           level = c("> 8", "< 6", "6-7", "7-8")
         )
  )
#### end ####

#### COX UTENTE ####
#variáveis clínicas ajustado
cox_pat_clin_adj <- coxph(Surv(delay_pat) ~ sex + age_group +
                   HIV + inflam + respi +
                   diabetes + dialysis + cancer,
                  data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = c(sex ~ "Sexo", 
                           age_group ~ "Grupo Etário",
                           HIV ~ "Infeção por VIH",
                           inflam ~ "Doença Inflamatória",
                           respi ~ "Doença Respiratória",
                           diabetes ~ "Diabetes",
                           dialysis ~ "Diálise",
                           cancer ~ "Cancro")
  )

#var clinicas nao ajustado
cox_pat_sex <- coxph(Surv(delay_pat) ~ sex,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = sex ~ "Sexo")

cox_pat_age <- coxph(Surv(delay_pat) ~ age_group,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = age_group ~ "Grupo Etário")

cox_pat_hiv <- coxph(Surv(delay_pat) ~ HIV,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = HIV ~ "Infeção por VIH")

cox_pat_inf <- coxph(Surv(delay_pat) ~ inflam,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = inflam ~ "Doença Inflamatória")

cox_pat_resp <- coxph(Surv(delay_pat) ~ respi,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = respi ~ "Doença Respiratória")

cox_pat_diab <- coxph(Surv(delay_pat) ~ diabetes,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = diabetes ~ "Diabetes")

cox_pat_dial <- coxph(Surv(delay_pat) ~ dialysis,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = dialysis ~ "Diálise")

cox_pat_canc <- coxph(Surv(delay_pat) ~ cancer,
                 data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = cancer ~ "Cancro")

cox_pat_clin_unadj <-
  tbl_stack(
    tbls = list(cox_pat_sex, cox_pat_age, cox_pat_hiv, cox_pat_inf,
            cox_pat_resp, cox_pat_diab, cox_pat_dial,
            cox_pat_canc)
  )
cox_pat_clin_unadj

#variáveis sociais ajustado
cox_pat_soc_adj <- coxph(Surv(delay_pat) ~ sex + age_group +
                   country_inc + commu_resid + inmate +
                   unemployment + health_job + homeless +
                   rurality + alcohol + drugs,
                 data = TB_surv) %>%
  tbl_regression(exponentiate = TRUE,
                 label = c(sex ~ "Sexo", 
                           age_group ~ "Grupo Etário",
                           country_inc ~ "Incidência no País de Origem",
                           commu_resid ~ "Residência Comunitária",
                           inmate ~ "Presidiário",
                           unemployment ~ "Desempregado",
                           health_job ~ "Profissional de Saúde",
                           homeless ~ "Sem-abrigo",
                           rurality ~ "Ruralidade",
                           alcohol ~ "Consumo de Álcool",
                           drugs ~ "Consumo de Drogas")
  )

#var sociais nao ajustado
cox_pat_sex <- coxph(Surv(delay_pat) ~ sex,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = sex ~ "Sexo")

cox_pat_age <- coxph(Surv(delay_pat) ~ age_group,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = age_group ~ "Grupo Etário")

cox_pat_coun <- coxph(Surv(delay_pat) ~ country_inc,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = country_inc ~ "Incidência no País de Origem")

cox_pat_com <- coxph(Surv(delay_pat) ~ commu_resid,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = commu_resid ~ "Residência Comunitária")

cox_pat_inm <- coxph(Surv(delay_pat) ~ inmate,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = inmate ~ "Presidiário")

cox_pat_unem <- coxph(Surv(delay_pat) ~ unemployment,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = unemployment ~ "Desempregado")

cox_pat_hjob <- coxph(Surv(delay_pat) ~ health_job,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = health_job ~ "Profissional de Saúde")

cox_pat_hom <- coxph(Surv(delay_pat) ~ homeless,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = homeless ~ "Sem-abrigo")

cox_pat_rur <- coxph(Surv(delay_pat) ~ rurality,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = rurality ~ "Ruralidade")

cox_pat_alc <- coxph(Surv(delay_pat) ~ alcohol,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = alcohol ~ "Consumo de Álcool")

cox_pat_drug <- coxph(Surv(delay_pat) ~ drugs,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = drugs ~ "Consumo de Drogas")

cox_pat_soc_unadj <-
  tbl_stack(
    tbls = list(cox_pat_sex, cox_pat_age, cox_pat_coun,
                cox_pat_com, cox_pat_inm, cox_pat_unem,
                cox_pat_hjob, cox_pat_hom, cox_pat_rur,
                cox_pat_alc, cox_pat_drug)
  )
cox_pat_soc_unadj

#variáveis contextuais ajustado
cox_pat_cont_adj <- coxph(Surv(delay_pat) ~ cluster(county) +
                   centro_saude_group + benef_group + school_group +
                   house_group + doc_rate_group,
                 data = TB_surv) %>%
  tbl_regression(exponentiate = TRUE,
                 label = c(centro_saude_group ~ "Centros de Saúde", 
                           benef_group ~ "Beneficiários de Apoio Social",
                           school_group ~ "Escolaridade",
                           house_group ~ "Sobrelotação",
                           doc_rate_group ~ "Médicos")
  )

#var contextuais nao ajustado
cox_pat_cs <- coxph(Surv(delay_pat) ~ cluster(county) + centro_saude_group,
                       data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = centro_saude_group ~ "Centros de Saúde")

cox_pat_ben <- coxph(Surv(delay_pat) ~ cluster(county) + benef_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = benef_group ~ "Beneficiários de Apoio Social")

cox_pat_sch <- coxph(Surv(delay_pat) ~ cluster(county) + school_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = school_group ~ "Escolaridade")

cox_pat_hou <- coxph(Surv(delay_pat) ~ cluster(county) + house_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = house_group ~ "Sobrelotação")

cox_pat_doc <- coxph(Surv(delay_pat) ~ cluster(county) + doc_rate_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = doc_rate_group ~ "Médicos")

cox_pat_cont_unadj <-
  tbl_stack(
    tbls = list(cox_pat_cs, cox_pat_ben, cox_pat_sch,
                cox_pat_hou, cox_pat_doc)
  )
cox_pat_cont_unadj

#### end ####

#### COX SERVIÇOS SAÚDE ####
#variáveis clínicas
cox_health_clin_adj <- coxph(Surv(delay_health) ~ sex + age_group +
                       HIV + inflam + respi +
                       diabetes + dialysis + cancer,
                     data = TB_surv) %>%
  tbl_regression(exponentiate = TRUE,
                 label = c(sex ~ "Sexo", 
                           age_group ~ "Grupo Etário",
                           HIV ~ "Infeção por VIH",
                           inflam ~ "Doença Inflamatória",
                           respi ~ "Doença Respiratória",
                           diabetes ~ "Diabetes",
                           dialysis ~ "Diálise",
                           cancer ~ "Cancro")
  )

#nao ajustado
cox_health_sex <- coxph(Surv(delay_health) ~ sex,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = sex ~ "Sexo")

cox_health_age <- coxph(Surv(delay_health) ~ age_group,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = age_group ~ "Grupo Etário")

cox_health_hiv <- coxph(Surv(delay_health) ~ HIV,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = HIV ~ "Infeção por VIH")

cox_health_inf <- coxph(Surv(delay_health) ~ inflam,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = inflam ~ "Doença Inflamatória")

cox_health_resp <- coxph(Surv(delay_health) ~ respi,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = respi ~ "Doença Respiratória")

cox_health_diab <- coxph(Surv(delay_health) ~ diabetes,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = diabetes ~ "Diabetes")

cox_health_dial <- coxph(Surv(delay_health) ~ dialysis,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = dialysis ~ "Diálise")

cox_health_canc <- coxph(Surv(delay_health) ~ cancer,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = cancer ~ "Cancro")

cox_health_clin_unadj <-
  tbl_stack(
    tbls = list(cox_health_sex, cox_health_age, cox_health_hiv, cox_health_inf,
                cox_health_resp, cox_health_diab, cox_health_dial,
                cox_health_canc)
  )
cox_health_clin_unadj

#variáveis sociais ajustado
cox_health_soc_adj <- coxph(Surv(delay_health) ~ sex + age_group +
                   country_inc + commu_resid + inmate +
                   unemployment + health_job + homeless +
                     rurality + alcohol + drugs,
                 data = TB_surv) %>%
  tbl_regression(exponentiate = TRUE,
                 label = c(sex ~ "Sexo", 
                           age_group ~ "Grupo Etário",
                           country_inc ~ "Incidência no País de Origem",
                           commu_resid ~ "Residência Comunitária",
                           inmate ~ "Presidiário",
                           unemployment ~ "Desempregado",
                           health_job ~ "Profissional de Saúde",
                           homeless ~ "Sem-abrigo",
                           rurality ~ "Ruralidade",
                           alcohol ~ "Consumo de Álcool",
                           drugs ~ "Consumo de Drogas")
  )

#var sociais nao ajustado
cox_health_sex <- coxph(Surv(delay_health) ~ sex,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = sex ~ "Sexo")

cox_health_age <- coxph(Surv(delay_health) ~ age_group,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = age_group ~ "Grupo Etário")

cox_health_coun <- coxph(Surv(delay_health) ~ country_inc,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = country_inc ~ "Incidência no País de Origem")

cox_health_com <- coxph(Surv(delay_health) ~ commu_resid,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = commu_resid ~ "Residência Comunitária")

cox_health_inm <- coxph(Surv(delay_health) ~ inmate,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = inmate ~ "Presidiário")

cox_health_unem <- coxph(Surv(delay_health) ~ unemployment,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = unemployment ~ "Desempregado")

cox_health_hjob <- coxph(Surv(delay_health) ~ health_job,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = health_job ~ "Profissional de Saúde")

cox_health_hom <- coxph(Surv(delay_health) ~ homeless,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = homeless ~ "Sem-abrigo")

cox_health_rur <- coxph(Surv(delay_health) ~ rurality,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = rurality ~ "Ruralidade")

cox_health_alc <- coxph(Surv(delay_health) ~ alcohol,
                     data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = alcohol ~ "Consumo de Álcool")

cox_health_drug <- coxph(Surv(delay_health) ~ drugs,
                      data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = drugs ~ "Consumo de Drogas")

cox_health_soc_unadj <-
  tbl_stack(
    tbls = list(cox_health_sex, cox_health_age, cox_health_coun,
                cox_health_com, cox_health_inm, cox_health_unem,
                cox_health_hjob, cox_health_hom, cox_health_rur,
                cox_health_alc, cox_health_drug)
  )
cox_health_soc_unadj

#variáveis contextuais ajustado
cox_health_cont_adj <- coxph(Surv(delay_health) ~ cluster(county) +
                            centro_saude_group + benef_group + school_group +
                            house_group + doc_rate_group,
                          data = TB_surv) %>%
  tbl_regression(exponentiate = TRUE,
                 label = c(centro_saude_group ~ "Centros de Saúde", 
                           benef_group ~ "Beneficiários de Apoio Social",
                           school_group ~ "Escolaridade",
                           house_group ~ "Sobrelotação",
                           doc_rate_group ~ "Médicos")
  )

#var contextuais nao ajustado
cox_health_cs <- coxph(Surv(delay_health) ~ cluster(county) + centro_saude_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = centro_saude_group ~ "Centros de Saúde")

cox_health_ben <- coxph(Surv(delay_health) ~ cluster(county) + benef_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = benef_group ~ "Beneficiários de Apoio Social")

cox_health_sch <- coxph(Surv(delay_health) ~ cluster(county) + school_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = school_group ~ "Escolaridade")

cox_health_hou <- coxph(Surv(delay_health) ~ cluster(county) + house_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = house_group ~ "Sobrelotação")

cox_health_doc <- coxph(Surv(delay_health) ~ cluster(county) + doc_rate_group,
                        data = TB_surv) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = doc_rate_group ~ "Médicos")

cox_health_cont_unadj <-
  tbl_stack(
    tbls = list(cox_health_cs, cox_health_ben, cox_health_sch,
                cox_health_hou, cox_health_doc)
  )
cox_health_cont_unadj


#### end ####

#### COMPILAÇÃO DE TABELAS ####
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

#variaveis clinicas
cox_clin_pat_final <-
  tbl_merge(tbls = list(cox_pat_clin_unadj, cox_pat_clin_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso do Utente - Variáveis Clínicas")
cox_clin_pat_final

cox_clin_health_final <-
  tbl_merge(tbls = list(cox_health_clin_unadj, cox_health_clin_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso dos Serviços de Saúde - Variáveis Clínicas")
cox_clin_health_final

#variaveis sociais
cox_soc_pat_final <-
  tbl_merge(tbls = list(cox_pat_soc_unadj, cox_pat_soc_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso do Utente - Variáveis Sociodemográficas")
cox_soc_pat_final

cox_soc_health_final <-
  tbl_merge(tbls = list(cox_health_soc_unadj, cox_health_soc_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso dos Serviços de Saúde - Variáveis Sociodemográficas")
cox_soc_health_final

#variaveis contextuais
cox_cont_pat_final <-
  tbl_merge(tbls = list(cox_pat_cont_unadj, cox_pat_cont_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso do Utente - Variáveis Contextuais")
cox_cont_pat_final

cox_cont_health_final <-
  tbl_merge(tbls = list(cox_health_cont_unadj, cox_health_cont_adj),
            tab_spanner = c("**Análise Não Ajustada**",
                            "**Análise Ajustada**")) %>% 
  as_gt() %>%
  gt::tab_header(title = "Atraso dos Serviços de Saúde - Variáveis Contextuais")
cox_cont_health_final

#### end ####


#### Schoenfeld test ####
#atraso utente
schoenfeld <- coxph(Surv(delay_pat) ~ sex + age_group +
                            HIV + inflam + respi +
                            diabetes + dialysis + cancer,
                          data = TB_surv)
cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)[7]) %>% 
  abline(h=0, col=2)

schoenfeld <- coxph(Surv(delay_pat) ~ sex + age_group +
                           country_inc + commu_resid + inmate +
                           unemployment + health_job + homeless +
                           rurality + alcohol + drugs,
                         data = TB_surv)
cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)[4]) %>% 
  abline(h=0, col=2)

schoenfeld <- coxph(Surv(delay_pat) ~ cluster(county) +
                               centro_saude_group + benef_group + school_group +
                               house_group + doc_rate_group,
                             data = TB_surv)
cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)[1]) %>% 
  abline(h=0, col=2)

#atraso serviços de saúde
schoenfeld <- coxph(Surv(delay_health) ~ sex + age_group +
                      HIV + inflam + respi +
                      diabetes + dialysis + cancer,
                    data = TB_surv)

cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)[2]) %>% 
  abline(h=0, col=2)

schoenfeld <- coxph(Surv(delay_health) ~ sex + age_group +
                      country_inc + commu_resid + inmate +
                      unemployment + health_job + homeless +
                      rurality + alcohol + drugs,
                    data = TB_surv)
cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)) %>% 
  abline(h=0, col=2)

schoenfeld <- coxph(Surv(delay_health) ~ cluster(county) +
                      centro_saude_group + benef_group + school_group +
                      house_group + doc_rate_group,
                    data = TB_surv)
cox.zph(schoenfeld)
plot(cox.zph(schoenfeld)[3]) %>% 
  abline(h=0, col=2)

#### end ####
