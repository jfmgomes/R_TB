library(survival)
library(survminer)
library(dplyr)
library(tidyverse)
library(ggplot2)

#### COX UTENTE ####
#clinicas
cox_pat_clin_adj <- coxph(Surv(delay_pat) ~ sex + age_group +
                            HIV + inflam + respi +
                            diabetes + dialysis + cancer,
                          data = TB_surv)

g1 <- ggforest(
  cox_pat_clin_adj,
  data = TB_surv,
  main = "Risco Proporcional para Atraso do Utente",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g1, filename="forestplots/HR_pat_clin.png", width=7, height=7)

#sociais
cox_pat_soc_adj <- coxph(Surv(delay_pat) ~ sex + age_group +
                           country_inc + commu_resid + inmate +
                           unemployment + health_job + homeless +
                           rurality + alcohol + drugs,
                         data = TB_surv)

g2 <- ggforest(
  cox_pat_soc_adj,
  data = TB_surv,
  main = "Risco Proporcional para Atraso do Utente",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g2, filename="forestplots/HR_pat_soc.png", width=7, height=7)

#contextuais
cox_pat_cont_adj <- coxph(Surv(delay_pat) ~ centro_saude_group +
                            benef_group + school_group +
                            house_group + doc_rate_group,
                          cluster = county,
                          data = TB_surv)

g3 <- ggforest(
  cox_pat_cont_adj,
  data = TB_surv,
  covariates = c("centro_saude_group", "benef_group",
                 "school_group", "house_group",
                 "doc_rate_group"),
  main = "Risco Proporcional para Atraso do Utente",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g3, filename="forestplots/HR_pat_cont.png", width=7, height=7)



#### end ####

#### COX SERVIÇOS SAUDE ####
#clinicas
cox_health_clin_adj <- coxph(Surv(delay_health) ~ sex + age_group +
                            HIV + inflam + respi +
                            diabetes + dialysis + cancer,
                          data = TB_surv)

g1 <- ggforest(
  cox_health_clin_adj,
  data = TB_surv,
  main = "Risco Proporcional para Atraso dos Serviços de Saúde",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g1, filename="forestplots/HR_health_clin.png", width=7, height=7)

#sociais
cox_health_soc_adj <- coxph(Surv(delay_health) ~ sex + age_group +
                           country_inc + commu_resid + inmate +
                           unemployment + health_job + homeless +
                           rurality + alcohol + drugs,
                         data = TB_surv)

g2 <- ggforest(
  cox_health_soc_adj,
  data = TB_surv,
  main = "Risco Proporcional para Atraso dos Serviços de Saúde",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g2, filename="forestplots/HR_health_soc.png", width=7, height=7)

#contextuais
cox_health_cont_adj <- coxph(Surv(delay_health) ~ centro_saude_group +
                            benef_group + school_group +
                            house_group + doc_rate_group,
                          cluster = county,
                          data = TB_surv)

g3 <- ggforest(
  cox_health_cont_adj,
  data = TB_surv,
  covariates = c("centro_saude_group", "benef_group",
                 "school_group", "house_group",
                 "doc_rate_group"),
  main = "Risco Proporcional para Atraso dos Serviços de Saúde",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "referência",
  noDigits = 2
)

ggsave(plot=g3, filename="forestplots/HR_health_cont.png", width=7, height=7)
#### end ####