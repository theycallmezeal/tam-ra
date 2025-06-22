library(tidyverse)
library(factoextra)

df_raw <- read.csv(file.choose(), header=T) # select transformed_data.csv

# ngo
df_raw %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
  filter(CONDITION_NAME %in%c("HABraINDngo1", "HAB0INDngo1")) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  ggplot(aes(HABraINDngo1,HAB0INDngo1))+geom_jitter()

# prog vs fut
df_raw %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
  filter(CONDITION_NAME %in%c("PROGraINDfinal1", "FUTraINDfinal1")) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  ggplot(aes(PROGraINDfinal1,FUTraINDfinal1))+geom_jitter()

df_raw %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
  filter(CONDITION_NAME %in%c("HABraINDngo1", "HAB0INDngo1")) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  filter(HABraINDngo1 < 4, HAB0INDngo1 < 4) %>%
  nrow()
