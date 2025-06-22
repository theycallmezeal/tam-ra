library(tidyverse)
library(factoextra)

df_raw <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df_raw$NORTHWEST <- "Elsewhere"
df_raw$NORTHWEST[df_raw$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df_raw$NORTHWEST_DIALECT <- FALSE
df_raw$NORTHWEST_DIALECT[df_raw$IKIGOYI] <- TRUE
df_raw$NORTHWEST_DIALECT[df_raw$IKIRERA] <- TRUE

df_wide = df_raw %>%
  pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                        "NORTHWEST", "NORTHWEST_DIALECT"),
              names_from="CONDITION_NAME",
              values_from="WOULD_YOU_SAY_THIS") %>%
  mutate(HAB0INDngo = (HAB0INDngo1 + HAB0INDngo2) / 2) %>%
  mutate(HABraINDngo = (HABraINDngo1 + HABraINDngo2) / 2) %>%
  mutate(PROG0INDDP = (PROG0INDDP1 + PROG0INDDP2) / 2) %>%
  mutate(PROG0INDngo = (PROG0INDngo1 + PROG0INDngo2) / 2) %>%
  mutate(PROG0INDko = (PROG0INDko1 + PROG0INDko2) / 2) %>%
  mutate(PROG0NEG = (PROG0NEG1 + PROG0NEG2) / 2) %>%
  mutate(PROG0REL = (PROG0REL1 + PROG0REL2) / 2) %>%
  mutate(PROG0PART = (PROG0PART1 + PROG0PART2) / 2) %>%
  mutate(PROGraINDDP = (PROGraINDDP1 + PROGraINDDP2) / 2) %>%
  mutate(PROGraINDfinal = (PROGraINDfinal1 + PROGraINDfinal2) / 2) %>%
  mutate(PROGraINDngo = (PROGraINDngo1 + PROGraINDngo2) / 2) %>%
  mutate(PROGraINDko = (PROGraINDko1 + PROGraINDko2) / 2) %>%
  mutate(PROGraNEG = (PROGraNEG1 + PROGraNEG2) / 2) %>%
  mutate(PROGraREL = (PROGraREL1 + PROGraREL2) / 2) %>%
  mutate(PROGraPART = (PROGraPART1 + PROGraPART2) / 2) %>%
  mutate(FUT0INDDP = (FUT0INDDP1 + FUT0INDDP2) / 2) %>%
  mutate(FUT0INDngo = (FUT0INDngo1 + FUT0INDngo2) / 2) %>%
  mutate(FUT0INDko = (FUT0INDko1 + FUT0INDko2) / 2) %>%
  mutate(FUT0NEG = (FUT0NEG1 + FUT0NEG2) / 2) %>%
  mutate(FUT0REL = (FUT0REL1 + FUT0REL2) / 2) %>%
  mutate(FUT0PART = (FUT0PART1 + FUT0PART2) / 2) %>%
  mutate(FUTraINDDP = (FUTraINDDP1 + FUTraINDDP2) / 2) %>%
  mutate(FUTraINDfinal = (FUTraINDfinal1 + FUTraINDfinal2) / 2) %>%
  mutate(FUTraINDngo = (FUTraINDngo1 + FUTraINDngo2) / 2) %>%
  mutate(FUTraINDko = (FUTraINDko1 + FUTraINDko2) / 2) %>%
  mutate(FUTraNEG = (FUTraNEG1 + FUTraNEG2) / 2) %>%
  mutate(FUTraREL = (FUTraREL1 + FUTraREL2) / 2) %>%
  mutate(FUTraPART = (FUTraPART1 + FUTraPART2) / 2) %>%
  select(-contains(c("1", "2")))

df_improvements = df_wide %>%
  mutate(HABINDDP = HABraINDDP - HAB0INDDP) %>%
  mutate(HABINDfinal = HABraINDfinal - HAB0INDfinal) %>%
  mutate(HABINDngo = HABraINDngo - HAB0INDngo) %>%
  mutate(HABINDko = HABraINDko - HAB0INDko) %>%
  mutate(HABNEG = HABraNEG - HAB0NEG) %>%
  mutate(HABREL = HABraREL - HAB0REL) %>%
  mutate(HABPART = HABraPART - HAB0PART) %>%
  mutate(PROGINDDP = PROGraINDDP - PROG0INDDP) %>%
  mutate(PROGINDfinal = PROGraINDfinal - PROG0INDfinal) %>%
  mutate(PROGINDngo = PROGraINDngo - PROG0INDngo) %>%
  mutate(PROGINDko = PROGraINDko - PROG0INDko) %>%
  mutate(PROGNEG = PROGraNEG - PROG0NEG) %>%
  mutate(PROGREL = PROGraREL - PROG0REL) %>%
  mutate(PROGPART = PROGraPART - PROG0PART) %>% 
  mutate(FUTINDDP = FUTraINDDP - FUT0INDDP) %>%
  mutate(FUTINDfinal = FUTraINDfinal - FUT0INDfinal) %>%
  mutate(FUTINDngo = FUTraINDngo - FUT0INDngo) %>%
  mutate(FUTINDko = FUTraINDko - FUT0INDko) %>%
  mutate(FUTNEG = FUTraNEG - FUT0NEG) %>%
  mutate(FUTREL = FUTraREL - FUT0REL) %>%
  mutate(FUTPART = FUTraPART - FUT0PART) %>%
  select(-contains(c("0", "ra", "PROGp"), ignore.case=FALSE)) %>%
  pivot_longer(!all_of(c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                         "NORTHWEST", "NORTHWEST_DIALECT")),
               names_to="CONDITION", values_to="IMPROVEMENT")

df_improvements$TAM <- "HAB"
df_improvements$TAM[grepl("PROG", df_improvements$CONDITION)] <- "PROG"
df_improvements$TAM[grepl("FUT", df_improvements$CONDITION)] <- "FUT"

df_improvements$FRAME <- "INDDP"
df_improvements$FRAME[grepl("INDfinal", df_improvements$CONDITION)] <- "INDfinal"
df_improvements$FRAME[grepl("INDngo", df_improvements$CONDITION)] <- "INDngo"
df_improvements$FRAME[grepl("INDko", df_improvements$CONDITION)] <- "INDko"
df_improvements$FRAME[grepl("NEG", df_improvements$CONDITION)] <- "NEG"
df_improvements$FRAME[grepl("REL", df_improvements$CONDITION)] <- "REL"
df_improvements$FRAME[grepl("PART", df_improvements$CONDITION)] <- "PART"

for (xTAM in c("HAB", "PROG", "FUT")) {
  for (xFRAME in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(xTAM, xFRAME, df_improvements %>%
              filter(TAM==xTAM, FRAME==xFRAME) %>%
              pull(IMPROVEMENT) %>%
              mean() %>%
              round(digits = 2)))
  }
}
df_improvements %>%
  filter(TAM=="HAB", FRAME=="INDDP") %>%
  pull(IMPROVEMENT) %>%
  mean() %>%
  round(digits = 2)
