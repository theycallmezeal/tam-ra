library(tidyverse)
library(factoextra)
library(ordinal)

df_all <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df_all$NORTHWEST <- "Elsewhere"
df_all$NORTHWEST[df_all$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df_all$NORTHWEST_DIALECT <- FALSE
df_all$NORTHWEST_DIALECT[df_all$IKIGOYI] <- TRUE
df_all$NORTHWEST_DIALECT[df_all$IKIRERA] <- TRUE

df_all$YOUNG <- FALSE
df_all$YOUNG[df_all$AGE < 35] <- TRUE

df_all$IS_FEMALE <- FALSE
df_all$IS_FEMALE[df_all$GENDER == "female"] <- TRUE

summary(df_all)

df_new <- df_all %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS))

# CONJOINT VERBS BEFORE NGO

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))))

# MEANING OF PROG/FUT RA-

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal1", "FUTraINDfinal2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal1", "FUTraINDfinal2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal1", "FUTraINDfinal2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST
    + (1 | CONDITION_NAME)  + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal1", "FUTraINDfinal2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + (1 | CONDITION_NAME)  + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal1", "FUTraINDfinal2"))))

# WHO ACCEPTS PROG/FUT RA- IN NEG?

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ scale(AGE) * NORTHWEST
    + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST * FRAME
    + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2", "PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

library(lmerTest)


m1<-lmer(SCALED_WOULD_YOU_SAY_THIS
         ~ AGE * NORTHWEST + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
         data=df_new %>%
           filter(CONDITION_NAME %in% c(
             "PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2")))

summary(m1)

ggplot(df_new, aes(AGE, WOULD_YOU_SAY_THIS))+geom_smooth(method="lm")

ggplot(df_new, aes(AGE, SCALED_WOULD_YOU_SAY_THIS))+geom_smooth(method="lm")

summary(
  clmm(
    as.factor(SCALED_WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST * FRAME
    + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_new))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"))))

# WHO ACCEPTS PROG/FUT RA- IN REL?

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

df_all$NORTHWEST <- as.factor(df_all$NORTHWEST)

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * relevel(NORTHWEST, "Elsewhere")
    + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"))))

# WHO ACCEPTS PROG/FUT RA- IN PART?

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(CONDITION_NAME %in% c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"))))

# PERIPHRASTICS

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(startsWith(CONDITION_NAME, "PROGp"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(startsWith(CONDITION_NAME, "PROGp"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(startsWith(CONDITION_NAME, "PROGp"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(startsWith(CONDITION_NAME, "PROGp"))))

summary(
  clmm(
    as.factor(WOULD_YOU_SAY_THIS)
    ~ AGE * NORTHWEST_DIALECT
    + CONDITION_NAME + (1 | RESPONDENT_ID),
    data=df_all %>%
      filter(startsWith(CONDITION_NAME, "PROGp"))))
