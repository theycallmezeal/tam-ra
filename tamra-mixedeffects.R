library(tidyverse)
library(factoextra)
library(lmerTest)
library(cluster)

df_raw <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df_raw$NORTHWEST <- "Elsewhere"
df_raw$NORTHWEST[df_raw$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df_raw$NORTHWEST_DIALECT <- "Elsewhere"
df_raw$NORTHWEST_DIALECT[df_raw$IKIGOYI] <- "Northwest"
df_raw$NORTHWEST_DIALECT[df_raw$IKIRERA] <- "Northwest"
df_raw$NORTHWEST_DIALECT <- as.factor(df_raw$NORTHWEST_DIALECT)

# scale responses

df_raw = df_raw %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS))

# scaled responses should not get higher or lower with age
ggplot(df_raw, aes(AGE, WOULD_YOU_SAY_THIS))+geom_jitter()+geom_smooth(method="lm")
ggplot(df_raw, aes(AGE, SCALED_WOULD_YOU_SAY_THIS))+geom_jitter()+geom_smooth(method="lm")

df_wide = df_raw %>%
  pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                        "NORTHWEST", "NORTHWEST_DIALECT"),
              names_from="CONDITION_NAME",
              values_from="SCALED_WOULD_YOU_SAY_THIS") %>%
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
  mutate(PERIPHRASTICINDDP = PROGpINDDP - pmax(PROGraINDDP, PROG0INDDP)) %>%
  mutate(PERIPHRASTICINDfinal = PROGpINDfinal - pmax(PROGraINDfinal, PROG0INDfinal)) %>%
  mutate(PERIPHRASTICINDngo = PROGpINDngo - pmax(PROGraINDngo, PROG0INDngo)) %>%
  mutate(PERIPHRASTICINDko = PROGpINDko - pmax(PROGraINDko, PROG0INDko)) %>%
  mutate(PERIPHRASTICNEG = PROGpNEG - pmax(PROGraNEG, PROG0NEG)) %>%
  mutate(PERIPHRASTICREL = PROGpREL - pmax(PROGraREL, PROG0REL)) %>%
  mutate(PERIPHRASTICPART = PROGpPART - pmax(PROGraPART, PROG0PART)) %>%
  select(-contains(c("0", "ra", "PROGp"), ignore.case=FALSE)) %>%
  pivot_longer(!all_of(c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                         "NORTHWEST", "NORTHWEST_DIALECT")),
               names_to="CONDITION", values_to="IMPROVEMENT")

df_improvements$TAM <- "HAB"
df_improvements$TAM[grepl("PROG", df_improvements$CONDITION)] <- "PROG"
df_improvements$TAM[grepl("FUT", df_improvements$CONDITION)] <- "FUT"
df_improvements$TAM[grepl("PERIPHRASTIC", df_improvements$CONDITION)] <- "PERIPHRASTIC"
df_improvements$TAM <- as.factor(df_improvements$TAM)

df_improvements$FRAME <- "INDDP"
df_improvements$FRAME[grepl("INDfinal", df_improvements$CONDITION)] <- "INDfinal"
df_improvements$FRAME[grepl("INDngo", df_improvements$CONDITION)] <- "INDngo"
df_improvements$FRAME[grepl("INDko", df_improvements$CONDITION)] <- "INDko"
df_improvements$FRAME[grepl("NEG", df_improvements$CONDITION)] <- "NEG"
df_improvements$FRAME[grepl("REL", df_improvements$CONDITION)] <- "REL"
df_improvements$FRAME[grepl("PART", df_improvements$CONDITION)] <- "PART"

# EVERYTHING
summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST
    + TAM + FRAME + (1 | RESPONDENT_ID),
    data=df_improvements
  )
)


# NGO

# too few observations to use improvements
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(CONDITION_NAME %in% c("HAB0INDngo1", "HAB0INDngo2"))
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST_DIALECT + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(CONDITION_NAME %in% c("HAB0INDngo1", "HAB0INDngo2"))
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST_DIALECT + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(CONDITION_NAME %in% c("HABraINDngo1", "HABraINDngo2"))
  )
)

# There were no effects of age, gender, or dialect on 0 usage.

# However, there was a marginal main effect of gender on ra- usage such that women rated ra- higher, but this effect was attentuated by increased age.

# MEANING

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * TAM * MORPHEME + GENDER * TAM * MORPHEME + NORTHWEST_DIALECT * TAM * MORPHEME + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(TAM %in% c("PROG", "FUT"), FRAME == "INDfinal")
  )
)

# No effects of age, gender, or dialect were found to predict {variable} individually or in interactions.

# NEGATION, RELATIVIZATION

summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST
    + TAM + FRAME + (1 | RESPONDENT_ID),
    data=df_improvements %>%
      filter(FRAME %in% c("NEG", "REL"), TAM %in% c("PROG", "FUT"))
    )
)

# A significant main effect of age was found such that younger people showed stronger preference for ra-.
# Another significant main effect of gender was found such that women showed stronger preference for ra-.
# There was an interaction between age and gender such that the effect of gender was attenuated with increasing age.

# As age increases, the effect of women preferring ra- becomes less, because the estimate for AGE:GENDERmale is the opposite of AGE and GENDERmale.

# PARTICIPIAL

summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST * TAM + (1 | RESPONDENT_ID),
    data=df_improvements %>%
      filter(FRAME %in% c("PART"), TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST * relevel(TAM, "PROG") + (1 | RESPONDENT_ID),
    data=df_improvements %>%
      filter(FRAME %in% c("PART"), TAM %in% c("PROG", "FUT"))
  )
)

# A significant interaction between gender and region was found such that men in the Northwest showed greater preference for ra- than the other respondents.
# However, a significant interaction between age, gender, and region was found such that the above effect was attentuated by increasing age.

# PERIPHRASTIC
summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST
    + FRAME + (1 | RESPONDENT_ID),
    data=df_improvements %>%
      filter(TAM == "PERIPHRASTIC")
  )
)

summary(
  lmer(
    IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST_DIALECT
    + FRAME + (1 | RESPONDENT_ID),
    data=df_improvements %>%
      filter(TAM == "PERIPHRASTIC")
  )
)

# CLUSTERS?

df_clustering = df_wide %>%
  select(!c(AGE, GENDER, REGION, NORTHWEST, NORTHWEST_DIALECT)) %>%
  column_to_rownames(var="RESPONDENT_ID") %>%
  na.omit()

set.seed(123)
fviz_nbclust(df_clustering, kmeans, method = "wss")
fviz_nbclust(df_clustering, kmeans, method = "silhouette")
fviz_gap_stat(clusGap(df_clustering, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50))
