library(tidyverse)
library(cluster)
library(factoextra)

df_all <- read.csv(file.choose(), header=T) # select transformed_data.csv

summary(df_all)

# add variables
df_all$NORTHWEST <- FALSE
df_all$NORTHWEST[df_all$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- TRUE

df_all$NORTHWEST_DIALECT <- FALSE
df_all$NORTHWEST_DIALECT[df_all$IKIGOYI] <- TRUE
df_all$NORTHWEST_DIALECT[df_all$IKIRERA] <- TRUE

df_all$YOUNG <- FALSE
df_all$YOUNG[df_all$AGE < 35] <- TRUE

# CHECK TO MAKE SURE THAT TRIALS OF THE SAME FRAME LOOK LIKE EACH OTHER
# E.G. HABraINDngo1 looks like HABraINDngo2

conditions <- list("HAB0INDngo", "HABraINDngo", "PROG0INDDP", "PROG0INDngo",
                   "PROG0INDko", "PROG0NEG", "PROG0REL", "PROG0PART",
                   "PROGraINDDP", "PROGraINDngo", "PROGraINDko", "PROGraNEG",
                   "PROGraREL", "PROGraPART", "FUT0INDDP", "FUT0INDngo",
                   "FUT0INDko", "FUT0NEG", "FUT0REL", "FUT0PART", "FUTraINDDP",
                   "FUTraINDngo", "FUTraINDko", "FUTraNEG", "FUTraREL",
                   "FUTraPART")

for (condition in conditions) {
  condition1 <- paste(condition, "1", sep="")
  condition2 <- paste(condition, "2", sep="")
  
  filtered_data = df_all %>%
    select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
    filter(CONDITION_NAME %in%c(condition1, condition2)) %>%
    pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS")
  
  lm <- lm(paste(condition1, "~", condition2, sep=" "), data=filtered_data)
  print(summary(lm)) # TODO: what stat do we appeal to in here to say "1 looks like 2"?

  # print(ggplot(filtered_data, aes(.data[[condition1]], .data[[condition2]]))+geom_jitter()+geom_smooth(method="lm"))
}

# CREATE A VERSION WHERE ONLY ONE OF EACH FRAME IS REPRESENTED

df_wide = df_all %>%
  filter(!endsWith(CONDITION_NAME, "2")) %>%
  mutate(CONDITION_NAME = gsub("1", "", CONDITION_NAME)) %>%
  pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION", "IKIGOYI",
                        "IKINYAGISAKA", "IKINYAMBO", "IKIRERA", "IGIKIGA",
                        "NORTHWEST", "NORTHWEST_DIALECT", "YOUNG"),
              names_from="CONDITION_NAME",
              values_from="WOULD_YOU_SAY_THIS")

# PER FRAME, HOW MANY PEOPLE ACCEPT BOTH, RA ONLY, 0 ONLY, NEITHER?
generate_acceptability <- function(data) {
  acceptability <- data.frame(TAM=character(),
                              FRAME=character(),
                              BOTH=integer(),
                              RA_ONLY=integer(),
                              O_ONLY=integer(),
                              NEITHER=integer())
  
  tams = list("HAB", "PROG", "FUT")
  frames = list("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")
  
  for (tam in tams) {
    for (frame in frames) {
      condition_ra = paste(tam, "ra", frame, sep="")
      condition_0 = paste(tam, "0", frame, sep="")
      
      both = data %>%
        filter(.data[[condition_0]] >= 4) %>%
        filter(.data[[condition_ra]] >= 4) %>%
        nrow()
      
      ra_only = data %>%
        filter(.data[[condition_0]] < 4) %>%
        filter(.data[[condition_ra]] >= 4) %>%
        nrow()
      
      O_only = data %>%
        filter(.data[[condition_0]] >= 4) %>%
        filter(.data[[condition_ra]] < 4) %>%
        nrow()
      
      neither = data %>%
        filter(.data[[condition_0]] < 4) %>%
        filter(.data[[condition_ra]] < 4) %>%
        nrow()
      
      acceptability <- rbind(acceptability, list(tam, frame, both, ra_only, O_only, neither))
    }
  }
  
  colnames(acceptability) <- list("TAM", "FRAME", "BOTH", "RA_ONLY", "O_ONLY", "NEITHER")
  
  return(acceptability)
}

df_wide %>% generate_acceptability()

df_wide %>%
  filter(YOUNG) %>%
  generate_acceptability()

df_wide %>%
  filter(!YOUNG) %>%
  generate_acceptability()

df_wide %>%
  filter(NORTHWEST) %>%
  generate_acceptability()

df_wide %>%
  filter(!NORTHWEST) %>%
  generate_acceptability()

df_wide %>%
  filter(NORTHWEST_DIALECT) %>%
  generate_acceptability()

df_wide %>%
  filter(!NORTHWEST_DIALECT) %>%
  generate_acceptability()

df_wide %>%
  filter(PROGraINDfinal >= 4) %>%
  generate_acceptability()

df_wide %>%
  filter(FUTraINDfinal >= 4) %>%
  generate_acceptability()

# HYPOTHESIS TESTING

# is ra more obligatory before ngo among young people? i.e. is 0 less acceptable among young people? (p = 0.8901)

test = df_all %>%
  filter(CONDITION_NAME %in% c("HAB0INDngo1", "HAB0INDngo2"))
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# is 0 less acceptable before ngo among young northwesterners? (p = 0.1107)

test = df_all %>%
  filter(CONDITION_NAME %in% c("HAB0INDngo1", "HAB0INDngo2"), NORTHWEST)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# is the PROG reading of ra- less acceptable among young people? (p = 0.1586)

test = df_all %>%
  filter(CONDITION_NAME %in% c("PROGraINDfinal1", "PROGraINDfinal2"))
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# control: the FUT reading of ra- should be equally acceptable regardless of age for all

test = df_all %>%
  filter(CONDITION_NAME %in% c("FUTraINDfinal1", "FUTraINDfinal2"))
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# control: ra before DP objects should be equally acceptable regardless of age for all (p = 0.1947)
# TODO how to statistically show NO relationship between two variables?

test = df_all %>%
  filter(CONDITION_NAME %in% c("PROGraINDDP1", "PROGraINDDP2", "FUTraINDDP1", "FUTraINDDP2"))
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under negation is more acceptable among young people outside of the Northwest (p = 0.003761)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"), NORTHWEST==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under relativization is more acceptable among young people outside of the Northwest (p = 0.0373)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"), NORTHWEST==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under participials is more acceptable among young people outside of the Northwest (p = 0.6484)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"), NORTHWEST==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under negation is more acceptable among young people who don't speak ikigoyi / ikirera (p = 0.02133)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"), NORTHWEST_DIALECT==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under relativization is more acceptable among young people who don't speak ikigoyi / ikirera (p = 0.1147)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2"), NORTHWEST_DIALECT==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# TAM ra under participials is more acceptable among young people who don't speak ikigoyi / ikirera (p = 0.6774)

test = df_all %>%
  filter(CONDITION_NAME %in%c("PROGraPART1", "PROGraPART2", "FUTraPART1", "FUTraPART2"), NORTHWEST_DIALECT==FALSE)
cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

# HYPOTHESIS-RELATED PLOTS

# prog ra-
df_all %>%
  filter(CONDITION_NAME %in%c("PROGraINDfinal1", "PROGraINDfinal2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+geom_jitter()+geom_smooth(method="lm")

# ngo
df_all %>%
  filter(CONDITION_NAME %in%c("HAB0INDngo1", "HAB0INDngo2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+geom_jitter()+geom_smooth(method="lm")

df_all %>%
  filter(CONDITION_NAME %in%c("HAB0INDngo1", "HAB0INDngo2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST)+geom_jitter()+geom_smooth(method="lm")

df_all %>%
  filter(CONDITION_NAME %in%c("HAB0INDngo1", "HAB0INDngo2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()+geom_smooth(method="lm")

# DP
df_all %>%
  filter(CONDITION_NAME %in%c("PROGraINDDP1", "PROGraINDDP2", "FUTraINDDP1", "FUTraINDDP2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+geom_jitter()+geom_smooth(method="lm")

# negation
df_all %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST)+geom_jitter()+geom_smooth(method="lm")

df_all %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()+geom_smooth(method="lm")

# relativization
df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST)+geom_jitter()+geom_smooth(method="lm")

df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()+geom_smooth(method="lm")

# participial
df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST)+geom_jitter()+geom_smooth(method="lm")

df_all %>%
  filter(CONDITION_NAME %in%c("PROGraREL1", "PROGraREL2", "FUTraREL1", "FUTraREL2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()+geom_smooth(method="lm")

# WHAT DOES TAM RA- MEAN?

# pull stats for only the people who like PROG
prog_likers <- df_all %>%
  filter(
    (CONDITION_NAME %in% c("PROGraINDfinal1") & WOULD_YOU_SAY_THIS >= 4) |
    (CONDITION_NAME %in% c("PROGraINDfinal2") & WOULD_YOU_SAY_THIS >= 4)
  ) %>%
  pull(RESPONDENT_ID) %>%
  unique()

fut_likers <- df_all %>%
  filter(
    (CONDITION_NAME %in% c("FUTraINDfinal1") & WOULD_YOU_SAY_THIS >= 4) |
      (CONDITION_NAME %in% c("FUTraINDfinal2") & WOULD_YOU_SAY_THIS >= 4)
  ) %>%
  pull(RESPONDENT_ID) %>%
  unique()


df_all %>%
  filter(TAM=="FUT", FRAME=="REL", MORPHEME=="0", RESPONDENT_ID %in% prog_likers) %>%
  pull(WOULD_YOU_SAY_THIS) %>%
  mean()

# final
df_wide %>%
  ggplot(aes(PROGraINDfinal, FUTraINDfinal))+geom_jitter()

df_wide %>%
  filter(PROGraINDfinal < 4, FUTraINDfinal >= 4) %>%
  nrow()

df_wide %>%
  ggplot(aes(PROGraINDfinal, FUTraINDfinal))+facet_wrap(~YOUNG)+geom_jitter()

df_wide %>%
  ggplot(aes(PROGraINDfinal, FUTraINDfinal))+facet_wrap(~NORTHWEST)+geom_jitter()

df_wide %>%
  ggplot(aes(PROGraINDfinal, FUTraINDfinal))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()

# before a DP
df_wide %>%
  ggplot(aes(PROGraINDDP, FUTraINDDP))+facet_wrap(~YOUNG)+geom_jitter()

df_wide %>%
  ggplot(aes(PROGraINDDP, FUTraINDDP))+facet_wrap(~NORTHWEST)+geom_jitter()

df_wide %>%
  ggplot(aes(PROGraINDDP, FUTraINDDP))+facet_wrap(~NORTHWEST_DIALECT)+geom_jitter()