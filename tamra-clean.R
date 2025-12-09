library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)

df_raw <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df_raw$TAMMORPHEME <- paste(df_raw$TAM, df_raw$MORPHEME, sep="")

df_raw$NORTHWEST <- "Elsewhere"
df_raw$NORTHWEST[df_raw$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df_raw$NORTHWEST_DIALECT <- "Elsewhere"
df_raw$NORTHWEST_DIALECT[df_raw$IKIGOYI] <- "Northwest"
df_raw$NORTHWEST_DIALECT[df_raw$IKIRERA] <- "Northwest"
df_raw$NORTHWEST_DIALECT <- as.factor(df_raw$NORTHWEST_DIALECT)

df_raw$AGERANGE <- "20-29"
df_raw$AGERANGE[df_raw$AGE >= 30] <- "30-39"
df_raw$AGERANGE[df_raw$AGE >= 40] <- "40-49"
df_raw$AGERANGE[df_raw$AGE >= 50] <- "50-59"

# scale responses
df_raw = df_raw %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS))

widen <- function(df, values_from) {
  return(df %>%
    pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                          "NORTHWEST", "NORTHWEST_DIALECT"),
                names_from="CONDITION_NAME",
                values_from=values_from) %>%
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
    select(-contains(c("1", "2"))))
}

# morphological preference score
mps <- function(df, values_from) {
  result = widen(df, values_from) %>%
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
  
  result$TAM <- "HAB"
  result$TAM[grepl("PROG", result$CONDITION)] <- "PROG"
  result$TAM[grepl("FUT", result$CONDITION)] <- "FUT"
  
  result$FRAME <- "INDDP"
  result$FRAME[grepl("INDfinal", result$CONDITION)] <- "INDfinal"
  result$FRAME[grepl("INDngo", result$CONDITION)] <- "INDngo"
  result$FRAME[grepl("INDko", result$CONDITION)] <- "INDko"
  result$FRAME[grepl("NEG", result$CONDITION)] <- "NEG"
  result$FRAME[grepl("REL", result$CONDITION)] <- "REL"
  result$FRAME[grepl("PART", result$CONDITION)] <- "PART"
  
  return(result)
}

df_mps = mps(df_raw, "WOULD_YOU_SAY_THIS")
df_mps$SCALED_IMPROVEMENT = mps(df_raw, "SCALED_WOULD_YOU_SAY_THIS")$IMPROVEMENT

# tag respondents based on whether they accept prog or fut

accepts_prog = df_raw %>%
  filter(TAM == "PROG", FRAME == "INDfinal", MORPHEME == "ra", WOULD_YOU_SAY_THIS >= 4) %>%
  pull(RESPONDENT_ID)
df_raw$ACCEPTS_PROG <- "False"
df_raw$ACCEPTS_PROG[df_raw$RESPONDENT_ID %in% accepts_prog] <- "True"
df_mps$ACCEPTS_PROG <- "False"
df_mps$ACCEPTS_PROG[df_mps$RESPONDENT_ID %in% accepts_prog] <- "True"
  
accepts_fut = df_raw %>%
  filter(TAM == "FUT", FRAME == "INDfinal", MORPHEME == "ra", WOULD_YOU_SAY_THIS >= 4) %>%
  pull(RESPONDENT_ID)
df_raw$ACCEPTS_FUT <- "False"
df_raw$ACCEPTS_FUT[df_raw$RESPONDENT_ID %in% accepts_fut] <- "True"
df_mps$ACCEPTS_FUT <- "False"
df_mps$ACCEPTS_FUT[df_mps$RESPONDENT_ID %in% accepts_fut] <- "True"

# SECTION 5.3.2 OVERALL RESPONSES
# overall responses
for (tammorpheme in c("HAB0", "HABra", "PROG0", "PROGra", "PROGp", "FUT0", "FUTra")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(tammorpheme, frame, df_raw %>%
              filter(TAMMORPHEME == tammorpheme, FRAME == frame) %>%
              pull(WOULD_YOU_SAY_THIS) %>% mean() %>% round(digits=2)))
  }
}
# overall morphological preference scores
for (tam in c("HAB", "PROG", "FUT")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(tam, frame, df_mps %>%
              filter(TAM==tam, FRAME==frame) %>%
              pull(IMPROVEMENT) %>%
              mean() %>%
              round(digits = 2)))
  }
}

# SECTION 5.3.3 ACCEPTABILITY OF RA-LESS VERBS BEFORE NGO

df_raw %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID, GENDER, AGERANGE) %>%
  filter(CONDITION_NAME %in%c("HABraINDngo1", "HAB0INDngo1")) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  ggplot(aes(HABraINDngo1,HAB0INDngo1,colour=AGERANGE,shape=GENDER))+
  scale_shape_manual(values=c(15,16,17,18))+
  geom_point(position="jitter",size=3)+ylab("Acceptability of ra-less verb")+
  xlab("Acceptability of verb with ra-")

# too few observations to use morphological preference scores
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

# SECTION 5.3.4 MEANING OF PROG/FUT RA-

df_raw %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
  filter(CONDITION_NAME %in%c("PROGraINDfinal1", "FUTraINDfinal1", "PROGraINDfinal2", "FUTraINDfinal2")) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  mutate(PROG_AVG = mean(PROGraINDfinal1, PROGraINDfinal2)) %>%
  mutate(FUT_AVG = mean(FUTraINDfinal1, FUTraINDfinal2)) %>%
  ggplot(aes(PROG_AVG, FUT_AVG))+geom_jitter()+
  xlab("progressive")+ylab("near-future")

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * TAM * MORPHEME + GENDER * TAM * MORPHEME + NORTHWEST_DIALECT * TAM * MORPHEME + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(TAM %in% c("PROG", "FUT"), FRAME == "INDfinal")
  )
)

# 5.3.5 INDEPENDENCE OF TAM READING AND SYNTACTIC FRAME

df_mps %>%
  filter(TAM=="PROG") %>%
  select(RESPONDENT_ID, FRAME, IMPROVEMENT) %>%
  filter(RESPONDENT_ID %in% accepts_prog) %>%
  pivot_wider(names_from="FRAME", values_from="IMPROVEMENT") %>%
  summary()

df_mps %>%
  filter(TAM=="FUT") %>%
  select(RESPONDENT_ID, FRAME, IMPROVEMENT) %>%
  filter(RESPONDENT_ID %in% accepts_prog) %>%
  pivot_wider(names_from="FRAME", values_from="IMPROVEMENT") %>%
  summary()

df_raw %>%
  filter(TAM=="PROG", MORPHEME=="ra") %>%
  select(RESPONDENT_ID, FRAME, WOULD_YOU_SAY_THIS, CONDITION_NAME) %>%
  filter(RESPONDENT_ID %in% accepts_prog)

df_raw %>%
  filter(TAM=="PROG", MORPHEME=="ra", RESPONDENT_ID %in% accepts_prog)

# average mps for people who accept tam readings
widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_prog) %>%
  select(starts_with("PROG0") | starts_with("PROGra"))  %>%
  summary()

widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_fut) %>%
  select(starts_with("FUT0") | starts_with("FUTra")) %>%
  summary()

# how many people accept?
for (morpheme in c("ra", "0")) {
  for (tam in c("PROG", "FUT")) {
    for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
      print(c(morpheme, tam, frame, df_raw %>%
                filter(RESPONDENT_ID %in% if (tam == "PROG") accepts_prog else accepts_fut) %>%
                filter(MORPHEME == morpheme, FRAME == frame, TAM == tam) %>%
                filter(WOULD_YOU_SAY_THIS >= 4) %>%
                select(RESPONDENT_ID) %>%
                unique() %>%
                nrow()))
    }
  }
}


# SECTION 5.3.6 ACCEPTABILITY OF PROG/FUT ra- IN SYNTACTIC FRAMES

## negation
### ra
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "ra")
  )
)

### 0
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "0")
  )
)

###mps
summary(
  lmer(
    SCALED_IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | RESPONDENT_ID),
    data=df_mps %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")))
  )
)

## relativization
### ra
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "ra")
  )
)

### 0
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "0")
  )
)

###mps
summary(
  lmer(
    SCALED_IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | RESPONDENT_ID),
    data=df_mps %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")))
  )
)

## participial
### ra
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "ra")
  )
)

### 0
summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME == "0")
  )
)

###mps
summary(
  lmer(
    SCALED_IMPROVEMENT
    ~ AGE * GENDER * NORTHWEST + TAM + (1 | RESPONDENT_ID),
    data=df_mps %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")))
  )
)

# graphs
df_mps %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL", "PART")) %>%
  ggplot(aes(AGE, IMPROVEMENT, color=GENDER))+
  facet_wrap(
    ~factor(FRAME, levels=c("NEG", "REL", "PART")),
    labeller=as_labeller(c(`NEG`="negated", `REL`="relativized", `PART`="participial")))+
  geom_jitter()+geom_smooth(method="lm", se=FALSE)+
  ylab("Scaled preference for ra-")+xlab("Age")+labs(color="Gender")

df_raw %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL", "PART"), MORPHEME %in% c("ra", "0")) %>%
  ggplot(aes(AGE, SCALED_WOULD_YOU_SAY_THIS, color=GENDER))+
  facet_grid(
    factor(MORPHEME, levels=c("ra", "0")) ~ factor(FRAME, levels=c("NEG", "REL", "PART")),
    labeller=as_labeller(c(`NEG`="negated", `REL`="relativized", `PART`="participial", `ra`="ra-", `0`="0")))+
  geom_jitter()+geom_smooth(method="lm", se=FALSE)+
  ylab("Scaled preference for morpheme")+xlab("Age")+labs(color="Gender")

# does acceptance of negated ra- imply acceptance of rel / part or vice versa?

neg_rel_part = rbind(
  widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
    filter(RESPONDENT_ID %in% accepts_prog) %>%
    select(PROGraNEG, PROGraREL, PROGraPART) %>%
    rename(NEG = PROGraNEG) %>%
    rename(REL = PROGraREL) %>%
    rename(PART = PROGraPART) %>%
    mutate(TAM = "PROG"),
  
  widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
    filter(RESPONDENT_ID %in% accepts_fut) %>%
    select(FUTraNEG, FUTraREL, FUTraPART) %>%
    rename(NEG = FUTraNEG) %>%
    rename(REL = FUTraREL) %>%
    rename(PART = FUTraPART) %>%
    mutate(TAM = "FUT")
)

neg_rel_part %>%
  pivot_longer(names_to = "TYPE", values_to = "SCORE", cols=c("PART", "REL")) %>%
  ggplot(
    aes(NEG, SCORE)
  ) + geom_jitter() + geom_smooth(method="lm") +
  facet_wrap(~ factor(TYPE, levels=c("REL", "PART")), labeller=as_labeller(c(`NEG`="negated", `REL`="relativized", `PART`="participial"))) +
  xlab("Scaled score: ra- in negation") + ylab("Scaled score: ra- in other environment")

summary(
  lmer(REL ~ NEG * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part))

summary(
  lmer(PART ~ NEG * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part))

summary(
  lmer(REL ~ PART * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part)) 

# SECTION 5.3.7 PERIPHRASTICS

summary(widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
  mutate(INDfinal = PROGpINDfinal - pmax(PROGraINDfinal, PROG0INDfinal)) %>%
  mutate(INDDP = PROGpINDDP - pmax(PROGraINDDP, PROG0INDfinal)) %>%
  mutate(INDngo = PROGpINDngo - pmax(PROGraINDngo, PROG0INDfinal)) %>%
  mutate(INDko = PROGpINDko - pmax(PROGraINDko, PROG0INDfinal)) %>%
  mutate(NEG = PROGpNEG - pmax(PROGraNEG, PROG0NEG)) %>%
  mutate(REL = PROGpREL - pmax(PROGraREL, PROG0REL)) %>%
  mutate(PART = PROGpPART - pmax(PROGraPART, PROG0PART)) %>%
  select(-contains(c("0", "ra", "PROGp"), ignore.case=FALSE)))