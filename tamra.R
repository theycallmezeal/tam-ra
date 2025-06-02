library(tidyverse)
library(cluster)
library(factoextra)

df_all <- read.csv(file.choose(), header=T) # select transformed_data.csv

summary(df_all)

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

df = df_all %>%
  filter(!endsWith(CONDITION_NAME, "2"))

df_wide = df %>%
  pivot_wider(names_from="RESPONDENT_ID", values_from="WOULD_YOU_SAY_THIS")

# PER FRAME, HOW MANY PEOPLE ACCEPT BOTH, RA ONLY, 0 ONLY, NEITHER?
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
    both = df %>%
      filter(TAM==tam) %>%
      filter(FRAME==frame) 
  }
}
