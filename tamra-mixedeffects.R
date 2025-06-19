library(tidyverse)
library(factoextra)

df <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df$NORTHWEST <- "Elsewhere"
df$NORTHWEST[df$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df$NORTHWEST_DIALECT <- FALSE
df$NORTHWEST_DIALECT[df_all$IKIGOYI] <- TRUE
df$NORTHWEST_DIALECT[df_all$IKIRERA] <- TRUE

# scale responses

df = df %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS))

# scaled responses should not get higher or lower with age
ggplot(df, aes(AGE, WOULD_YOU_SAY_THIS))+geom_jitter()+geom_smooth(method="lm")
ggplot(df, aes(AGE, SCALED_WOULD_YOU_SAY_THIS))+geom_jitter()+geom_smooth(method="lm")
