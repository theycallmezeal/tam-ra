# install.package("tidyverse")
# install.package("cluster")
# install.package("factoextra")

library(tidyverse)
library(cluster)
library(factoextra)

df <- read.csv(file.choose(), header=T)

summary(df)

df$NORTHWEST<-"elsewhere"
df$NORTHWEST[df$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke", "Rubavu")]<-"northwest"

df %>%
  filter(CONDITION_NAME %in%c("HABraINDfinal", "HAB0INDfinal")) %>%
  ggplot(aes(CONDITION_NAME, HAVE_YOU_HEARD_THIS))+geom_boxplot()


df %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2")) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=TAM))+facet_wrap(~NORTHWEST)+geom_jitter()+geom_smooth(method="lm")

test = df %>%
  filter(CONDITION_NAME %in%c("PROGraNEG1", "PROGraNEG2", "FUTraNEG1", "FUTraNEG2"), NORTHWEST!="northwest")

  cor.test(test$AGE, test$WOULD_YOU_SAY_THIS)

  
ngo <- df %>%
    select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
    filter(CONDITION_NAME %in%c("HABraINDngo1", "HAB0INDngo1", "HABraINDngo2", "HAB0INDngo2")) %>%
    pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
    ggplot(aes(HABraINDngo1,HAB0INDngo1))+geom_jitter()



