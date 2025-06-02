library(tidyverse)
library(cluster)
library(factoextra)

df <- read.csv(file.choose(), header=T)

df$NORTHWEST<-"elsewhere"
df$NORTHWEST[df$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke", "Rubavu")]<-"northwest"

summary(df)

observations <- df %>%
  select(CONDITION_NAME, TAM, WOULD_YOU_SAY_THIS, RESPONDENT_ID, NORTHWEST) %>%
  # filter(NORTHWEST!="northwest") %>%
  select(-TAM, -NORTHWEST) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  ungroup() %>%
  column_to_rownames(var="RESPONDENT_ID") %>%
  na.omit() %>%
  scale()

summary(observations)

set.seed(123)
fviz_nbclust(observations, kmeans, method = "wss")
fviz_nbclust(observations, kmeans, method = "silhouette")
gap_stat <- clusGap(observations, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

k2 <- kmeans(observations, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data=observations)

print(k2)


demographics <- df %>%
  select(RESPONDENT_ID, NORTHWEST, AGE) %>%
  unique() %>%
  remove_rownames() %>%
  column_to_rownames(var="RESPONDENT_ID") %>%
  mutate(Cluster = k2$cluster)


