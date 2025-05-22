library(tidyverse)
library(cluster)
library(factoextra)

df <- read.csv(file.choose(), header=T)

summary(df)

observations <- df %>%
  select(CONDITION_NAME, WOULD_YOU_SAY_THIS, RESPONDENT_ID) %>%
  pivot_wider(names_from="CONDITION_NAME", values_from="WOULD_YOU_SAY_THIS") %>%
  ungroup()

observations <- na.omit(observations)
observations <- scale(observations)

summary(observations)

k2 <- kmeans(observations, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data=observations)


set.seed(123)
fviz_nbclust(observations, kmeans, method = "wss")
fviz_nbclust(observations, kmeans, method = "silhouette")
gap_stat <- clusGap(observations, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
