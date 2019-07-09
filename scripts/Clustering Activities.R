library(tidyverse)
library(cluster)

#1. Use kk Means Clustering to group the observations in the mtcars data. 
#Is it important to standardise these data first? Vary the number of clusters 
#and choose an appropriate value for kk. Interpret the clusters.

?mtcars

set.seed(1234)

k <- kmeans(mtcars, centers = 4, nstart = 25)

table(k$cluster)

# look at characteristics within each cluster
mtcars_cluster <- as.data.frame(k$cluster)

df_cluster_mtcars <- bind_cols(as.data.frame(mtcars), k_cluster) %>% 
  rename(cluster = `k$cluster`)

ggplot(data = df_cluster_mtcars) + 
  geom_jitter(aes(x = mpg, y = disp, color = as.factor(cluster)))


#2. Use the animals data from the cluster package to perform hierarchical clustering. 
# Try different distance measures and select the one that makes the most sense. Check 
# ?dist to ensure that you understand how missing values are treated.

#3. Perform hierarchical clustering on the mtcars data. Plot the dendrogram. Does the 
# structure make sense?

#4. Perform hierarchical clustering on the USArrests data. Try all possible linkage 
# methods and see what effect they have on the results.