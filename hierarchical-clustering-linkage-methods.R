# read in and reformat data
mt <- read.csv("mtcounty.csv", header = T) 
row.names(mt) <- mt$County
mt <- mt[c(-1)]
mt$Beale <- as.factor(mt$Beale)

# withhold beale code to compare to clusters
mt2 <- mt[c(-1)]

# summary statistics
library(psych)
library(knitr)
descMt2 <- describe(mt2)
kable(descMt2[c(2:5, 8:10)])

# missing data pattern
library(mice)
mtPattern <- md.pattern(mt, rotate.names = T)

# distance matrix using gower's method and dealing with missing values
library(vegan)
distmt2 <- vegdist(mt2, method = "gower", na.rm = TRUE)

# single linkage
hcmt2C <- hclust(distmt2, "single")
plot(as.dendrogram(hcmt2C), main = "Cluster Dendrogram using Single Linkage")

# complete linkage
hcmt2C <- hclust(distmt2, "complete")
plot(as.dendrogram(hcmt2C), main = "Cluster Dendrogram using Complete Linkage")

# average linkage
hcmt2A <- hclust(distmt2, "average")
plot(as.dendrogram(hcmt2A), main = "Cluster Dendrogram using Average Linkage")

# ward's method
hcmt2W <- hclust(distmt2, "ward.D2")
plot(as.dendrogram(hcmt2W), main = "Cluster Dendrogram using Ward's Method")

# cluster assignments
cutree(hcmt2A, 3)

# compare clusters to beale code
library(RColorBrewer)
tableMt <- table(factor(cutree(hcmt2A, 3)), mt$Beale)
tableMt
mosaicplot(tableMt, xlab = "Beale Code", ylab = "Clusters", 
           dir = c("h", "v"), off = 15, legend = T,
           main = "", color = brewer.pal(n = 6, name = "PuBuGn"))

# scale btwn 0 and 1 for pcp
scale01 <- function(vec){
  vec1 <- vec[!is.na(vec)]
  (vec-min(vec1))/(max(vec1)-min(vec1))
}
mt2Sc <- sapply(mt2, scale01)

# color palette
palette(brewer.pal(n = 3, name = "Set2"))

# parallel coordinate plot
matplot(t(mt2Sc), ylab = "Scaled responses", 
        xaxt = "n", type = "l", lwd = 2, lty = 2,
        col = factor(cutree(hcmt2A, 3)))
tics = 1:ncol(mt2)
xlabs <- names(mt2)
tck = axis(1, at = tics, labels = FALSE)
text(tck, par("usr")[3], labels = xlabs, srt = 310, xpd = TRUE, 
     adj = c(-0.2, 1), cex = 0.65)

# add means on 0,1 scale
library(dplyr)
library(tibble)
mt2Sc2 <- mutate(as_tibble(mt2Sc), 
                 Cluster = factor(cutree(hcmt2A, 3))) 
means <- mt2Sc2 %>% group_by(Cluster) %>% summarise_all(mean, na.rm = TRUE)

# plot scaled means
matplot(t(means[c(-1)]), add = T, type = "l", lty = 1, lwd = 4,
        col = means$Cluster)
legend("topright", legend = means$Cluster, lwd = 4,
       col = means$Cluster)

# average wage original scale by cluster
library(beanplot)
beanplot(mt2$Average.wage ~ factor(cutree(hcmt2A, 3)), 
         method = "jitter", log = "", xlab = "Cluster",
         ylab = "Wage (dollars)",
         col = list("#66C2A5", "#FC8D62", "#8DA0CB"))


