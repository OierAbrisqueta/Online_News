library(dplyr)
library(ggplot2)
library(factoextra)
library(tidyr)

setwd("./data")

data <- read.csv("OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

quantile(data$shares,c(0.05,0.25,0.5,0.75,0.95,0.995))

data_clean <- data
nrow(data_clean)

boxplot(data_clean$shares)
hist(data_clean$shares)

d <- density(data_clean$shares)

plot(d)


# CATEGORICAL DATA

weekdays <- c(
  'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
  'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday',
  'weekday_is_sunday'
)

weekday_labels <- c(
  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
  'Saturday', 'Sunday'
)

data_clean$day_of_week <- names(data_clean[, weekdays])[max.col(data_clean[, weekdays])]

data_clean$day_of_week <- gsub("weekday_is_", "", data_clean$day_of_week)

data_clean$day_of_week <- factor(data_clean$day_of_week, 
                                 levels = c("monday", "tuesday", "wednesday", "thursday", 
                                            "friday", "saturday", "sunday"),
                                 labels = weekday_labels)

data_clean <- data_clean[, !(names(data_clean) %in% weekdays)]

class(data_clean$day_of_week)
head(data_clean)

summary(data_clean)

# Varibles de conteo

# n_tokens_content n_unique_tokens n_non_stop_words    n_non_stop_unique_tokens   num_hrefs 
# num_self_hrefs num_imgs num_videos self_reference_min_shares self_reference_max_shares 
# self_reference_avg_sharess kw_min_min kw_max_min kw_avg_min kw_min_max kw_max_max
# kw_avg_max kw_min_avg kw_max_avg kw_avg_avg 

data_clean <- data_clean[, !names(data_clean) %in% c("n_non_stop_words")]

data_clean <- data_clean %>%
  filter(n_tokens_content<=2247,
         n_unique_tokens<=1,
         n_non_stop_unique_tokens>=0.25,
         num_hrefs<=52,
         num_imgs<=33,
         num_videos<=21,
         self_reference_min_shares<=50200,
         self_reference_max_shares<=83300)

boxplot(data_clean$kw_min_min)
summary(data_clean$kw_min_min)
quantile(data_clean$kw_min_min, probs = c(0.25, 0.5, 0.75, 0.95, 0.99, 0.995))
nrow(data_clean)

#Articles with 0 words and all null values are deleted
data_clean <- data_clean %>% filter(n_tokens_content > 0)
summary(data_clean)

#Remove the column kw_min_min because 55% of the total articles has an unknown value (-1)
data_clean <- data_clean[, !names(data_clean) %in% c("kw_min_min")]
summary(data_clean)

#Remove the timedelta column. It represents the days from the article publication and the dataset creation. Even the UCI consideres it non-predictive
data_clean <- data_clean[, !names(data_clean) %in% c("timedelta")]
summary(data_clean)

#741 articles have a negative kw_avg_min value and 633 of them are -1. It represents again an unknown value, the rest negatives that are not -1
#are articles that have some valid keywords and some unkowns. We recode to NA.
data_clean$kw_avg_min[data_clean$kw_avg_min < 0] <- NA 
summary(data_clean)

#The column kw_min_avg has 5 unkown values
data_clean <- data_clean %>% filter(kw_min_avg >= 0)
summary(data_clean)

#The histogram shows shares are still right skewed
hist(data_clean$shares)
#This is why we create a log_shares column
data_clean$log_shares <- log(data_clean$shares)
summary(data_clean)
#We can see that now it is left skewed this happens because of the articles that
#have very few shares. This articles can be filtered in the future if needed.
hist(data_clean$log_shares)

colnames(data_clean)

print(data_clean[,c("n_unique_tokens","n_non_stop_unique_tokens")])
summary(data_clean$n_unique_tokens)
summary(data_clean$n_non_stop_unique_tokens)
cor(data_clean$n_unique_tokens, data_clean$n_non_stop_unique_tokens, use="complete.obs")

cor(data_clean$n_unique_tokens,data_clean$log_shares,use="complete.obs")
cor(data_clean$n_non_stop_unique_tokens,data_clean$log_shares,use="complete.obs")

data_clean <- data_clean %>% select(-n_unique_tokens)

#N_unique_tokens and n_non_stop_unique_tokens carry redundant information as it is seen by using the correlation of variables.
# By comparing the two columns , we see that the different between the these two columns is very similar in all the observations 
# The result is to eliminate n_unique_tokens since it includes separators or "stop words" 

kw_variables <- data_clean %>% select(starts_with("kw"))
correlation_matrix <- cor(kw_variables,use="complete.obs")
matrix_pairs <- as.data.frame(as.table(correlation_matrix))
matrix_pairs <- matrix_pairs %>% 
  mutate (Var1 = as.character(Var1) , Var2 = as.character(Var2) )%>% 
  filter(Var1 != Var2, abs(Freq) >= 0.8, abs(Freq) <= 0.99) %>%
  arrange(desc(abs(Freq))) 

print(matrix_pairs)

# We build a correlation matrix with all variables related to the keywords to see if there is correlation
# We see that between two of them there is a significant correlation.

cor(data_clean$kw_avg_min,data_clean$log_shares,use="complete.obs")
cor(data_clean$kw_max_min,data_clean$log_shares,use="complete.obs")

data_clean <- data_clean %>% select(-kw_max_min)

#Now we use the cor() function to see which variable is more correlated to the target variable 
# As a result , we eliminate the one with less correlation which is kw_max_min


polarity_variables <- data_clean %>% select(ends_with("polarity"))

correlation_matrix <- cor(polarity_variables,use="complete.obs")
matrix_pairs <- as.data.frame(as.table(correlation_matrix))
matrix_pairs <- matrix_pairs %>% 
  mutate (Var1 = as.character(Var1) , Var2 = as.character(Var2) )%>% 
  filter(Var1 != Var2, abs(Freq) >= 0.8, abs(Freq) <= 0.99) %>%
  arrange(desc(abs(Freq)))

print(matrix_pairs)

#We analyze if variables regarding polarity had a significant correlation between them , resulting negative


data_clean <- data_clean %>% rename(self_reference_avg_shares = self_reference_avg_sharess) # This column had an incorrect name

subjectivity_vars <- data_clean %>% select(contains("subjectivity"))
cor(subjectivity_vars, use="complete.obs")

#We analyze if variables regarding subjetivity had a significant correlation between them , resulting negative

cor(data_clean[, c("self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_shares")], use="complete.obs")

#Analyzing the correlation of variables regarding self_reference we observed that self_reference_avg_shares
#had a significant correlation ( >0.8) with the other two variables . The decision made in this point will depend on the
# correlation between these variables and the target variables

cor(data_clean$self_reference_avg_shares,data_clean$log_shares)
cor(data_clean$self_reference_min_shares,data_clean$log_shares)
cor(data_clean$self_reference_max_shares,data_clean$log_shares)

# As self_reference_avg_shares has a stronger correlation with log_shares the decision is to eliminate the other two
#variables : self_reference_min_shares and self_reference_max_shares.


cor(data_clean[, c("global_rate_positive_words", "global_rate_negative_words",
                   "rate_positive_words", "rate_negative_words")], use="complete.obs")

# Analyzing these variables of positive and negative word rate we observed a very extreme correlation value (>0.99)
#between global_rate_positive_words and global_rate_negative_words so , as before , we will compare each variable to the target

cor(data_clean$global_rate_negative_words,data_clean$log_shares)
cor(data_clean$global_rate_positive_words,data_clean$log_shares)

# The decision based on the correlation coefficients it to eliminate global_rate_negative_words 

lda_vars <- data_clean %>% select(starts_with("LDA"))
cor(lda_vars, use="complete.obs")

#We analyze if variables regarding LDA topics had a significant correlation between them , resulting negative

data_clean <- data_clean %>% select(-global_rate_negative_words,-self_reference_min_shares,-self_reference_max_shares)

#----------------------------------------------------------------
# PCA : Principal component analysis
#----------------------------------------------------------------

#----------------Correlation study before PCA-----------------------

colnames(data_clean)

#To derive a PCA analysis , we want to focus on the content of the article so we are
#going to study its correlations and select the variables according to this study.
#This study is performed in order to remove redundancy of variables.


# First, we compute the correlation matrix of the candidate variables

candidate_vars <- data_clean %>%
  select(n_tokens_content, num_hrefs, num_imgs, num_videos, num_keywords,
         kw_avg_avg, kw_max_avg, kw_avg_min, kw_min_avg,
         global_subjectivity, global_sentiment_polarity,
         rate_positive_words, rate_negative_words,
         avg_positive_polarity, min_positive_polarity, max_positive_polarity)

cor_matrix <- cor(candidate_vars, use = "complete.obs")

cor_matrix

#Now , we evaluate the correlation matrix aiming to find highly correlated
#pairs ( >0.75)

high_cor <- which(abs(cor_matrix) > 0.75 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor

#In this list , we have the pairs of variables which are higly correlated. To decide
#which one to keep , we compare the correlation of each component of the pair to the target
#variable : log_shares

cor(data_clean$kw_max_avg,data_clean$log_shares)
cor(data_clean$kw_avg_avg,data_clean$log_shares)

# kw_avg_avg has higher correlation with log_shares -> we remove kw_max_avg

cor(data_clean$rate_positive_words,data_clean$log_shares)

cor(data_clean$rate_negative_words,data_clean$log_shares)

# Very similar -> we remove rate_negative_words

cor(data_clean$global_sentiment_polarity,data_clean$log_shares)
cor(data_clean$rate_negative_words,data_clean$log_shares)

#We have already removed rate_negative_words

cor(data_clean$global_sentiment_polarity,data_clean$log_shares)
cor(data_clean$rate_positive_words,data_clean$log_shares)

# global_sentiment_polarity has higher correlation -> we remove rate_positive_words

# Final set of variables for PCA (13 variables):
# n_tokens_content, num_hrefs, num_imgs, num_videos, num_keywords,
# kw_avg_avg, kw_avg_min, kw_min_avg, global_subjectivity,
# global_sentiment_polarity, avg_positive_polarity,
# min_positive_polarity, max_positive_polarity

pca_vars <- candidate_vars %>% select(-rate_positive_words,-rate_negative_words,-kw_max_avg)

colnames(pca_vars)

#-----------------------Deriving PC´s------------------------------


pca_vars <- pca_vars %>% na.omit() #We omit the NA values as they generate problems when building the following matrices

X <- scale(pca_vars) #To derive principal components , we need all variables to be standardised

S <- cor(X) #We build the sample correlation matrix (same as covariance matrix when standardised)

E <- eigen(S) #We compute eigenvalues and eigenvectors of the matrix S

PCloadings <- E$vectors # The loadings are the eigenvectors of the matrix S 

rownames(PCloadings) <- colnames(pca_vars) # To remind us of the interpretation of PC loadings as coefficients of the standardised variables 
# in the linear combinations defining each PC, we rename rows appropriately

PCs <- X %*% PCloadings #We compute the principal components by the matrix multiplication of X and the loadings

variance_explained <- 100 * E$values / sum(E$values) 
variance_explained
cum_variance_explained <- cumsum(variance_explained)

#-----------------------PC analysis and interpretation------------------------------

# Creating the scree plot comparing cumulative variance explained and variance explained by each of the
#principal components

plot(cum_variance_explained,type="b",col="blue",ylim=c(0,100),xlab="Component index",ylab="Percentage",xaxt="n")
lines(variance_explained,type="b",col="red")
axis(1,at=1:length(cum_variance_explained))
legend(7.3, 60, legend=c("Cumul. variance explained", "Variance explained"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

# The scree (or elbow) plot reveals that no single component dominates the variance explanation,
#The first component explains approximately 20% of the total variance,
# and the curve flattens gradually without a clear elbow point.Therefore , we decide to select components
#based on the 80% criterion . This criterion consists on selecting the components that together
#(looking at the cumulative variance) explain >=80% of the total variance
#In this case , this point is the 7th component so we select the first 7 components,
# which together explain nearly 80% of the total variance while reducing the
# dimensionality from 13 variables to 7 components (a 46% reduction).

PCs_final <- PCs[,1:7]
PCs_final

# Scaled PC loadings give us the correlation between the principal components and the
# original variables. This is helpful to identify which variables drive each of the
# principal components.

scaled_loadings <- PCloadings
for (j in 1:ncol(PCloadings)){
  scaled_loadings[,j] <- sqrt(E$values[j]) * PCloadings[,j]
}

# We focus on the final components (first 7)
round(scaled_loadings[, 1:7], 3)

round(variance_explained[1:7],2)

# PC1 -> Positive polarity and subjectivity (20.14% of variance explained)
# The variables with highest correlation are avg_positive_polarity (-0.739),
# max_positive_polarity (-0.735) and global_subjectivity (-0.577).
# All correlations are negative, which means that articles with high scores on PC1
# tend to have less positive and less subjective content.

# PC2 -> Article length and minimum positive polarity (14.35% of variance explained)
# The variables with highest correlation are n_tokens_content (-0.741) and
# min_positive_polarity (0.674). This component give us information about the
#length of the article in contrast with the minimum positive polarity

# PC3 -> Keyword average popularity (12.17% of variance explained)
# The variables with highest correlation are kw_avg_avg (-0.686) and
# kw_min_avg (-0.657). This third component clearly captures information
#about the average keyword popularity . As in the first component , the most relevant
#correlations are negative meaning that articles with a high values on PC3
#tend to have lower values on those two variables.

# PC4 - Keyword usage (9.21% of variance explained)
# The variables with highest correlation are num_keywords (-0.661) and
# kw_avg_min (-0.637). This component captures how many keywords the
# article has and the average of the minimum shares of its keywords.

# PC5 -> Multimedia content (8.51% of variance explained)
# The variables with highest correlation are num_videos (0.557) and
# num_imgs (-0.558). This component contrasts articles with videos
# against articles with images.

# PC6 -> Video intensity (7.69% of variance explained)
# Dominated by num_videos (0.716). This component shows information about articles
# with a high number of videos.

# PC7 -> Subjectivity tone (6.06% of variance explained)
# Dominated by global_subjectivity (0.460). This component captures
# subjectivity not explained by previous components.

# After analyzing the correlations of each component, we can conclude that the majority
# of the variance (~56%, captured by PC1 to PC4) is driven by sentiment-related variables
# (positive polarity, subjectivity) and keyword popularity. This suggests that the
# emotional tone and keyword strategy of an article are the most informative dimensions
# in the dataset.

# On the other hand, the last components (PC5 to PC7) are associated with multimedia
# content (images and videos), which collectively explain a small fraction of the
# total variance. This reinforces the idea that multimedia features contribute
# relatively little to the overall structure of the data.

#-----------------------Plotting individuals------------------------------

#Due to dataset size , we take a sample of 1000 individuals to evaluate

set.seed(123)
index <- sample(1:nrow(PCs), 1000)

plot(PCs[index, 1], PCs[index, 2],
     type = "p",
     pch = 16,
     col = rgb(0, 0, 1, 0.3),
     xlab = "PC1 (20.14%)",
     ylab = "PC2(14.35%)",
     main = "PCA - Projection of individuals (sample n=1,000)")
abline(h = 0, v = 0, lty = 2)

#The plot of individuals on PC1 and PC2 shows that most of the articles are concentrated
#around the origin which is an indication of the majority of articles having average values
#on article length , positive polarity and subjectivity.

#Individuals are more scattered along PC1 , which is consistent with PC1 explaining a higher
#percentage of variance . 

#Some outliers are visible far from the origin , particularly along PC2 which indicate the existence
#of articles with extreme values of article length and minimum positive polarity.

#To reinforce the conclusions of above , we use summary to show the distribution of all individuals
#and not only of the sample of 1000 visible in the plot

summary(PCs[,1])  # Range of PC1
summary(PCs[,2])  # Range of PC2

# PC1 ranges from -14.80 to 7.02, with 50% of articles ranging between -1.05 and 1.11,
# meaning that this 50% of articles are concentrated close to the origin.

# PC2 ranges from -5.65 to 13.62, also with most articles close to the origin.

#In both dimensions we see extreme values which correspond to this points far away from the
#bulk of points , clearly visible in the plot.

#-----------------------Plotting variables------------------------------

#Now we are going to show the factor map of variables , which is very helpful
#to know the correlation of the variables with the first two components in a 
#visual way

# Plotting variables (factor map of variables)
plot(scaled_loadings[,1], scaled_loadings[,2],
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "PC1 (20.14%)", ylab = "PC2 (14.35%)",
     main = "PCA - Projection of variables",
     pch = 16, cex = 0.5)

# Add the correlation circle
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)

# Add arrows from origin to each variable
arrows(0, 0, scaled_loadings[,1], scaled_loadings[,2], length = 0.1)

# Add variable names
text(scaled_loadings[,1], scaled_loadings[,2],
     labels = rownames(scaled_loadings),
     cex = 0.7, pos = 3)

# Add reference lines
abline(h = 0, v = 0, lty = 2)

#The factor map shows the projection of the 13 variables onto PC1 and PC2.
#Variables which are closer to the origin (represented by the discontinuous lines)
#are poorly represented by the principal component while variables closer to the 
#edge of the circle (far away from origin) are better represented.

#PC1 --> All arrows point to the left meaning that PC1 is negatively correlated with all
#variables.The longest arrows along PC1 belong to avg_positive_polarity, max_positive_polarity 
#and global_subjectivity, consistent with the correlation analysis performed earlier.

#PC2 --> Along PC2 , in contrast with the situation of PC1 , we have arrows pointing up and down.
#This means that PC2 is positively correlated with some variables and negatively with other ones.
#Talking about arrows , the longest ones are min_positive_polarity pointing upward
# and n_tokens_content and num_hrefs pointing downward, confirming that PC2
# captures the opposition between article length and minimum positive polarity
#(as said before in the correlation analysis)


#CLUSTERING
#K-Means clustering

#Hierarchical clustering
cat("\nThe lenght of the data frame we are working with is: ", nrow(PCs_final), "\n")

#35400 obervations are too many for the hierarchical clustering algorith, 
#owing to the fact that it has a time complexity of O(n^3) or O(n^2logn) and a space complexity
#of O(n^2). We use a reproducible (with seed 1234) random sample of 2000 observation,
#which is large enough to capture to find the structure and paterns in the original data frame.
set.seed(555)
n <- 2000
index <- sample(1:nrow(PCs_final), n)

#We extract the observations with those indexes to create the new table
PCs_sample <- PCs_final[index,]
pca_vars_sample <- pca_vars[index,]

#Filtro outliers
#We plot the different PCs in order to filter outliers that
#can lead the hierarchical algorithm into misinterpretations
plot(PCs_sample[,1], PCs_sample[,2],
     main = "Outliers in PCA",
     xlab = "PC1",
     ylab = "PC2",
     pch = 19,
     col = "blue")

plot(PCs_sample[,3], PCs_sample[,4],
     main = "Outliers in PCA",
     xlab = "PC1",
     ylab = "PC2",
     pch = 19,
     col = "blue")

filtro_outliers <- PCs_sample[, 1] < 5 & PCs_sample[, 1] > -5 & PCs_sample[, 2] < 5 & PCs_sample[, 2] > -4.5 & PCs_sample[, 3] > -5 & PCs_sample[, 4] > -10
PCs_sample_clean <- PCs_sample[filtro_outliers,]
pca_vars_sample <- pca_vars_sample[filtro_outliers,]
nrow(PCs_sample_clean)
nrow(pca_vars_sample)
#Before computing the final distance matrix, we compare the three following distance
#metrics: Euclidean, Manhattan and Chebyshev
calculate_distance <- function(m) {
  return(dist(PCs_sample_clean, method = m))
}

distance_methods <- c("euclidean", "manhattan", "maximum")

#Compute the 3 distance matrixes
dist_matrix_list <- lapply(distance_methods, calculate_distance)
names(dist_matrix_list) <- distance_methods

fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(dist_matrix_list[["euclidean"]]),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "ward.D2",
             k.max = 10)

fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(dist_matrix_list[["manhattan"]]),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "ward.D2",
             k.max = 10)

fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(dist_matrix_list[["maximum"]]),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "ward.D2",
             k.max = 10)

#The silhouette analysis revealed the absolute peak is for k = 2 and it is
#for the Manhattan distance metric. However, as it will be discused further afterwards,
#k = 2 is not what we consider the perfect segmentation. So we will choose k = 4 (this will be discussed later)
#however Manhattan has as well the highest average silhouette width for 4 clusters. Therefore,
#we select Manhattan distance matrix to construct the hierarchical model.

distance_metric <- "euclidean"
distance_matrix <- dist_matrix_list[[distance_metric]]

cat("\nThe selected distance metric is", distance_metric, "\n")

heatmap(as.matrix(distance_matrix), Rowv = NA, Colv = NA)

#Once we have selected the appropriate distance metric, the next step is to choose
#between single or complete linkage. 

#For the comparison we perform another silhouette analysis.
fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(distance_matrix),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "complete",
             k.max = 10)

fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(distance_matrix),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "single",
             k.max = 10)

#It is impossible to decide between single or complete linkage visually through this analysis
#owing to the fact that both present visually equal values for k = 4.

#So in order to properly select a proper linkage method, we compute the clustering
#and compare the sizes of the clusters.
hc_single <- hclust(distance_matrix, method = "single")
hc_complete <- hclust(distance_matrix, method = "complete")

clusters_single <- cutree(hc_single, k = 4)
clusters_complete <- cutree(hc_complete, k = 4)

cat("\nSingle Linkage Clusters\n")
print(table(clusters_single))

cat("\nComplete Linkage Clusters\n")
print(table(clusters_complete))

#The results for both complete and single linkage have been terrible because both grouped
# +85% of the observations in the first cluster. Our conclusion on these results are that:
#1. Single Linkage fails because it ends up merging clusters because there happened to be a path
#of slightly similar articles connecting them. This concludes in on one giant cluster and a few tiny clusters of extrem outliers.
#2. Complete Linkage: even though it could seem safer, our dataset does have extreme values. This will conclude
#in clusters that are similar and should be merged together not merged and the clusters that are
#really different merged.

#Given that the both linkage methods taught in class did not produce the results we were looking for
#we made additional research to identify a suitable alternative.

#We have identified the Ward's method as an addecuate solution for this problem.
#The ward's method merges two clusters only if that results in the smallest increase in
#total within cluster variance. This makes this method better for chaining and outlier sensitivity.

hc_ward <- hclust(distance_matrix, method = "ward.D2")
clusters_ward <- cutree(hc_ward, k = 4)

cat("\nComplete Linkage Clusters\n")
print(table(clusters_ward))

#We can see that Ward's method is clearly an improvement. 
#Our next step is to determine the optimal number of clusters.

#Plot dendogram

#We plot the dendogram without labels to make the graph understandable
plot(hc_ward,
     main = "Dendrogram",
     xlab = "Observations",
     sub = "",
     ylab = "Height(Distance)",
     labels = FALSE,
     hang = -1)

#We compute again the silhouette analysis to confirm our decision.
fviz_nbclust(PCs_sample_clean,
             diss = as.matrix(distance_matrix),
             FUNcluster = hcut,
             method = "silhouette",
             hc_func = "hclust",
             hc_method = "ward.D2",
             k.max = 10)

#Both methods show that the best choice would be to divide it into 4 clusters

#The average silhouette width show worrying results. 0.15 is bellow what is considered weak.
#This does not necessarily invalidate the analysis, but  means that there is not a strong
#segmentation of the data. However, the clustering can still reveal tendencies and patterns.

#Final Clustering
final_k <- 4
final_model <- hc_ward
clusters <- cutree(final_model, k = final_k)
cat("\nFinal Hierarchical Clustering Model\n")
cat("Final amount of clusters", final_k, "\n")
cat("Cluster sizes:", "\n")
print(table(clusters))

#Visualize the clusters: We will plot the following combinations in order to capture enough variance:
#PC1 and PC2, PC1 and PC2, and PC2 and PC3
plot_data1 <- data.frame(
  PC1 <- PCs_sample_clean[,1],
  PC2 <- PCs_sample_clean[,2],
  Cluster = factor(clusters)
)

ggplot(plot_data1, aes(x= PC1, y = PC2, colour = Cluster)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon",
               alpha = 0.08, level = 0.95, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette   = "Set1") +
  labs(
    title = "Hierarchical Clustering Results (PC1 and PC2)",
    x = "PC1",
    y = "PC2",
    colour = "Cluster"
  ) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

plot_data2 <- data.frame(
  PC1 <- PCs_sample_clean[,1],
  PC3 <- PCs_sample_clean[,3],
  Cluster = factor(clusters)
)

ggplot(plot_data2, aes(x= PC1, y = PC3, colour = Cluster)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon",
               alpha = 0.08, level = 0.95, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette   = "Set1") +
  labs(
    title = "Hierarchical Clustering Results (PC1 and PC3)",
    x = "PC1",
    y = "PC3",
    colour = "Cluster"
  ) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

plot_data3 <- data.frame(
  PC2 <- PCs_sample_clean[,2],
  PC3 <- PCs_sample_clean[,3],
  Cluster = factor(clusters)
)

ggplot(plot_data1, aes(x= PC2, y = PC3, colour = Cluster)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon",
               alpha = 0.08, level = 0.95, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette   = "Set1") +
  labs(
    title = "Hierarchical Clustering Results (PC2 and PC3)",
    x = "PC2",
    y = "PC3",
    colour = "Cluster"
  ) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

#Cluster Interpretation
cluster_data <- as.data.frame(scale(pca_vars_sample))
cluster_data$Cluster <- factor(clusters)

cluster_means <- cluster_data %>% 
  group_by(Cluster) %>% 
  summarise(across(where(is.numeric), mean), .groups = "drop")

print(cluster_means, width = 1000)

#We transform the table for a better understanding of the following heatmap
plotting_data <- cluster_means %>% 
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Mean")

#We plot the results for a better understanding
ggplot(plotting_data, aes(x = Variable, y = Cluster, fill = Mean)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Mean, 2)), size = 3, colour = "black") +
  scale_fill_gradient2(low      = "blue",
                       mid      = "white",
                       high     = "red",
                       midpoint = 0,
                       name     = "z-score\n(cluster mean)") +
  labs(
    title = "Cluster Interpretation: Standarised Mean per Variable",
    y = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 9))