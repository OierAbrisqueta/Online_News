library(dplyr)
library(FactoMineR)
library(factoextra)


setwd("./Online_News/data")

data <- read.csv("OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

quantile(data$shares,c(0.05,0.25,0.5,0.75,0.95,0.995))

data_clean <- data
nrow(data_clean)


# The histogram shows that the distribution of shares is right-skewed because the mean is greater than the median.
boxplot(data_clean$shares)
hist(data_clean$shares)

d <- density(data_clean$shares)
plot(d)


# CATEGORICAL DATA

# Binary columns representing each day of the week
weekdays <- c(
  'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
  'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday',
  'weekday_is_sunday'
)

# Readable labels for each day
weekday_labels <- c(
  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
  'Saturday', 'Sunday'
)

# We create a single categorical column for the day of the week.
# max.col() returns the index of the column with value 1 for each row,
# which we then map to the corresponding label.
data_clean$day_of_week <- names(data_clean[, weekdays])[max.col(data_clean[, weekdays])]

# We remove the "weekday_is_" prefix to keep only the day name
data_clean$day_of_week <- gsub("weekday_is_", "", data_clean$day_of_week)

# We convert the column to a factor so R treats it as a categorical variable
data_clean$day_of_week <- factor(data_clean$day_of_week, 
                                 levels = c("monday", "tuesday", "wednesday", "thursday", 
                                            "friday", "saturday", "sunday"),
                                 labels = weekday_labels)

# Once the new column is created, we remove the original binary weekday columns
data_clean <- data_clean[, !(names(data_clean) %in% weekdays)]

class(data_clean$day_of_week)
head(data_clean)
summary(data_clean)


# DATA CLEANING

# n_non_stop_words is removed as it is redundant with other token-based variables
data_clean <- data_clean[, !names(data_clean) %in% c("n_non_stop_words")]

# We apply upper-bound filters to remove extreme outliers in count variables.
# Thresholds were chosen based on quantile analysis performed during the exploratory phase.
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

# Articles with 0 words and all null values are deleted
data_clean <- data_clean %>% filter(n_tokens_content > 0)
summary(data_clean)

# We remove kw_min_min because 55% of the total articles have an unknown value (-1)
data_clean <- data_clean[, !names(data_clean) %in% c("kw_min_min")]
summary(data_clean)

# We remove the timedelta column. It represents the days between article publication
# and the dataset acquisition date. The UCI repository itself considers it non-predictive.
data_clean <- data_clean[, !names(data_clean) %in% c("timedelta")]
summary(data_clean)

# 741 articles have a negative kw_avg_min value and 633 of them are exactly -1,
# which represents an unknown value. The remaining negatives correspond to articles
# with some valid keywords and some unknowns. We recode all of them to NA.
data_clean$kw_avg_min[data_clean$kw_avg_min < 0] <- NA 
summary(data_clean)

# The column kw_min_avg has 5 unknown values (negative), which we remove entirely
data_clean <- data_clean %>% filter(kw_min_avg >= 0)
summary(data_clean)

# The histogram confirms shares are still right-skewed after cleaning
hist(data_clean$shares)

# We create a log_shares column to reduce the skewness of the target variable.
# Working with log(shares) improves the behaviour of regression and other models.
data_clean$log_shares <- log(data_clean$shares)
summary(data_clean)
hist(data_clean$log_shares)

colnames(data_clean)

# n_unique_tokens and n_non_stop_unique_tokens carry redundant information,
# as confirmed by their very high mutual correlation.
# We keep n_non_stop_unique_tokens because it excludes stop words,
# making it a cleaner measure of vocabulary richness.
print(data_clean[,c("n_unique_tokens","n_non_stop_unique_tokens")])
summary(data_clean$n_unique_tokens)
summary(data_clean$n_non_stop_unique_tokens)
cor(data_clean$n_unique_tokens, data_clean$n_non_stop_unique_tokens, use="complete.obs")

cor(data_clean$n_unique_tokens,data_clean$log_shares,use="complete.obs")
cor(data_clean$n_non_stop_unique_tokens,data_clean$log_shares,use="complete.obs")

data_clean <- data_clean %>% select(-n_unique_tokens)

# We build a correlation matrix for all keyword-related variables to detect redundancy
kw_variables <- data_clean %>% select(starts_with("kw"))
correlation_matrix <- cor(kw_variables,use="complete.obs")
matrix_pairs <- as.data.frame(as.table(correlation_matrix))
matrix_pairs <- matrix_pairs %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>% 
  filter(Var1 != Var2, abs(Freq) >= 0.8, abs(Freq) <= 0.99) %>%
  arrange(desc(abs(Freq))) 

print(matrix_pairs)

# We found a significant correlation between kw_avg_min and kw_max_min.
# We compare their individual correlations with the target to decide which one to keep.
cor(data_clean$kw_avg_min,data_clean$log_shares,use="complete.obs")
cor(data_clean$kw_max_min,data_clean$log_shares,use="complete.obs")

# kw_avg_min has a higher correlation with log_shares, so we remove kw_max_min
data_clean <- data_clean %>% select(-kw_max_min)

# We analyze if polarity-related variables show significant correlation between them
polarity_variables <- data_clean %>% select(ends_with("polarity"))

correlation_matrix <- cor(polarity_variables,use="complete.obs")
matrix_pairs <- as.data.frame(as.table(correlation_matrix))
matrix_pairs <- matrix_pairs %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>% 
  filter(Var1 != Var2, abs(Freq) >= 0.8, abs(Freq) <= 0.99) %>%
  arrange(desc(abs(Freq)))

print(matrix_pairs)

# No significant correlations found among polarity variables, so no removals needed

# This column had a typo in the original dataset (extra 's'), we rename it for clarity
data_clean <- data_clean %>% rename(self_reference_avg_shares = self_reference_avg_sharess)

# We analyze if subjectivity-related variables show significant correlation between them
subjectivity_vars <- data_clean %>% select(contains("subjectivity"))
cor(subjectivity_vars, use="complete.obs")

# No significant correlations found among subjectivity variables

# We analyze the correlation among the three self_reference variables
cor(data_clean[, c("self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_shares")], use="complete.obs")

# self_reference_avg_shares shows a high correlation (>0.8) with the other two.
# We compare each variable's correlation with the target to decide which one to keep.
cor(data_clean$self_reference_avg_shares,data_clean$log_shares)
cor(data_clean$self_reference_min_shares,data_clean$log_shares)
cor(data_clean$self_reference_max_shares,data_clean$log_shares)

# self_reference_avg_shares has the strongest correlation with log_shares,
# so we remove the other two: self_reference_min_shares and self_reference_max_shares.

# We analyze the correlation among positive and negative word rate variables
cor(data_clean[, c("global_rate_positive_words", "global_rate_negative_words",
                   "rate_positive_words", "rate_negative_words")], use="complete.obs")

# We found a very high correlation (>0.99) between global_rate_positive_words
# and global_rate_negative_words. We compare each with the target to decide.
cor(data_clean$global_rate_negative_words,data_clean$log_shares)
cor(data_clean$global_rate_positive_words,data_clean$log_shares)

# global_rate_positive_words has a higher correlation with log_shares,
# so we remove global_rate_negative_words

# We analyze if LDA topic variables show significant correlations between them
lda_vars <- data_clean %>% select(starts_with("LDA"))
cor(lda_vars, use="complete.obs")

# No significant correlations found among LDA variables

# We apply all variable removals decided in the steps above
data_clean <- data_clean %>% select(-global_rate_negative_words,
                                    -self_reference_min_shares,
                                    -self_reference_max_shares)


#----------------------------------------------------------------
# CORRESPONDENCE ANALYSIS (CA)
#----------------------------------------------------------------

# The goal of this analysis is to study whether there is a relationship between
# the thematic channel of an article (technology, entertainment, etc.)
# and the day of the week on which it was published.
# Both variables are categorical, which makes CA the appropriate technique.


#---------- STEP 1: Create the thematic channel categorical variable ----------

# The channel variables are currently stored as binary columns (0 or 1),
# one per channel. We need to convert them into a single categorical column,
# following the same logic used earlier for the day of the week.

# Names of the binary channel columns in the dataset
channel_cols <- c(
  "data_channel_is_lifestyle",
  "data_channel_is_entertainment",
  "data_channel_is_bus",
  "data_channel_is_socmed",
  "data_channel_is_tech",
  "data_channel_is_world"
)

# Readable labels for each channel
channel_labels <- c("Lifestyle", "Entertainment", "Business", "Social Media", "Tech", "World")

# Some articles have no channel assigned (all six binary columns are 0).
# We filter these out using rowSums(): only rows where exactly one column equals 1 are kept.
data_ca <- data_clean %>%
  filter(rowSums(select(., all_of(channel_cols))) == 1)

# We create the categorical 'channel' column using max.col(),
# which returns the index of the column with the maximum value (i.e. the column with 1).
data_ca$channel <- channel_labels[max.col(data_ca[, channel_cols])]

# We convert the column to a factor so R treats it as a categorical variable
data_ca$channel <- factor(data_ca$channel, levels = channel_labels)

# We verify the result and inspect the distribution across channels and days
cat("Articles with an assigned channel:", nrow(data_ca), "\n")
table(data_ca$channel)
table(data_ca$day_of_week)


#---------- STEP 2: Contingency table and chi-square test ----------

# The table() function counts how many articles exist for each combination
# of day of the week (rows) and thematic channel (columns).
# The result is the contingency table, which is the starting point of the CA.
contingency_table <- table(data_ca$day_of_week, data_ca$channel)

print("Contingency table: Day of the week vs Thematic channel")
print(contingency_table)

# Before interpreting any results, we must check whether the relationship
# between the two variables is statistically significant or could be due to chance.
# We use the chi-square test (chisq.test()) for this purpose.
# H0: the two variables are independent (no relationship between day and channel)
# H1: the two variables are NOT independent (a relationship exists)
chi_test <- chisq.test(contingency_table)

print("Chi-square test result:")
print(chi_test)

# The degrees of freedom of the test are (I-1)*(J-1),
# where I is the number of rows (7 days) and J the number of columns (6 channels).
# In our case: (7-1)*(6-1) = 30 degrees of freedom.
cat("Expected degrees of freedom:", (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1), "\n")

# If the p-value is below 0.05, we reject H0 and conclude that a significant
# relationship exists between the publication day and the thematic channel.
# Only in that case does it make sense to continue with the CA.

# The expected table shows the counts we would observe if the two variables
# were completely independent. Comparing it with the observed contingency table
# reveals which day-channel combinations are over- or under-represented.
print("Expected table under independence:")
print(round(chi_test$expected, 1))


#---------- STEP 3: Row and column profiles, decomposition and factor map ----------

# Correspondence Analysis works with row profiles instead of raw counts.
# A row profile divides each cell by its row total, giving the proportion
# of each channel within each day.
# For example: of all articles published on Monday, what share is Tech? What share is World?
# This allows days to be compared on equal footing regardless of how many articles
# were published on each day.

# We compute row profiles manually to make the logic transparent before calling CA()
# margin=1 means we divide by row totals
row_profiles <- prop.table(contingency_table, margin = 1)
print("Row profiles (proportion of each channel per day):")
print(round(row_profiles, 3))

# We also compute column profiles (proportion of each day per channel),
# which show which days concentrate more articles of each channel.
# margin=2 means we divide by column totals
col_profiles <- prop.table(contingency_table, margin = 2)
print("Column profiles (proportion of each day per channel):")
print(round(col_profiles, 3))

# We now apply the CA() function from the FactoMineR package.
# This function takes the contingency table and performs the singular value
# decomposition of the high-dimensional space of profiles, creating new dimensions
# (similar to what PCA does for continuous variables).
# graph=FALSE prevents the automatic plot so we can control the output ourselves.
ca_result <- CA(contingency_table, graph = FALSE)

# The summary shows the eigenvalues (inertia per dimension), row and column coordinates,
# cosines (quality of representation) and contributions for each category.
print(summary(ca_result))

# The total inertia in CA is the equivalent of total variance in PCA.
# Each dimension explains a percentage of that inertia.
# The maximum number of dimensions is min(I-1, J-1) = min(6, 5) = 5.
# We examine how much inertia the first two dimensions explain,
# as these will be used for the graphical representation.
print("Variance explained by each dimension (inertia):")
print(ca_result$eig)


#---------- STEP 4: Graphical representation and interpretation ----------

# The factor map is the main output of the CA.
# It simultaneously represents days (rows) and channels (columns)
# in the same two-dimensional space.
# The key interpretation rule is: when a day and a channel appear close to each
# other in the plot, it means that channel is published more than expected on that day.
# Points far from the origin represent strong and characteristic associations.
# Points near the origin are "average" or uncharacteristic categories.

# fviz_ca_biplot() from the factoextra package generates the combined factor map.
# A biplot displays both rows and columns simultaneously in the same space.
fviz_ca_biplot(ca_result,
               repel = TRUE,    # repel=TRUE prevents overlapping labels
               title = "CA - Factor map: Day of publication vs Thematic channel")

# For more detail, we also plot rows and columns separately.

# Projection of days of the week only (rows)
fviz_ca_row(ca_result,
            repel = TRUE,
            title = "CA - Projection of days of the week")

# Projection of thematic channels only (columns)
fviz_ca_col(ca_result,
            repel = TRUE,
            title = "CA - Projection of thematic channels")

# Finally, we print the contributions of each row and column to the first two dimensions.
# A high contribution means that day or channel is the main driver of that dimension,
# and should be prioritised when interpreting what each dimension represents.
print("Contributions of days (rows) to the dimensions:")
print(round(ca_result$row$contrib, 3))

print("Contributions of channels (columns) to the dimensions:")
print(round(ca_result$col$contrib, 3))
