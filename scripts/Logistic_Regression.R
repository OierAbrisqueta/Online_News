library(dplyr)
library(ggplot2)
library(pROC)
library(glmtoolbox)
library(splines)

setwd("./data")

data <- read.csv("OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

data_clean <- data 

# En el histograma podemos ver que es asimetrico (right skew) porque la media es mayor a la mediana.
boxplot(data_clean$shares)
hist(data_clean$shares)

d <- density(data_clean$shares)

plot(d)


# CATEGORICAL DATA

# Variables de días de la semana
weekdays <- c(
  'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
  'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday',
  'weekday_is_sunday'
)

# Crear una columna con categorica para el dia de la semana que sea
weekday_labels <- c(
  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
  'Saturday', 'Sunday'
)

data_clean <- data

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




#LOGISTIC MODEL

#We create a new column that representes virality
#We stablish virality as the top 25% of shares.
viral_cut <- quantile(data_clean$shares, 0.75, na.rm = TRUE)

model_data <- data_clean %>% 
  mutate(is_viral = ifelse(shares >= viral_cut, 1, 0))
model_data$is_viral <- factor(model_data$is_viral, levels = c(0,1))

summary(model_data)
prop.table(table(model_data$is_viral))


#EXPLORATION
#Before creating the model we do a pre-exploration of the data

ggplot(model_data, aes(x = is_viral, y = num_imgs, fill = is_viral)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  theme_minimal() +
  labs(title = "Number of images by virality class", x = "", y = "num_imgs")

ggplot(model_data, aes(x = is_viral, y = num_videos, fill = is_viral)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  theme_minimal() +
  labs(title = "Number of videos by virality class", x = "", y = "num_videos")

ggplot(model_data, aes(x = is_viral, y = kw_avg_avg, fill = is_viral)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  theme_minimal() +
  labs(title = "kw_avg_avg by virality class", x = "", y = "kw_avg_avg")


#VARIABLE SELECTION
#Once the initial exploration of the data we are analizing is over,
#we select the variables that are going to be studied.

#Model A: just takes "superficial" main variables we want to understand
modelA_variables <- c("is_viral", "num_imgs", "num_videos", "kw_avg_avg")

#Model B: uses as well other columns that can have an influence.
#This control variables will not only improve the performance of the model
#but they will as well give us a more real understanding of the variables we are focusing the model in.
modelB_control_variables <- c(
  "n_tokens_content",
  "n_non_stop_unique_tokens",
  "num_hrefs",
  "num_self_hrefs",
  "average_token_length",
  "self_reference_avg_shares",
  "global_subjectivity",
  "global_sentiment_polarity",
  "rate_positive_words",
  "rate_negative_words",
  "day_of_week",
  "data_channel_is_lifestyle",
  "data_channel_is_entertainment",
  "data_channel_is_bus",
  "data_channel_is_socmed",
  "data_channel_is_tech",
  "data_channel_is_world"
)

#Model b contains both the control variables and the variables we want to understand
modelB_variables <- c(modelA_variables, modelB_control_variables)

#We are using the same dataframe for both models because the comparison should be made over the same data
model_df <- model_data %>%
  select(all_of(unique(modelB_variables))) %>%
  na.omit()


#SPLIT TRAIN / TEST (70-30)

set.seed(123)

#For this division we use the Stratified Sampling technique as we want to balance
#viral and non viral in the split.
index_viral <- which(model_df$is_viral == 1)
index_nonviral <- which(model_df$is_viral == 0)

train_viral <- sample(index_viral, size = floor(0.7 * length(index_viral)))
train_nonviral <- sample(index_nonviral, size = floor(0.7 * length(index_nonviral)))

index_train <- c(train_viral, train_nonviral)
index_test <- setdiff(seq_len(nrow(model_df)), index_train)

train <- model_df[index_train,]
test <- model_df[index_test,]

#We apply as well standardisation technique to have for each numeric predictor
#mean 0 and st deviation 1. This is useful for the comparability of odds ratios
#because without it the odds ratios of predictors would be incomparable.
num_cols <- names(train)[sapply(train, is.numeric)]
num_cols <- setdiff(num_cols, "is_viral")

train_means <- sapply(train[, num_cols, drop = FALSE], mean, na.rm = TRUE)
train_sds   <- sapply(train[, num_cols, drop = FALSE], sd, na.rm = TRUE)

#We avoid dividing by 0.
train_sds[train_sds == 0] <- 1

train[, num_cols] <- scale(train[, num_cols, drop = FALSE],
                              center = train_means, scale = train_sds)
test[, num_cols]  <- scale(test[, num_cols, drop = FALSE],
                              center = train_means, scale = train_sds)

#MODEL CREATION
#At the beggining the plan was to create 2 models. One was just intended to have the media and keyword variables 
#we want to completely understand and the other one as well variables that could have an influence on shares.
#Nevertheless, while working in the models we have realized that both models showed a poor global calibration.
#Therefore, we introduced nonlinearity in the continuous predictors using splines, because the relation
#between these variables and the log odds of virality is not strictly linear. In conclusion, we finally work
#with 4 models.

#MODEL A
model_A <- glm(formula = is_viral ~ num_imgs + num_videos + kw_avg_avg, data = train, family = binomial())

print("SUMMARY MODEL A")
summary(model_A)

#G statistic and p value of the observed statistic
g_A <- model_A$null.deviance - model_A$deviance
p_A <- pchisq(g_A, df = model_A$df.null - model_A$df.residual, lower.tail = FALSE)
cat("G statistic Model A:", g_A, " p-value:", p_A, "\n")

#Hosmer-Lemeshow test - Ctd
hltest(model_A)


#MODEL B
model_B <- glm(formula = is_viral ~ ., data = train, family = binomial())

print("SUMMARY MODEL B")
summary(model_B)

#G statistic and p value of the observed statistic
g_B <- model_B$null.deviance - model_B$deviance
p_B <- pchisq(g_B, df = model_B$df.null - model_B$df.residual, lower.tail = FALSE)
cat("G statistic Model B:", g_B, " p-value:", p_B, "\n")

#Hosmer-Lemeshow test - Ctd
hltest(model_B)

#Apply nonlinearity on continuous variables


#MODEL A WITH SPLINES
model_A_nonlinearity <- glm(formula = is_viral ~ num_imgs + num_videos + ns(kw_avg_avg, df = 3), data = train, family = binomial())

print("SUMMARY MODEL A")
summary(model_A_nonlinearity)

#G statistic and p value of the observed statistic
g_A <- model_A_nonlinearity$null.deviance - model_A_nonlinearity$deviance
p_A <- pchisq(g_A, df = model_A_nonlinearity$df.null - model_A_nonlinearity$df.residual, lower.tail = FALSE)
cat("G statistic Model A:", g_A, " p-value:", p_A, "\n")

#Hosmer-Lemeshow test - Ctd
hltest(model_A_nonlinearity)


#MODEL B WITH SPLINES
model_B_nonlinearity <- glm(
  is_viral ~ num_imgs +
    num_videos +
    ns(kw_avg_avg, df = 3) +
    ns(n_tokens_content, df = 3) +
    ns(n_non_stop_unique_tokens, df = 3) +
    num_hrefs +
    num_self_hrefs +
    ns(average_token_length, df = 3) +
    self_reference_avg_shares +
    ns(global_subjectivity, df = 3) + 
    ns(global_sentiment_polarity, df = 3) + 
    ns(rate_positive_words, df = 3) + 
    ns(rate_negative_words, df = 3) +
    day_of_week +
    data_channel_is_lifestyle +
    data_channel_is_entertainment +
    data_channel_is_bus +
    data_channel_is_socmed +
    data_channel_is_tech +
    data_channel_is_world,
  data = train,
  family = binomial()
)

print("SUMMARY MODEL B")
summary(model_B_nonlinearity)

#G statistic and p value of the observed statistic
g_B <- model_B_nonlinearity$null.deviance - model_B_nonlinearity$deviance
p_B <- pchisq(g_B, df = model_B_nonlinearity$df.null - model_B_nonlinearity$df.residual, lower.tail = FALSE)
cat("G statistic Model B:", g_B, " p-value:", p_B, "\n")

#Hosmer-Lemeshow test - Ctd
hltest(model_B_nonlinearity)


#MODEL EVALUATION: AUC, ROC and Metrics

#Generate probability predictions for the test set
pred_A <- predict(model_A, newdata = test, type = "response")
pred_A_splines <- predict(model_A_nonlinearity, newdata = test, type = "response")
pred_B <- predict(model_B, newdata = test, type = "response")
pred_B_splines <- predict(model_B_nonlinearity, newdata = test, type = "response")

#Calculate ROC and AUC
roc_A <- roc(as.numeric(as.character(test$is_viral)), pred_A)
roc_A_splines <- roc(as.numeric(as.character(test$is_viral)), pred_A_splines)
roc_B <- roc(as.numeric(as.character(test$is_viral)), pred_B)
roc_B_splines <- roc(as.numeric(as.character(test$is_viral)), pred_B_splines)

cat("\nAUC Model A:", auc(roc_A), "\n")
cat("AUC Model A with splines:", auc(roc_A_splines), "\n")
cat("AUC Model B:", auc(roc_B), "\n")
cat("AUC Model B with splines:", auc(roc_B_splines), "\n")

#Plot ROC Curves so that models can be compared
plot(roc_A, col = "lightblue", main = "ROC Curves")
plot(roc_A_splines, col = "blue", add = TRUE)
plot(roc_B, col = "red", add = TRUE)
plot(roc_B_splines, col = "lightcoral", add = TRUE)
legend("bottomright",
       legend = c(paste0("Model A AUC=", round(auc(roc_A), 3)),
                  paste0("Model A Spline AUC=", round(auc(roc_A_splines), 3)),
                  paste0("Model B AUC=", round(auc(roc_B), 3)),
                  paste0("Model B Spline AUC=", round(auc(roc_B_splines), 3))),
       col = c("lightblue", "blue", "red", "lightcoral"), lwd = 2)

#We create a function to calculate the confusion matrix, accuracy, sensitivity and specificity.
get_metrics <- function(y_true_factor, y_prob, threshold = 0.5) {
  y_true <- as.numeric(as.character(y_true_factor))
  y_pred <- ifelse(y_prob >= threshold, 1, 0)
  
  TP <- sum(y_pred == 1 & y_true == 1)
  TN <- sum(y_pred == 0 & y_true == 0)
  FP <- sum(y_pred == 1 & y_true == 0)
  FN <- sum(y_pred == 0 & y_true == 1)
  
  accuracy    <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  list(
    confusion_matrix = matrix(c(TN, FN, FP, TP), nrow = 2, byrow = TRUE,
                              dimnames = list("Predicted" = c("0", "1"),
                                              "Actual"    = c("0", "1"))),
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity
  )
}

metrics_A <- get_metrics(test$is_viral, pred_A, threshold = 0.25)
metrics_A_spline <- get_metrics(test$is_viral, pred_A_splines, threshold = 0.25)
metrics_B <- get_metrics(test$is_viral, pred_B, threshold = 0.25)
metrics_B_spline <- get_metrics(test$is_viral, pred_B_splines, threshold = 0.25)

print("Metrics Model A")
print(metrics_A$confusion_matrix)
cat("Accuracy:", round(metrics_A$accuracy, 4), "\n")
cat("Sensitivity:", round(metrics_A$sensitivity, 4), "\n")
cat("Specificity:", round(metrics_A$specificity, 4), "\n")

print("Metrics Model A Splines")
print(metrics_A_spline$confusion_matrix)
cat("Accuracy:", round(metrics_A_spline$accuracy, 4), "\n")
cat("Sensitivity:", round(metrics_A_spline$sensitivity, 4), "\n")
cat("Specificity:", round(metrics_A_spline$specificity, 4), "\n")

print("Metrics Model B")
print(metrics_B$confusion_matrix)
cat("Accuracy:", round(metrics_B$accuracy, 4), "\n")
cat("Sensitivity:", round(metrics_B$sensitivity, 4), "\n")
cat("Specificity:", round(metrics_B$specificity, 4), "\n")

print("Metrics Model B Splines")
print(metrics_B_spline$confusion_matrix)
cat("Accuracy:", round(metrics_B_spline$accuracy, 4), "\n")
cat("Sensitivity:", round(metrics_B_spline$sensitivity, 4), "\n")
cat("Specificity:", round(metrics_B_spline$specificity, 4), "\n")


#ODDS RATION

#Function that calculates odds ratios for a given model
or_ci <- function(model) {
  sum_tab <- summary(model)$coefficients
  valid_terms <- rownames(sum_tab)
  ci <- suppressMessages(confint.default(model))
  ci <- ci[valid_terms, , drop = FALSE]
  
  out <- data.frame(
    term = valid_terms,
    OR = exp(sum_tab[, 1]),         
    CI_low = exp(ci[, 1]),          
    CI_high = exp(ci[, 2]),         
    p_value = sum_tab[, 4]          
  )
  
  rownames(out) <- NULL
  return(out)
}

or_A <- or_ci(model_A)
or_A_splines <- or_ci(model_A_nonlinearity)
or_B <- or_ci(model_B)
or_B_splines <- or_ci(model_B_nonlinearity)

#We only study key variables, the study focuses on media and keywords
#the rest of the variables have been used as control variables
or_A_key <- subset(or_A, grepl("num_imgs|num_videos|kw_avg_avg", term))
or_A_key_splines <- subset(or_A_splines, grepl("num_imgs|num_videos|kw_avg_avg", term))
or_B_key <- subset(or_B, grepl("num_imgs|num_videos|kw_avg_avg", term))
or_B_key_splines <- subset(or_B_splines, grepl("num_imgs|num_videos|kw_avg_avg", term))

print("Odds Ratios of the variables that are being studied")
print("Odds Ratios of the model A")
print(or_A_key)
print("Odds Ratios of the model A splines")
print(or_A_key_splines)
print("Odds Ratios of the model B")
print(or_B_key)
print("Odds Ratios of the model B splines")
print(or_B_key_splines)