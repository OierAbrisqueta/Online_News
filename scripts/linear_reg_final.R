library(dplyr)
library(ggplot2)

setwd("./Online_News/data")

data <- read.csv("OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

quantile(data$shares,c(0.05,0.25,0.5,0.75,0.95,0.995))

data_clean <- data
nrow(data_clean)


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




colnames(data_clean)

#----------------------------------------------------------------
# MODEL : Predict log-shares based on content of the article (popularity , subjectivity...)
#----------------------------------------------------------------

model_predictors <- data_clean %>% select(contains("token"),contains("content",),
                                               contains("subjectivity"),contains("rate"),contains("polarity")) %>% colnames()

model_predictors

data_model <- data_clean %>% select(log_shares,contains("token"),contains("content",),
                                    contains("subjectivity"),contains("rate"),contains("polarity"))

cat("Number of observations available: " ,nrow(data_model))

#Exploration of correlation between selected variables and the target , log_shares

cor_model <- sapply(model_predictors , function(v){
  cor(data_model[[v]] , data_model$log_shares,use = "complete.obs")
})

cor_model

#Exploration of correlation between predictors 

correlation_matrix <- cor(data_model[,model_predictors],use="complete.obs")
print(correlation_matrix)

high_cor_pairs <- which(abs(correlation_matrix) > 0.8 & 
                          upper.tri(correlation_matrix), 
                        arr.ind = TRUE)

high_cor_df <- data.frame(
  Variable_1 = rownames(correlation_matrix)[high_cor_pairs[, 1]],
  Variable_2 = colnames(correlation_matrix)[high_cor_pairs[, 2]],
  Correlation = correlation_matrix[high_cor_pairs]
) %>%
  arrange(desc(abs(Correlation)))

print(high_cor_df)

#We observe that rate_positive_words and rate_negative_words have an extreme correlation (>0.99)
#We evaluate now the correlation of each of them with the target to decide which to maintain

cor(data_clean$rate_positive_words,data_clean$log_shares)
cor(data_clean$rate_negative_words,data_clean$log_shares)

# Rate_positive_words have a better correlation so we delete rate_negative_words from the model

data_model <- data_model %>% select(-rate_negative_words)


# SPLIT TRAIN / TEST (70-30)

set.seed(124)
n <- nrow(data_model)
index <- sample(1:n, size = 0.7*n)

data_model_train <- data_model[index,]
data_model_test <- data_model[-index,]

# COMPLETE MODEL BUILDING - F TEST

model.1 <- lm(log_shares~.,data=data_model_train)

model.1.s <- summary(model.1)
model.1.s

# The p-value is very small (< 2.2e-16) which means that we 
#reject H0 : none of the predictor tells relevant information so we conclude 
# that there is a significant linear relationship between log_shares and at
#least one of the predictors 

# We are now going to test each of the predictor to see its relevance (T TEST)
# Info : we are using alpha = 0.05 

model.1.s 

#In the summary we can see the t-value and the corresponding probability of
# |Tstat| > t_alpha.

# BACKWARD ELIMINATION

#We are now going to order the p-values in descending order to update the model 
# without the variable with the highest p-value. We will repeat this until all
#variables have p-values below 0.05.

p_value_order <- sort(model.1.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is max_positive_polarity

model.2 <- lm(log_shares~.-max_positive_polarity,data=data_model_train)
model.2.s <- summary(model.2)
model.2.s

anova(model.2,model.1) #We use the anova function to compare models.
#The p-value is very high so we do not reject H0 : both model are equivalents.
#As a result, we continue with the reduced model

p_value_order <- sort(model.2.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is max_negative_polarity

model.3 <- lm(log_shares~.-max_negative_polarity-max_positive_polarity,data=data_model_train)
model.3.s <- summary(model.3)
model.3.s

anova(model.3,model.2) #p-value = 0.6407 > 0.05 , we use the reduced model.

p_value_order <- sort(model.3.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is title_subjectivity

model.4 <- lm(log_shares~.-max_negative_polarity-max_positive_polarity-title_subjectivity,data=data_model_train)
model.4.s <- summary(model.4)
model.4.s

anova(model.4,model.3) #p-value 0.3752 > 0.05 , we use the reduced model

p_value_order <- sort(model.4.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is avg_negative_polarity

model.5 <- lm(log_shares~.-max_negative_polarity-max_positive_polarity-title_subjectivity-avg_negative_polarity,data=data_model_train)
model.5.s <- summary(model.5)
model.5.s

anova(model.5,model.4) #p-value 0.3348 > 0.05 , we use the reduced model

p_value_order <- sort(model.5.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is min_negative_polarity

model.6 <- lm(log_shares~.-max_negative_polarity-max_positive_polarity-min_negative_polarity-avg_negative_polarity-title_subjectivity,data=data_model_train)
model.6.s <- summary(model.6)
model.6.s

anova(model.6,model.5) #p-value 0.5646 > 0.05 , we use the reduced model 

p_value_order <- sort(model.6.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is rate_positive_words

model.7<- lm(log_shares~.-max_negative_polarity-max_positive_polarity-min_negative_polarity-avg_negative_polarity-title_subjectivity-rate_positive_words,data=data_model_train)
model.7.s <- summary(model.7)
model.7.s

anova(model.7,model.6) #p-value 0.06951 > 0.05 , we use the reduced model 

p_value_order <- sort(model.7.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is global_sentiment_polarity

model.8<- lm(log_shares~.-global_sentiment_polarity-max_negative_polarity-max_positive_polarity-min_negative_polarity-avg_negative_polarity-title_subjectivity-rate_positive_words,data=data_model_train)
model.8.s <- summary(model.8)
model.8.s

anova(model.8,model.7) #p-value 0.4431 > 0.05 , we use the reduced model

p_value_order <- sort(model.8.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is min_positive_polarity

model.9<- lm(log_shares~.-min_positive_polarity-global_sentiment_polarity-max_negative_polarity-max_positive_polarity-min_negative_polarity-avg_negative_polarity-title_subjectivity-rate_positive_words,data=data_model_train)
model.9.s <- summary(model.9)
model.9.s

anova(model.9,model.8) #p-value 0.07089 > 0.05 , we use the reduced model

p_value_order <- sort(model.9.s$coefficients[,4],decreasing = T)
p_value_order #Highest one is avg_positive_polarity

model.10<- lm(log_shares~.-avg_positive_polarity-min_positive_polarity-global_sentiment_polarity-max_negative_polarity-max_positive_polarity-min_negative_polarity-avg_negative_polarity-title_subjectivity-rate_positive_words,data=data_model_train)
model.10.s <- summary(model.10)
model.10.s

anova(model.10,model.9) #p-value 0.1738 > 0.05 , we use the reduced model

p_value_order <- sort(model.10.s$coefficients[,4],decreasing = T)
p_value_order #All variables have p-values below 0.05 so we have finished backward elimination


model.final <- model.10
model.final.s <- summary(model.final)
model.final.s

# RESPONSE PREDICTION : CONFIDENCE & PREDICTION INTERVALS

# We compute the confidence interval for the coefficients of all the predictors for alpha = 0.05

confint(model.final)

#The confidence interval analysis (95%) reveals that the majority of our predictors are statistically 
#significant, as their ranges do not include zero, confirming they have an impact on log_shares.
#global_subjectivity stands out as the strongest positive driver, while structural features like n_tokens_title and 
#n_tokens_content show a consistent but small negative effect. 


conf_inter <- data.frame(log_shares = data_model_train$log_shares)
conf_inter <- cbind(conf_inter,as.data.frame(predict(model.final,data_model_train,
                                                     interval="confidence",level=0.95)))
conf_inter$error_distance <- ifelse(
  conf_inter$log_shares >= conf_inter$lwr & conf_inter$log_shares <= conf_inter$upr, 
  0, pmin(abs(conf_inter$log_shares - conf_inter$lwr), abs(conf_inter$log_shares - conf_inter$upr))
)

conf_inter

cat("The mean error in log_shares measurement the model did with the train set was : " ,mean(conf_inter$error_distance))

predict_inter <- data.frame(log_shares = data_model_test$log_shares)
predict_inter <- cbind(predict_inter,as.data.frame(predict(model.final,data_model_test,
                                                           interval="prediction")))
predict_inter

predict_inter$error_distance <- ifelse(
  predict_inter$log_shares >= predict_inter$lwr & predict_inter$log_shares <= predict_inter$upr, 
  0, pmin(abs(predict_inter$log_shares - predict_inter$lwr), abs(predict_inter$log_shares - predict_inter$upr))
)

cat("The mean error in log_shares measurement the model did with the test set was : " ,mean(predict_inter$error_distance))

#MODEL DIAGNOSIS

#We perform a quick diagnosis to confirm everything is correct
plot(model.final,3)

#We observe that our model suffers of heteroscedasticity which
#occurs when there is a non-constant variance of errors. In our case,
# it is clearly visible that as fitted values increase , the dispersion 
# of errors also increase , which is a negative aspect.

plot(model.final,2)

#With the Q-Q residuals plot we are assessing normality of erros.
#The S-shape of the graph evidentiates that we have more outliers
#than the ones expected by a normal distribution.The problem with this
# is that the model is very sensible to "strange" values. We then conclude
# the non-normality of errors. We ignore using the shapiro test as our dataset
# is very large.


plot(model.final,1)

#With the residuals vs fitted graph we are trying to detect non-linearity 
# in the data by studying the erros . If the linear assumption makes sense ,
#residuals should be randomly scattered around zero. However , in our case , 
#point are more grouped in the center and are less dispersed in the extremes.
#This reinforces the idea previously uncovered , our model´s variance is not
#constant and tend to predict better common values rather than extreme ones.

plot(model.final,4)

#The Cook´s distances graph is used to identify high-leverage points in the data
#In our case , we have three clear ones : 17043,31741,6132.To check if this points
#have a greater influence on data than the one expected we use the Residuals vs
#Leverage plot.

plot(model.final,5)

#We see that those high-leverage points are actually in the left part of the X-axis
# with less than 0.007 actual leverage so , although they are high errors , they are
#not influential at all and are not pushing the line. The conclusion
# is that is not worth eliminating these points.



