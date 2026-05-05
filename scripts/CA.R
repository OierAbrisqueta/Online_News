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

#----------------------------
# CA: Correspondence Analysis
#----------------------------

#------------ PASO 1: Crear la variable de canal temático

# Definimos los nombres de las columnas de canal
channel_cols <- c(
  "data_channel_is_lifestyle",
  "data_channel_is_entertainment",
  "data_channel_is_bus",
  "data_channel_is_socmed",
  "data_channel_is_tech",
  "data_channel_is_world"
)

# Definimos las etiquetas que queremos mostrar
channel_labels <- c("Lifestyle", "Entertainment", "Business", "Social Media", "Tech", "World")

data_ca <- data_clean %>%
  filter(rowSums(select(., all_of(channel_cols))) == 1)

# Crear la columna categórica 'channel' usando max.col()
data_ca$channel <- channel_labels[max.col(data_ca[, channel_cols])]

data_ca$channel <- factor(data_ca$channel, levels = channel_labels)

cat("Articulos con canal asignado:", nrow(data_ca), "\n")
table(data_ca$channel)
table(data_ca$day_of_week)

#------------ PASO 2: tabla de contingencia y test chi-cuadrado

contingency_table <- table(data_ca$day_of_week, data_ca$channel)

print("Tabla de contingencia: Dia de la semana vs Canal temático")
print(contingency_table)

# Antes de interpretar hay que comprobar si la relacion entre las variables es significativa o se da al azar
# H0: las variables son independientes
# H1: las variables no son independientes
chi_test <- chisq.test(contingency_table)

print("Resultado del test:")
print(chi_test)

cat("Grados de libertad esperados:", (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1), "\n")

# Si el p-valor es < 0.05, rechazamos H0 y concluimos que existe una relacion significativa
