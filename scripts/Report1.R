
library(dplyr)
library(ggplot2)

setwd("./data")

data <- read.csv("OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

# SHARES
boxplot(data$shares)
summary(data$shares)
quantile(data$shares, probs = c(0.25, 0.5, 0.75, 0.95))

# Nos quedamos con el 95%
data_clean <- data %>%
  filter(shares<=10800)

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

data_clean <- data_clean %>% select(-n_unique_tokens)

#N_unique_tokens and n_non_stop_unique_tokens carry redundant information as it is seen by using the correlation of variables.
# By comparing the two columns , we see that the different between the these two columns is very similar in all the observations 
# The result is to eliminate n_unique_tokens since it includes separators or "stop words" 



ggplot(data_clean, aes(x = day_of_week, y = log_shares, fill = day_of_week)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Share distribution by weekday")

#To introduce the possibility of doing a linear model using as predictor the day of the week we use
# this grouped boxplot to see the difference in shares between the different weekdays
# As it is seen , there is a significant difference between weekends and the other days.