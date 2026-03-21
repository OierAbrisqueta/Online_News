library(dplyr)

data <- read.csv("./data/OnlineNewsPopularity.csv", strip.white=T)

head(data)
summary(data)

# SHARES
boxplot(data$shares)
summary(data$shares)
quantile(data$shares, probs = c(0.25, 0.5, 0.75, 0.95))

# Nos quedamos con el 95%
data_clean <- data %>%
  filter(shares<10800)

# En el histograma podemos ver que es asimetrico (right skew) porque la media es mayor a la mediana.
boxplot(data_clean$shares)
hist(data_clean$shares)

d <- density(data_clean$shares)

plot(d)


# CATEGORICAL DATA
# variables de tipos de data channel
data_channel_types <- c('data_channel_is_lifestyle', 'data_channel_is_entertainment', 
                        'data_channel_is_bus', 'data_channel_is_socmed', 
                        'data_channel_is_tech', 'data_channel_is_world')


data_clean$channel <- names(data_clean[, data_channel_types])[max.col(data_clean[, data_channel_types])]

data_clean$channel <- gsub("data_channel_is_", "", data_clean$channel)

data_clean$channel <- as.factor(data_clean$channel)
data_clean$channel <- gsub("bus", "business", data_clean$channel)

data_clean$channel <- as.factor(data_clean$channel)

unique(data_clean$channel)
class(data_clean$channel)

data_clean <- data_clean[, !(names(data_clean) %in% data_channel_types)]

head(data_clean)

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
