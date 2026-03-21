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
    filter(shares<11000)

boxplot(data_clean$shares)
hist(data_clean$shares)
