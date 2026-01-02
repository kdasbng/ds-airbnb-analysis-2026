
# -------------

library(tidyverse)
library(lubridate)
library(caret)
library(corrplot)
setwd("C:\\workdir-course\\project")

airbnb <- read.csv("airbnb_500.csv")

glimpse(airbnb)
summary(airbnb)

#fixing col names
airbnb <- airbnb %>%
  rename_with(~ str_replace_all(., "\\.", "_"))

# convert to numeric
airbnb <- airbnb %>%
  mutate(
    price = as.numeric(gsub("[$,]", "", price)),
    service_fee = as.numeric(gsub("[$,]", "", service_fee))
  )

## Handle missing value
colSums(is.na(airbnb))

airbnb <- airbnb %>%
  filter(!is.na(price))

#convert to categorical values
airbnb <- airbnb %>%
  mutate(
    room_type = as.factor(room_type),
    neighbourhood_group = as.factor(neighbourhood_group),
    instant_bookable = as.factor(instant_bookable),
    host_identity_verified = as.factor(host_identity_verified)
  )

xd <- head(airbnb, 2)
xd$room_type

#plotting data
ggplot(airbnb, aes(price)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Airbnb Prices")

ggplot(airbnb, aes(neighbourhood_group, price)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = "Price by Neighbourhood Group")


numeric_vars <- airbnb %>%
  select_if(is.numeric)

#correlation analysis
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color")

airbnb <- airbnb %>%
  mutate(
    price_per_night = price / minimum_nights,
    high_availability = ifelse(availability_365 > 200, 1, 0),
    high_review_volume = ifelse(number_of_reviews > median(number_of_reviews), 1, 0)
  )



airbnb <- airbnb %>%
  mutate(
    neighbourhood_group = case_when(
      neighbourhood_group == "brookln" ~ "Brooklyn",
      TRUE ~ neighbourhood_group
    )
  )

#create train/test split
set.seed(123)
train_index <- createDataPartition(airbnb$price, p = 0.8, list = FALSE)

train <- airbnb[train_index, ]
test <- airbnb[-train_index, ]

#build model
model <- lm(
  price ~ room_type + neighbourhood_group +
    number_of_reviews + availability_365 +
    host_identity_verified + instant_bookable,
  data = train
)

summary(model)

predictions <- predict(model, test)

RMSE(predictions, test$price, na.rm = TRUE)

mean(test$price, na.rm = TRUE)
