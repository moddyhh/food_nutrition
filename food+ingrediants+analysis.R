#importing important libraries 
library(ggplot2)
library(dplyr)
library(readr)
library(moments)


#importing dataset
food_dataset <- read.csv(file.choose())
View(food_dataset)

#getting the summary of a dataset
summary(food_dataset)

#getting to know the dataset 
skimr::skim_without_charts(food_dataset)

#dealing with missing values in a dataset
for (column in colnames(food_dataset)){
  #storing the missing values first
  missing_values <- is.na(food_dataset[, column]) 
  
  #filling missing values in their respective columns using median
  food_dataset[missing_values, column] <- median(food_dataset[!missing_values, column])
}


#looking for changes made
View(food_dataset)
skimr::skim_without_charts(food_dataset)


#distribution of calories , carbohydrate , protein , total fats in foods 
ggplot(data = food_dataset, aes(x = calories_J)) + geom_histogram()
ggplot(data = food_dataset, aes(x = carbohydrate_g)) + geom_histogram()
ggplot(data = food_dataset, aes(x = protein_g)) + geom_histogram()
ggplot(data = food_dataset, aes(x = total_fat_g)) + geom_histogram()
ggplot(data = food_dataset, aes(x = fiber_g)) + geom_histogram()
ggplot(data = food_dataset, aes(x = sugars_g)) + geom_histogram()
ggplot(data = food_dataset, aes(x = water_g)) + geom_histogram()


#running a correlation test to assess the influence of nutrients on the food calories 
cor.test(food_dataset$total_fat_g , food_dataset$calories_J)
cor.test(food_dataset$carbohydrate_g , food_dataset$calories_J)
cor.test(food_dataset$protein_g, food_dataset$calories_J)
cor.test(food_dataset$water_g, food_dataset$calories_J)


#identifying foods with zero calories
zero_calories <- subset(food_dataset, calories_J < 1)
View(zero_calories)

#identifying foods with calories above the average
above_calories <- subset(food_dataset, calories_J > mean(food_dataset[ , "calories_J"]))
View(above_calories)

#identifying foods with calories below the average
below_calories <- subset(food_dataset, calories_J < mean(food_dataset[, "calories_J"]))
View(below_calories)












