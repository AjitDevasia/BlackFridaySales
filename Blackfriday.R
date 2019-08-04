#Loading Libraries
library(tidyverse)
library(dplyr)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("data.table")
library(data.table)
install.packages("xda")
library(ggplot2)
install.packages("vcd")
library(vcd)
install.packages("rpart")
library(rpart)

#---------------------------------#

#Load the Dataset
blackf_data <-read.csv("train.csv")

#Step 1: Data Profiling
# Size of the dataset
object.size(blackf_data)/10^6

#How many Rows & Columns
dim(blackf_data)

#Structure of Dataset
str(blackf_data)
PlotStr(blackf_data)

# Checking for NAs across the dataset
TrainBF %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

#--------------------------------------#
#Continous Vs Discrete Features
#Discrete variables
prop.table(table(blackf_data$Marital_Status))

 
ggplot(data = blackf_data, 
       mapping = aes(x = blackf_data$Marital_Status, fill = as.factor(blackf_data$Marital_Status))) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..)/sum(..count..)),
                          y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Marital Status", 
       y = "Male Vs Female",
       fill = "married") 
# How many male and female?
prop.table(table(blackf_data$Gender))
ggplot(blackf_data,aes(x=blackf_data$Gender, fill = blackf_data$Gender)) + 
  geom_bar(colour="blue")
# Males contribute to 75% of the dataset

#How many people in each age groups?
prop.table(table(blackf_data$Age))
ggplot(blackf_data,aes(x=blackf_data$Age, fill = blackf_data$Age)) + 
  geom_bar(colour="blue")
# Age group between 26-35 constitute the highest no. of shoppers (nearly 40%)
#Occupation List?
prop.table(table(blackf_data$Occupation))

# City Category? Which city category contributes maximum?

ggplot(blackf_data,aes(x=blackf_data$City_Category, fill = blackf_data$City_Category)) + 
  geom_bar(colour="blue")
prop.table(table(blackf_data$City_Category))
# City B contributes 42% of shoppers

# Stay in Current Cities Analysis:
ggplot(blackf_data,aes(x=blackf_data$Stay_In_Current_City_Years, fill = blackf_data$Stay_In_Current_City_Years)) + 
  geom_bar(colour="blue") 
  prop.table(table(blackf_data$Stay_In_Current_City_Years)) 
  
# People staying for a year contribute to the shopping list
  
### Transforming the Dataset
  
  blackf_data$User_ID <- as.factor(blackf_data$User_ID)
  blackf_data$Product_ID <- as.factor(blackf_data$Product_ID)
  blackf_data$Gender <- as.factor(ifelse(blackf_data$Gender == 'M', 'Male', 'Female'))
  blackf_data$Age <- as.factor(blackf_data$Age)
  blackf_data$Occupation <- as.factor(blackf_data$Occupation)
  blackf_data$City_Category <- as.factor(blackf_data$City_Category)
  blackf_data$Stay_In_Current_City_Years <- as.factor(blackf_data$Stay_In_Current_City_Years)
  blackf_data$Marital_Status <- as.factor(ifelse(blackf_data$Marital_Status == 1, 'Married', 'Single'))
  blackf_data$Product_Category_1 <- as.integer(blackf_data$Product_Category_1)
  blackf_data$Product_Category_2 <- as.integer(blackf_data$Product_Category_2)
  blackf_data$Product_Category_3 <- as.integer(blackf_data$Product_Category_3)
  blackf_data$Purchase <- as.numeric(blackf_data$Purchase)
  
  str(blackf_data)
## Imputing Values in NAs
  
  fit <-  rpart(Product_Category_2 ~ User_ID + Product_ID + Age + Gender,
                data = blackf_data[!is.na(blackf_data$Product_Category_2),],
                method = "anova")
  blackf_data$Product_Category_2[is.na(blackf_data$Product_Category_2)] <-
    predict(fit, blackf_data[is.na(blackf_data$Product_Category_2),])

  fit1 <- rpart(Product_Category_3 ~ User_ID + Product_ID + Age + Gender,
                data = blackf_data[!is.na(blackf_data$Product_Category_3),],
                method = "anova")
  blackf_data$Product_Category_3[is.na(blackf_data$Product_Category_3)] <-
    predict(fit1, blackf_data[is.na(blackf_data$Product_Category_3),])
# Check for missing values
  
  blackf_data %>%
    select(everything()) %>%
    summarise_all(funs(sum(is.na(.))))
  
  ###-----------Exploratory Data Analysis----------------------###

  # How many unique customers?
  length(unique(blackf_data$User_ID))
#5891 unique customers
  
  #How many items did each customer purchase ?
  Unique_userid <- as.data.frame(table(blackf_data$User_ID))
 names(Unique_userid) <- c("Unique User ID", " Purchase")  
 head(Unique_userid)
## Group by Age, Gender, Occupation, City Category
 new_data <- blackf_data %>%   
   group_by(User_ID, Age, Gender, Occupation, City_Category, Stay_In_Current_City_Years, Marital_Status) %>% 
   summarise_each(funs(mean), Product_Category_1, Product_Category_2, Product_Category_3, Purchase) 
 
 # Name the columns to average
 colnames(new_data)[8]<- "Product_Cat_1_Avg"
 colnames(new_data)[9]<- "Product_Cat_2_Avg"
 colnames(new_data)[10]<- "Product_Cat_3_Avg"
 colnames(new_data)[11]<- "Avg_Purchase_Amount"
 
 # Peek into new_data
 head(new_data)
 
 #Explore Age and Gender Variables along with product categories and purchase amount
 #Product Category1
 ggplot(new_data, aes(Gender, Product_Cat_1_Avg, fill = Gender)) + geom_col(width = 0.4) + facet_wrap(~ Age) + 
   labs(title = "Age Group/Gender Vs Product Category 1")
 
 #Product Category2
 ggplot(new_data, aes(Gender, Product_Cat_2_Avg, fill = Gender)) + geom_col(width = 0.4) + facet_wrap(~ Age) + 
   labs(title = "Age Group/Gender Vs Product Category 2")
 #Product Category3
 ggplot(new_data, aes(Gender, Product_Cat_3_Avg, fill = Gender)) + geom_col(width = 0.4) + facet_wrap(~ Age) + 
   labs(title = "Age Group/Gender Vs Product Category 3")
 #Average Purchase Amount
 ggplot(new_data, aes(Gender, Avg_Purchase_Amount, fill = Gender)) + geom_col(width = 0.4) + facet_wrap(~ Age) + 
   labs(title = "Age Group/Gender Vs Avg_Purchase_Amount")
 
 #Applying Regression
 
 ins_model <-lm(Purchase ~ .,data = blackf_data)