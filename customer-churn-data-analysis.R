
# STEP 1: DATA DESCRIPTION
# ------------------------
#######################################################################################################
#                                                                                                     #
# The “Telco Customer Churn” dataset was downloaded from Kaggle.com. The dataset provides information #
# about the customers, the various services that they have opted, their account information and       #
# demographic details. This dataset can be used to predict behavior to retain customers. You can      #
# analyze all relevant customer data and develop focused customer retention programs.                 #
#                                                                                                     #
#######################################################################################################

install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("purr")
install.packages("caret")
install.packages("skimr")
install.packages("rpart.plot")
install.packages("readr")
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)
library(tibble)
library(purrr)
library(caret)
library(skimr)
library(rpart.plot)
library(readr)

#load the downloaded csv dataset into R using readr library
churn_data <- read.csv("/Users/lavanyagovindarajan/Documents/CSDA/R/finalProject/WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, sep = ",")

#######################################################################################################
#                                                                                                     #
#     This dataset contains about 7043 observations and 21 fields.                                    #
#     Out of which 4 are numeric and 17 are character data type.                                      #
#                                                                                                     #
#######################################################################################################
#customerID               :	Randomized Customer ID
#gender                   : Whether the customer is a male or a female
#Partner                  : Whether the customer has a partner or not (Yes, No)
#Dependents               :	Whether the customer has dependents or not (Yes, No)
#PhoneService	            : Whether the customer has a phone service or not (Yes, No)
#MultipleLines            : Whether the customer has multiple lines or not (Yes, No, No phone service)
#InternetService          :	Customer’s internet service provider (DSL, Fiber optic, No)
#OnlineSecurity           :	Whether the customer has online security or not (Yes, No, No internet service)
#OnlineBackup             :	Whether the customer has online backup or not (Yes, No, No internet service)
#DeviceProtection         :	Whether the customer has device protection or not (Yes, No, No internet service)
#TechSupport              :	Whether the customer has tech support or not (Yes, No, No internet service)
#StreamingTV              :	Whether the customer has streaming TV or not (Yes, No, No internet service)
#StreamingMovies          :	Whether the customer has streaming movies or not (Yes, No, No internet service)
#Contract	                : The contract term of the customer (Month-to-month, One year, Two year)
#PaperlessBilling	        : Whether the customer has paperless billing or not (Yes, No)
#PaymentMethod            :	The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
#Churn	                  : Whether the customer churned or not (Yes or No)
#SeniorCitizen	          : Whether the customer is a senior citizen or not (1, 0)
#tenure                   : Number of months the customer has stayed with the company	
#MonthlyCharges           :	The amount charged to the customer monthly
#TotalCharges             :	The total amount charged to the customer

# use the str() to look at the structure of the data
str(churn_data)

# use the head() function to look at the first 10 rows of data
head(churn_data, 10)

# Use the tail() function to look at the last 10 rows of data
tail(churn_data,10)

# Look at the names of the variables in the data set
names(churn_data)

# Looking at the head() and tail() of the data set gives us an glimpse at the data.  
# We can also look at more meaningful information using the summary() to provide some descriptive statistics
# for each variable in the data set.
summary(churn_data)

#skim function helps in providing overview of the data, its an alternative to summary but with lot more details.
skim(churn_data)

# STEP 2: Filter data and check for missing values
# ------------------------------------------------

# We can calculate how many NAs there are in each variable by using the map() in the purrr package
churn_data %>% 
  map(is.na) %>%
  map(sum)

# We can also calculate the proportion of missingness for each variable to confirm if there are missing values/NA in our data set
churn_data %>% 
  map(is.na) %>%
  map(sum)%>%
  map(~ . / nrow(churn_data))%>%
  bind_cols()

# There seems to be NAs for TotalCharges, we will replace these with mean values
any(is.na(churn_data$TotalCharges))
mean_val <- mean(churn_data$TotalCharges[churn_data$TotalCharges > 0], na.rm = TRUE)
churn_data$TotalCharges <- ifelse(is.na(churn_data$TotalCharges), round(mean_val,0), churn_data$TotalCharges)

# converting SeniorCitizen to factors as the values are either 0 or 1. It would be better to retain this as factor than an integer.
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)

# Now we look at the summary again
summary(churn_data)

# STEP 3: IDENTIFYING DATA DISTRIBUTION
# -------------------------------------
# Using ggplot() geom_histogram to graph quantitative variables
my_colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6")

ggplot(data = churn_data)+
  geom_histogram(mapping = aes(x = MonthlyCharges, fill = after_stat(count)), binwidth = 10)+ 
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Monthly Charges ")

ggplot(data = churn_data)+
  geom_histogram(mapping = aes(x = TotalCharges, fill = after_stat(count)), binwidth = 10)+ 
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Total Charges ")

ggplot(data = churn_data)+
  geom_histogram(mapping = aes(x = tenure, fill = after_stat(count)), binwidth = 10)+ 
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Tenure")

# Using ggplot() geom_bar to graph categorial variables
ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = gender, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Gender")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = Partner, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Partner")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = Dependents, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Dependents")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = PhoneService, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors)  +
  labs(title="Distribution of PhoneService")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = MultipleLines, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7)) +
  labs(title="Distribution of MultipleLines")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = InternetService, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of InternetService")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = OnlineSecurity, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of OnlineSecurity")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = OnlineBackup, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of OnlineBackup")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = DeviceProtection, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors)  +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of DeviceProtection")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = TechSupport, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of TechSupport")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = StreamingTV, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of StreamingTV")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = StreamingMovies, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of StreamingMovies")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = Contract, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of Contract")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = PaperlessBilling, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of PaperlessBilling")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = PaymentMethod, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  labs(title="Distribution of PaymentMethod")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = Churn, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of Churn")

ggplot(data = churn_data)+
  geom_bar(mapping = aes(x = SeniorCitizen, fill = after_stat(count)))+
  scale_fill_gradientn(colors = my_colors) +
  labs(title="Distribution of SeniorCitizen")

# Considering the Churn as the outcome variable, as we are interested in understanding the causes for churn
# is tenure a good predictor of Churn
ggplot(data = churn_data, aes(x = tenure)) +
  geom_bar(aes(fill = Churn)) + 
  ggtitle("Distribution of Tenure(in months)")

# is contract a good predictor of Churn
ggplot(data = churn_data, aes(x = Contract)) +
  geom_bar(aes(fill = Churn)) + 
  ggtitle("Distribution of Contract ")

# is total charges a good predictor of Churn
ggplot(data = churn_data, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 30, aes(fill = Churn)) + 
  ggtitle("Distribution of Total Charges")

# is PaperlessBilling a good predictor of Churn
ggplot(data = churn_data, aes(x = PaperlessBilling)) +
  geom_bar(aes(fill = Churn)) + 
    ggtitle("Distribution of PaperlessBilling ")

# is Payment Method a good predictor of Churn
ggplot(data = churn_data, aes(x = PaymentMethod)) +
  geom_bar( aes(fill = Churn)) + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.7))+
  ggtitle("Distribution of Payment Method ")

# correlation plot
quantVars <- unlist(lapply(churn_data, is.numeric))  
churn_data_num <- churn_data[ , quantVars]
churnCorr <- cor(churn_data_num)
corrplot(churnCorr, method="number")

# Since we want to understand the cause of Churn, we can use the Generalized Linear Models, as 
# it is a better fit for data that is binomial.
# The data is split into training and testing sets.The training data set is the data we will use to
# train the models we create and the testing partition is where we will test our models
set.seed(56)
split_train_test <- createDataPartition(churn_data$Churn,p=0.7,list=FALSE)
train<- churn_data[split_train_test,]
test<-  churn_data[-split_train_test,]
train <- train[,-1]
test <- test[,-1]

train$Churn <- as.factor(train$Churn)
glm_model1 <- glm(Churn ~., data = train, family = binomial(link = "logit"))
summary(glm_model1)

# From the above summary, we can see that the tenure, contract status, and total charges have the lowest p-values
# and therefore can be identified as the best predictors of customer churn. We can update the model by considering
# these significant predictor variables

glm_model2 <- update(glm_model1, ~. - tenure - Contract - PaperlessBilling - TotalCharges)
summary(glm_model2)

# Now we can plot updated model
par(mfrow = c(2,2))
plot(glm_model2)

# The next step is to make predictions
glm_probs <- predict(glm_model2, newdata = test, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, "Yes", "No")

# Confusion Matrix for logistic regression using the table() function
table(Predicted = glm_pred, Actual = test$Churn)

# Using the confusionMatrix() in Caret 
confusionMatrix(as.factor(glm_pred), as.factor(test$Churn), positive = "Yes" )

#conclusion
# we can conclude that the logistic regression model is the best fit model with 78% accuracy and very low p-value.
# we have now identified some important predictor variables for customer churn, such as tenure, contract, TotalCharges and PaperlessBilling
# 
# Here is a summary of our findings:
# Customers with month-to-month contracts are less likely to churn.
# Customers who have been with the company longer or have paid more in total are less likely to churn.






