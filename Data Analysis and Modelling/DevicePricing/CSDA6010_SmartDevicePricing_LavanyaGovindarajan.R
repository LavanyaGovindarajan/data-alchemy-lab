########################################################################################################################################################
## Author: Lavanya Govindarajan
## Date: May 08, 2024
########################################################################################################################################################

# install.packages("skimr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("tidyverse")
# install.packages("purr")
# install.packages("caret")
# install.packages("rpart.plot")
# install.packages("readr")
# install.packages("wesanderson")
# #install.packages("colorspace")
# install.packages("gridExtra")
# install.packages("e1071")
# install.packages("fastDummies")
# install.packages("mice")
# install.packages("VIM")
# install.packages("missForest")
# install.packages("Boruta")
# install.packages("plotly")
# install.packages("paletteer")
# install.packages("neuralnet")
# install.packages("class")
# install.packages("rattle")
library(skimr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)
library(tibble)
library(purrr)
library(caret)
library(rpart.plot)
library(readr)
library(wesanderson)
library(colorspace)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(e1071)
library(fastDummies)
library(mice)
library(VIM)
library(missForest)
library(Boruta)
library(plotly)
library(paletteer)
library(neuralnet)
library(class)
library(rattle)

########################################################################################################################################################
#                                     DATA EXPLORATION
########################################################################################################################################################
#load the used phone dataset
phone.df <- read.csv("used_device_data.csv", header = TRUE, sep = ",")

# exploring the structure and content of the data
dim(phone.df)
head(phone.df,10)
tail(phone.df,10)

# understanding data types and statistics
str(phone.df)
skim(phone.df) 
summary(phone.df)

###########################
# MISSING VALUE ANALYSIS
###########################

# Check for missing values
phone.df %>% 
  map(is.na) %>%
  map(sum)

# visualizing missing values
md.pattern(phone.df, rotate.names = TRUE)
aggr(phone.df, prop = FALSE, numbers = TRUE, cex.axis=0.6)

# using correlations to exploring missing values
x <- as.data.frame(abs(is.na(phone.df)))
y <- x[which(apply(x, 2, sum) > 0)]
cor(y)

# converting X4g and X5g to binary numerical variables as they represent the 
# presence or absence of a feature
phone.df$X4g <- ifelse(phone.df$X4g == "yes", 1, 0)
phone.df$X5g <- ifelse(phone.df$X5g == "yes", 1, 0)
# factorizing categorical variables
phone.df[c("device_brand","os")] <- lapply(phone.df[c("device_brand","os")], factor)

# Using missForest under Single imputation to handle missing values.
# choosing missForest over kNN as dataset size is > 1000
set.seed(1234)
phone.df.imp <- missForest(phone.df)$ximp
phone.df.imp

# check again for NAs on the imputed dataset
phone.df.imp %>% 
  map(is.na) %>%
  map(sum)

# all the missing values have been successfully handled 
# using the missForest approach without deleting the records


###########################
# ZERO VALUE CHECK
###########################
# check for zeros
zeros <- colSums(phone.df.imp == 0, na.rm = TRUE)
zeros
# There are  zero values present in the dataset for the binary variables X4g and X5g
# And there are zeros for front_camera_mp for 39 observations for Nokia phones
phone.df.imp[phone.df.imp$front_camera_mp == 0, ]

#Skim function output after data preprocessing.
skim(phone.df.imp)

########################################################################################################################################################
#                                    DATA VISUALIZATIONS
########################################################################################################################################################

###########################
# SETTING COLOR PALETTES 
###########################
single_palette <- c('#FF8989')
spectrum_palette <- paletteer_d("ggsci::red_material")
discrete_palette <- paletteer_d("ggthemes::Purple_Pink_Gray", 4)
discrete_palette_8 <- paletteer_d("ggthemes::Purple_Pink_Gray", 8)
pal1 <- wes_palette("Moonrise1", 4, type = "discrete")
pal2 <- wes_palette("Moonrise2", 4, type = "discrete")
pal3 <- wes_palette("Moonrise3", 4, type = "discrete")

###########################
# OUTLIER ANALYSIS
###########################
# check for outliers
# Draw box plots for numeric variables
par(mfcol=c(4, 3))
boxplot(phone.df.imp$screen_size, xlab="screen size", ylab="values", col=pal1[1])
boxplot(phone.df.imp$rear_camera_mp, xlab="rear camera mp", ylab="values", col=pal1[2])
boxplot(phone.df.imp$front_camera_mp, xlab="front camera mp", ylab="values", col=pal1[3])
boxplot(phone.df.imp$internal_memory, xlab="internal memory", ylab="values", col=pal1[4])
boxplot(phone.df.imp$ram, xlab="ram", ylab="values", col=pal2[1])
boxplot(phone.df.imp$battery, xlab="battery", ylab="values", col=pal2[2])
boxplot(phone.df.imp$weight, xlab="weight", ylab="values", col=pal2[3])
boxplot(phone.df.imp$release_year, xlab="release year", ylab="values", col=pal2[4])
boxplot(phone.df.imp$days_used, xlab="days used", ylab="values", col=pal3[1])
boxplot(phone.df.imp$normalized_used_price, xlab="normalized used price", ylab="values", col=pal3[2])
boxplot(phone.df.imp$normalized_new_price, xlab="normalized new price", ylab="values", col=pal3[3])


################################
# CATEGORICAL VARIABLE ANALYSIS
################################
# os
ggplot(phone.df.imp, aes(x = os, fill = os)) +
  geom_bar(color = "red") +
  scale_fill_manual(values = discrete_palette) +
  labs(x = "OS", y = "Count", title = "Distribution by OS share") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# 4G
# Calculate count and percentage of each value of X4g
count_4g <- phone.df.imp %>%
  group_by(X4g) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define colors for "Yes" and "No"
yes_color <- "#FF8989" 
no_color <- "gray" 
    
# Create the pie chart with modified legend labels and colors
ggplot(count_4g, aes(x = "", y = Percentage, fill = factor(X4g))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("0" = no_color, "1" = yes_color), labels = c("No", "Yes")) +  # Specify colors and labels
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "4G Supported")

# 5G
# Calculate count and percentage of each value of X4g
count_5g <- phone.df.imp %>%
  group_by(X5g) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
  
# Define colors for "Yes" and "No"
yes_color <- "#FF8989"  
no_color <- "gray" 

# Create the pie chart with modified legend labels and colors
ggplot(count_5g, aes(x = "", y = Percentage, fill = factor(X5g))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    scale_fill_manual(values = c("0" = no_color, "1" = yes_color), labels = c("No", "Yes")) +  # Specify colors and labels
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
    labs(fill = "5G Supported")
    
################################
# NUMERICAL VARIABLE ANALYSIS
################################
# screen size 
summary(phone.df.imp["screen_size"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = screen_size, fill = after_stat(count)), binwidth = 0.5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone screen sizes") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# rear_camera_mp 
summary(phone.df.imp["rear_camera_mp"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = rear_camera_mp, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone rare camera") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# front_camera_mp 
summary(phone.df.imp["front_camera_mp"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = front_camera_mp, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone front camera") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# internal_memory 
summary(phone.df.imp["internal_memory"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = internal_memory, fill = after_stat(count)), binwidth = 100, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone internal memory") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# ram 
summary(phone.df.imp["ram"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = ram, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone RAM") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# battery 
summary(phone.df.imp["battery"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = battery, fill = after_stat(count)), binwidth = 1000, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone battery") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# weight 
summary(phone.df.imp["weight"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = weight, fill = after_stat(count)), binwidth = 100, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of phone weight") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# release year 
summary(phone.df.imp["release_year"])
ggplot(phone.df.imp, aes(x = factor(release_year))) +
  geom_bar(fill = "#FF8989", color = "red") +
  labs(title = "Distribution of Release Year",
       x = "Release Year",
       y = "Count") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# days_used 
summary(phone.df.imp["days_used"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = days_used, fill = after_stat(count)), binwidth = 50, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of no of days used") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# normalized_used_price 
summary(phone.df.imp["normalized_used_price"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = normalized_used_price, fill = after_stat(count)), binwidth = 0.5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of used price") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# normalized_new_price 
summary(phone.df.imp["normalized_new_price"])
ggplot(data = phone.df.imp)+
  geom_histogram(mapping = aes(x = normalized_new_price, fill = after_stat(count)), binwidth = 0.5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of new price") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")
    
## brand wise analysis
# Calculate the count and percentage of each device brand
brand_counts <- phone.df.imp %>%
  count(device_brand) %>%
  mutate(percentage = prop.table(n) * 100)

# Create a bar plot for device brands with count and percentage labels
ggplot(brand_counts, aes(x = reorder(device_brand, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_gradient(low = "#FFBED1FF", high = "#C46487FF", name = "Count") +
  labs(title = "Count of Devices by Brand",
       x = "Device Brand",
       y = "Count") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

################################
# BIVARIATE ANALYSIS
################################

# used price vs new price
ggplot(phone.df.imp, aes(x = normalized_new_price, y = normalized_used_price, color = factor(device_brand))) +
  geom_point() +  # Add points
  labs(x = "New Price", y = "Used Price") + 
  ggtitle("Normalized Used Price vs Normalized New Price") + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# days used vs used price
ggplot(phone.df.imp, aes(x = days_used, y = normalized_used_price)) +
  geom_point(color = "steelblue") +  # Add points
  labs(x = "Days Used", y = "Normalized Used Price", title = "Days Used vs. Normalized Used Price") +
  theme_minimal()

# Analyzing 4G and 5G services support
# Count observations based on combinations of X4g and X5g, and calculate percentages
df_summary <- phone.df.imp %>%
  mutate(
    combination = case_when(
      X4g == 1 & X5g == 1 ~ "Both 4G and 5G",
      X4g == 1 & X5g == 0 ~ "Only 4G",
      X4g == 0 & X5g == 1 ~ "Only 5G",
      TRUE ~ "None"
    )
  ) %>%
  group_by(combination) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(df_summary, aes(x = combination, y = count, fill = combination)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Observations by 4G and 5G Presence",
    x = "Combination of 4G and 5G",
    y = "Count"
  ) +
  theme(panel.grid = element_blank())  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = discrete_palette)

# Analyzing the distribution of phones and tablets
# Calculate percentage of devices with weights above 300 grams
weight_above_300 <- mean(phone.df.imp$weight > 300) * 100
# Calculate percentage of devices with screen sizes greater than 18 centimeters
screen_size_above_18 <- mean(phone.df.imp$screen_size > 18) * 100
df <- data.frame(
  category = c("Weight > 300g", "Screen Size > 18cm"),
  percentage = c(weight_above_300, screen_size_above_18)
)
ggplot(df, aes(x = category, y = percentage, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Percentage of tablets",
    x = "Category",
    y = "Percentage"
  ) +
  theme(panel.grid = element_blank())  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#C46487FF", "#FFBED1FF"))

# used price vs release year
cust_paletter_release_year <- c("#C6C1F0FF", "#C6C1F0FF", "#C6C1F0FF", "#C6C1F0FF",
                            "#C46487FF",  "#C46487FF",  "#C46487FF",  "#C46487FF")
phone.df.imp.release <- phone.df.imp
phone.df.imp.release$release_year <- as.factor(phone.df.imp$release_year)
summary_stats <- aggregate(normalized_used_price ~ release_year, data = phone.df.imp.release, FUN = mean)  # You can replace 'mean' with 'median' if you prefer
ggplot(summary_stats, aes(x = release_year, y = normalized_used_price)) +
  geom_bar(stat = "identity", fill = cust_paletter_release_year) +  
  labs(x = "Release Year", y = "Mean Used Price") +  
  ggtitle("Mean Used Price vs Release Year") +  
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")
                                           
# used price vs screen size
thresholds <- c(0, 7.62, 12.7, 19.05, Inf)
categories <- c("small", "medium", "large", "very large")
phone.df.imp$screen_size_category <- cut(phone.df.imp$screen_size, breaks = thresholds, labels = categories, right = FALSE)
summary_stats <- aggregate(normalized_used_price ~ screen_size_category, data = phone.df.imp, FUN = mean)
ggplot(summary_stats, aes(x = screen_size_category, y = normalized_used_price)) +
  geom_bar(stat = "identity", fill = discrete_palette) +
  labs(x = "Screen Size Category", y = "Mean Used Price") +
  ggtitle("Mean Used Price vs Screen Size Category") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# used price vs front and rear camera resolutions
ggplot(phone.df.imp, aes(x = front_camera_mp, y = normalized_used_price, color = rear_camera_mp)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "#C6C1F0FF", high = "#C4648FFF", name = "Rear Camera MP") +
  labs(title = "Impact of Camera Specs on Used Price",
       x = "Front Camera Megapixels",
       y = "Normalized Used Price") +
  coord_flip()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

########################################################################################################################################################
#                                     PREDICTOR ANALYSIS AND RELEVANCY
########################################################################################################################################################
str(phone.df.imp)

################################
# CORRELATION ANALYSIS
################################
# correlation plot
phone_corr<- cor(phone.df.imp[, -1] %>% mutate_if(is.factor, as.numeric))
corrplot(phone_corr, method = "color")

# simplified correlation plot
phone_corr_simple <- function(data=phone.df.imp[, -1],sig=0.3){
  corr <- cor(phone.df.imp[, -1] %>% mutate_if(is.factor, as.numeric))
  corr[corr == 1] <- NA #drop perfect correlations
  corr <- as.data.frame(as.table(corr))
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method = "number")
}
phone_corr_simple()

# target variable (normalized_used_price) correlation
numeric_df <- phone.df.imp[sapply(phone.df.imp, is.numeric)] 
target_cor <- cor(phone.df.imp$normalized_used_price, numeric_df)
corrplot(target_cor, method = "number")

################################
# DUMMY VARIABLES
################################
# creating dummy variables for os and device_brand
phone.df.imp <- dummy_cols(phone.df.imp, select_columns = c("os","device_brand"),
                                        remove_selected_columns=TRUE, 
                                        remove_first_dummy=FALSE) 
skim(phone.df.imp)


################################
# VARIABLE SELECTION
################################

# 1. Using Boruta
#_________________
set.seed(1234)
phone.df.boruta <- Boruta(normalized_used_price ~ . , data = phone.df.imp, doTrace = 0)
print(phone.df.boruta)
# all variables deemed important
# printing important scores
phone.df.boruta$ImpHistory[1:30,]
# box plot for important scores
df_long <- tidyr::gather(as.data.frame(phone.df.boruta$ImpHistory), feature, measurement)
plot_ly(df_long, y = ~measurement, color = ~feature, type = "box") %>%
  layout(title="Box-and-whisker Plots across all 51 Features (used phone data)",
         xaxis = list(title="Features"),
         yaxis = list(title="Importance"),
         showlegend=F)
# get the confirmed variables
phone.df.predictor.boruta <- getSelectedAttributes(phone.df.boruta, withTentative = F)
phone.df.predictor.boruta

# 2. Using RFE
#_________________
set.seed(1234)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
phone.df.imp.rfe <- rfe(phone.df.imp[, -12], phone.df.imp[, 12], sizes = c(10, 20, 30, 40, 50),
                       rfeControl = control )
phone.df.imp.rfe
phone.df.imp.rfe$optsize
phone.df.imp.rfe$optvariables
phone.df.predictor.rfe <- predictors(phone.df.imp.rfe)
phone.df.predictor.rfe

# 3. Using Stepwise
#_________________
# define a base model
phone.df.imp.base.model <- lm(normalized_used_price ~ 1, data = phone.df.imp)
# define full model
phone.df.imp.all.model <- lm(normalized_used_price ~ ., data = phone.df.imp)
phone.df.imp.stepwise <- step(phone.df.imp.base.model, scope = list(lower = phone.df.imp.base.model,
                               upper = phone.df.imp.all.model), direction = "both", k=2, trace = F)
summary(phone.df.imp.stepwise)
# stepwise confirmed variables
phone.df.imp.stepwise.conf <- names(unlist(phone.df.imp.stepwise[[1]]))
phone.df.imp.stepwise.conf <- phone.df.imp.stepwise.conf[!phone.df.imp.stepwise.conf %in% "(Intercept)"]
print(phone.df.imp.stepwise.conf)

# comparing models
intersect(phone.df.predictor.boruta, phone.df.predictor.rfe)
intersect(phone.df.predictor.boruta, phone.df.imp.stepwise.conf)
variables <- unique(c(phone.df.predictor.boruta, phone.df.predictor.rfe, phone.df.imp.stepwise.conf))
variable_table <- data.frame(Variable = variables)
variable_table$Boruta <- ifelse(variable_table$Variable %in% phone.df.predictor.boruta, "*", "")
variable_table$RFE <- ifelse(variable_table$Variable %in% phone.df.predictor.rfe, "*", "")
variable_table$Stepwise <- ifelse(variable_table$Variable %in% phone.df.imp.stepwise.conf, "*", "")
print(variable_table)

########################################################################################################################################################
#                                DATA PARTITIONING
########################################################################################################################################################
## partitioning into training (40%), validation (35%), holdout (25%)
## Splitting output and predictor variables
set.seed(45)
Y <- data.frame(used_price = phone.df.imp[, 12])
X <- phone.df.imp[, -12] 

## creating train, validation and holdout partition indices
train.rows <- sample(rownames(X), nrow(X)*0.4)
valid.rows <- sample(setdiff(rownames(X), train.rows),
                     nrow(X)*0.35)
holdout.rows <- setdiff(rownames(X), union(train.rows, valid.rows))

## creating train, validation and holdout partition dataset
phone.train.df <- X[train.rows, ]
phone.valid.df <- X[valid.rows, ]
phone.holdout.df <- X[holdout.rows, ]

## creating training partition output variables
phone.train.df.labels <- data.frame(used_price = Y[train.rows, ])
mean_train_Y <- mean(phone.train.df.labels[, 1])
mean_train_Y
## 4.357909
phone.train.df.labels$price_range <- ifelse(phone.train.df.labels[, 1] > mean_train_Y, "Premium", "WalletFriendly")
phone.train.df.labels$price_range <- factor(phone.train.df.labels$price_range, levels = c("WalletFriendly", "Premium"), labels = c(0, 1))

## creating validation partition output variables
phone.valid.df.labels <- data.frame(used_price = Y[valid.rows, ])
phone.valid.df.labels$price_range <- ifelse(phone.valid.df.labels[, 1] > mean_train_Y, "Premium", "WalletFriendly")
phone.valid.df.labels$price_range <- factor(phone.valid.df.labels$price_range, levels = c("WalletFriendly", "Premium"), labels = c(0, 1))

## creating holdout partition output variables
phone.holdout.df.labels <-  data.frame(used_price = Y[holdout.rows, ])
phone.holdout.df.labels$price_range <- ifelse(phone.holdout.df.labels[, 1] > mean_train_Y, "Premium", "WalletFriendly")
phone.holdout.df.labels$price_range <- factor(phone.holdout.df.labels$price_range, levels = c("WalletFriendly", "Premium"), labels = c(0, 1))

########################################################################################################################################################
#                                   DATA NORMALIZATION
########################################################################################################################################################

phone.train.df.scale.values <- preProcess(phone.train.df, method=c("center", "scale"))
phone.train.df.norm <- predict(phone.train.df.scale.values, phone.train.df)
phone.valid.df.norm <- predict(phone.train.df.scale.values, phone.valid.df)
phone.holdout.df.norm <- predict(phone.train.df.scale.values, phone.holdout.df)

########################################################################################################################################################
#                                    REGRESSION MODEL SELECTION
########################################################################################################################################################

##################################
## (a) MULTIPLE LINEAR REGRESSION
##################################
# fit the model on training data
lm_model <- lm(phone.train.df.labels[, 1] ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                 weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                 device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                 device_brand_Sony + device_brand_Realme + device_brand_Lenovo, data = phone.train.df.norm)

# evaluate goodness of fit
summary(lm_model)

# optimizing the model by considering only the significant predictors
lm_model_1 <- lm(phone.train.df.labels[, 1] ~ screen_size + rear_camera_mp + front_camera_mp + ram + battery +
                   weight + release_year + days_used + normalized_new_price + device_brand_Celkon + device_brand_Xiaomi +
                   device_brand_Sony + device_brand_Realme, data = phone.train.df.norm)
summary(lm_model_1)

ggplot() +
  geom_histogram(aes(x=lm_model_1$residuals), fill="lightgray", color="grey") +
  labs(x="Residuals", y="Frequency")
summary(lm_model_1$residuals)

# predict on validation data
lm_pred <- predict(lm_model_1, phone.valid.df.norm)
all.residuals <- phone.valid.df.labels[, 1] - lm_pred

ggplot() +
  geom_histogram(aes(x=all.residuals), fill="lightgray", color="grey") +
  labs(x="Residuals", y="Frequency")
summary(all.residuals)

# Evaluate Predictive Accuracy
summary(lm_pred)
summary(phone.valid.df.labels[, 1])
mean(phone.valid.df.labels[, 1])
cor(lm_pred, phone.valid.df.labels[, 1])
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(phone.valid.df.labels[,1], lm_pred)
forecast::accuracy(phone.valid.df.labels[,1], lm_pred)

##################################
## (b) REGRESSION TREE
##################################

# fit the model on training data
reg_tree_model<-rpart(phone.train.df.labels[, 1] ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                        weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                        device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                        device_brand_Sony + device_brand_Realme + device_brand_Lenovo, data=phone.train.df.norm)
reg_tree_model
fancyRpartPlot(reg_tree_model, cex = 0.7)

# predict on validation data
reg_tree_pred<-predict(reg_tree_model, phone.valid.df.norm)

# Evaluate Predictive Accuracy
summary(reg_tree_pred)
summary(phone.valid.df.labels[, 1])
cor(reg_tree_pred, phone.valid.df.labels[, 1])
MAE(phone.valid.df.labels[, 1], reg_tree_pred)
forecast::accuracy(phone.valid.df.labels[,1], reg_tree_pred)

##################################
## (c) NEURAL NETWORKS
##################################

# fit the model on training data
set.seed(45)
nn_model <- neuralnet(phone.train.df.labels$used_price ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                        weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                        device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                        device_brand_Sony + device_brand_Realme + device_brand_Lenovo, data = phone.train.df.norm, linear.output = T, hidden = 4)

nn_model$weights
plot(nn_model)

# predict on validation data
pred_nn <- compute(nn_model, phone.valid.df.norm)

# Evaluate Predictive Accuracy
cor(pred_nn$net.result, phone.valid.df.labels[, 1])
MAE(phone.valid.df.labels[, 1], pred_nn$net)

plot_ly() %>%
  add_markers(x=pred_nn$net.result, y=phone.valid.df.labels[, 1], 
              name="Data Scatter", type="scatter", mode="markers") %>%
  add_trace(x = c(0,1), y = c(0,1), type="scatter", mode="lines",
            line = list(width = 4), name="Ideal Agreement") %>%
  layout(title=paste0('Scatterplot (Normalized) Observed vs. Predicted Used prices of smart devices, Cor(Obs,Pred)=',
                      round(cor(pred_nn$net.result, phone.valid.df.labels[, 1]), 2)),
         xaxis = list(title="NN (hidden=4) Used Price Predictions"),
         yaxis = list(title="(Normalized) Observed Used Price"),
         legend = list(orientation = 'h'))

forecast::accuracy(phone.valid.df.labels[,1], pred_nn$net)

########################################################################################################################################################
#                                    CLASSIFICATION MODEL SELECTION
########################################################################################################################################################

###############################
## (a) LOGISTIC REGRESSION
###############################

# Fit Logistic Regression model on training data
logistic_model <- glm(phone.train.df.labels$price_range ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                        weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                        device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                        device_brand_Sony + device_brand_Realme + device_brand_Lenovo, family = binomial(), data = phone.train.df.norm)

# Evaluate goodness of fit
summary(logistic_model)

# Use the fitted model for evaluation on validation data
log_pred <- predict(logistic_model, newdata = phone.valid.df.norm, type = "response")

# Evaluate Predictive Accuracy
conf_matrix <- confusionMatrix(factor(ifelse(log_pred > 0.5, 1, 0), levels = c(1,0)),
                               factor(phone.valid.df.labels$price_range, levels = c(1,0)))
conf_matrix

# The customers are ranked by their probabilities of being a purchaser from High to Low.
phone.valid.df.norm.rank <- phone.valid.df.norm
phone.valid.df.norm.rank$actual <- phone.valid.df.labels$price_range
phone.valid.df.norm.rank$predicted_probabilities <- log_pred

# Cumulative gains chart
ranked_data_frame <- phone.valid.df.norm.rank[order(-phone.valid.df.norm.rank$predicted_probabilities),]
ranked_data_frame$cumulative_positives <- cumsum(as.numeric(ranked_data_frame$actual))
total_positives <- sum(as.numeric(ranked_data_frame$actual))
ranked_data_frame$percentage_of_gain <- ranked_data_frame$cumulative_positives / total_positives * 100
ranked_data_frame$percentage_of_cases <- seq_along(ranked_data_frame$actual) / nrow(ranked_data_frame) * 100

ggplot(ranked_data_frame, aes(x = percentage_of_cases, y = percentage_of_gain)) +
  geom_line(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Percentage of Cases", y = "Percentage of Positives Gained", 
       title = "Cumulative Gains Chart for Logistic Regression") +
  theme_minimal()

# Cumulative Lifts chart
ranked_data_frame$cumulative_positives <- cumsum(as.numeric(ranked_data_frame$actual))
total_positives <- sum(as.numeric(ranked_data_frame$actual))
ranked_data_frame$response_rate <- ranked_data_frame$cumulative_positives / seq_along(ranked_data_frame$actual)
overall_response_rate <- total_positives / nrow(ranked_data_frame)
ranked_data_frame$cumulative_lift <- ranked_data_frame$response_rate / overall_response_rate
ranked_data_frame$percentage_of_cases <- seq_along(ranked_data_frame$actual) / nrow(ranked_data_frame) * 100

ggplot(ranked_data_frame, aes(x = percentage_of_cases, y = cumulative_lift)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Percentage of Cases Targeted", y = "Cumulative Lift", 
       title = "Cumulative Lift Chart for Logistic Regression") +
  theme_minimal()


###############################
## (b) NAIVE BAYES
###############################
# Fit Naive Bayes model on training data
nb_model <- naiveBayes(phone.train.df.labels$price_range ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                         weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                         device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                         device_brand_Sony + device_brand_Realme + device_brand_Lenovo, data = phone.train.df.norm)
nb_model
# Use the fitted model for evaluation on validation data
nb_pred <- predict(nb_model, newdata = phone.valid.df.norm)
nb_pred
# Convert predicted and actual outcomes to factors with explicit levels
nb_pred_factor <- factor(nb_pred, levels = c("1", "0"))
nb_actual_factor <- factor(phone.valid.df.labels$price_range, levels = c("1", "0"))

# Evaluate Predictive Accuracy
# Confusion Matrix
confusionMatrix(nb_pred_factor, nb_actual_factor, positive = "1")

###############################
## (c) CLASSIFICATION TREES
###############################
# Fit a tree model on training data
rpart_model <- rpart(phone.train.df.labels$price_range  ~ screen_size + X4g + X5g + rear_camera_mp + front_camera_mp + ram + battery + 
                       weight + release_year + days_used + normalized_new_price + device_brand_Asus +  device_brand_BlackBerry + 
                       device_brand_Celkon + device_brand_Infinix + device_brand_Nokia + device_brand_Xiaomi + os_iOS + os_Others +
                       device_brand_Sony + device_brand_Realme + device_brand_Lenovo, data=phone.train.df.norm, method="class")
rpart_model
# plot the tree
rpart.plot(rpart_model, extra=1, fallen.leaves=FALSE)
# predict on validation data
rpart_pred <- predict(rpart_model, phone.valid.df.norm, type = "class")
# confusion matrix
class_conf_matrix <- confusionMatrix(factor(rpart_pred, levels = c(1,0)), factor(phone.valid.df.labels$price_range, levels = c(1,0)), positive = "1")
class_conf_matrix

########################################################################################################################################################
#                                     PREDICTION
########################################################################################################################################################

## Using Logistic Regression as the classification model to make classifications on holdout set
predict_class_holdout <- predict(logistic_model, phone.holdout.df.norm, type = "response")
predict_class_holdout
conf_matrix <- confusionMatrix(factor(ifelse(predict_class_holdout > 0.5, 1, 0), levels = c(1,0)),
                               factor(phone.holdout.df.labels$price_range, levels = c(1,0)))
conf_matrix
## Add a column to the data frame with the predicted probability of purchase
phone.holdout.df.norm$predicted_prob <- predict_class_holdout
phone.holdout.df.norm$predicted_price_range <- ifelse(predict_class_holdout > 0.5, "High", "Low")

## Using Multiple Linear regression as the regression model to make predictions on holdout set
predict_reg_holdout<-predict(lm_model, phone.holdout.df.norm)
predict_reg_holdout
summary(phone.holdout.df.labels[,1])
mean(phone.holdout.df.labels[,1])
cor(predict_reg_holdout, phone.holdout.df.labels[,1])

MAE(phone.holdout.df.labels[,1], predict_reg_holdout)
mean(phone.holdout.df.labels[,1])
## Add a column to the data frame with the predicted spending
phone.holdout.df.norm$predicted_used_price <- predict_reg_holdout

