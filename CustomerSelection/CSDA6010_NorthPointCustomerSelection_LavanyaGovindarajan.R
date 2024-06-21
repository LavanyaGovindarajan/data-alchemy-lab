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
library(gridExtra)
library(rpart)
library(rpart.plot)
library(e1071)

########################################################################################################################################################
#                                   DATA EXPLORATION
########################################################################################################################################################
#load csv dataset
npdata <- read.csv("North-Point List.csv", header = TRUE, sep = ",")

# exploring data
dim(npdata)
head(npdata,10)
tail(npdata,10)

# Renaming Attributes
npdata <- rename(npdata, sequenceNo = sequence_number, USCustomer = US, sourceA = source_a, 
                 sourceB = source_b, sourceC = source_c, sourceD = source_d, sourceE = source_e, 
                 sourceM = source_m, sourceO = source_o, sourceH = source_h, sourceR = source_r, 
                 sourceS = source_s, sourceT = source_t, sourceU = source_u, sourceP = source_p, 
                 sourceX = source_x, sourceW = source_w, frequency = Freq, lastUpdate = last_update_days_ago,
                 firstUpdate = X1st_update_days_ago, isResAddr = Address_is_res, webOrder = Web.order, 
                 gender = Gender.male, purchase = Purchase, spending = Spending)
names(npdata)

# understanding data types and statistics
str(npdata)
skim(npdata)
summary(npdata)

###########################
# MISSING VALUE ANALYSIS
###########################
# Check for missing values
npdata %>% 
  map(is.na) %>%
  map(sum)
# no missing values

###########################
# ZERO VALUE CHECK
###########################
zeros <- colSums(npdata == 0, na.rm = TRUE)
zeros
# There are only 999 zeros for spending

# The customer 711 is a non-purchaser who has spent some amount, which might be an anomaly.
data_non_purchasers <- npdata[npdata$purchase == 0, ]
ggplot(data_non_purchasers, aes(x = factor(purchase), y = spending)) +
  geom_point(aes(color = ifelse(spending == 1, "red", "black"))) +
  geom_text(data = subset(data_non_purchasers, spending == 1), aes(label = sequenceNo), vjust = -1, color = "red") + 
  labs(x = "Purchase", y = "Spending") +
  scale_color_identity(guide = "none") +  
  scale_y_continuous(limits = c(0, 2)) +  
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black")) 
# Hence updating the Spending value to 0
npdata$spending[npdata$purchase == 0 & npdata$spending != 0] <- 0
# checking if the update is successful
npdata[npdata$sequence_number == 711,]
########################################################################################################################################################
#                                    DATA VISUALIZATIONS
########################################################################################################################################################

###########################
# SETTING COLOR PALETTES 
###########################
pal <- wes_palette("Moonrise2", 2000, type = "continuous")
pal1 <- wes_palette("Moonrise2", 4, type = "discrete")
paldiscrete <- wes_palette("Moonrise2", 2, type = "discrete")

###########################
# OUTLIER ANALYSIS
###########################
# Draw box plots for numeric variables
par(mfcol=c(1, 4))
boxplot(npdata$frequency, xlab="frequency", ylab="values", col=pal1[1])
boxplot(npdata$lastUpdate, xlab="lastUpdate", ylab="values", col=pal1[2])
boxplot(npdata$firstUpdate, xlab="firstUpdate", ylab="values", col=pal1[3])
boxplot(npdata$spending, xlab="spending", ylab="values", col=pal1[4])

################################
# NUMERICAL VARIABLE ANALYSIS
################################
# frequency 
summary(npdata["frequency"])
ggplot(data = npdata)+
  geom_histogram(mapping = aes(x = frequency, fill = after_stat(count)), binwidth = 0.5)+ 
  scale_fill_gradientn(colors = pal) +
  labs(title="Distribution of frequency of purchases") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# lastupdate 
summary(npdata["lastUpdate"])
ggplot(data = npdata)+
  geom_histogram(mapping = aes(x = lastUpdate, fill = after_stat(count)), binwidth = 200)+ 
  scale_fill_gradientn(colors = pal) +
  labs(title="Distribution of last updates") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# firstupdate 
summary(npdata["firstUpdate"])
ggplot(data = npdata)+
  geom_histogram(mapping = aes(x = firstUpdate, fill = after_stat(count)), binwidth = 200)+ 
  scale_fill_gradientn(colors = pal) +
  labs(title="Distribution of first updates") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# spending
data <- npdata[npdata$spending > 0, ]
summary(data["spending"])
ggplot(data)+
  geom_histogram(mapping = aes(x = spending, fill = after_stat(count)), 
                 binwidth = 100) + 
  scale_fill_gradientn(colors = pal) +
  labs(title="Distribution of spending") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

################################
# BINARY VARIABLE ANALYSIS
################################
plot1 <- ggplot(data = npdata) +
  geom_bar(mapping = aes(x = USCustomer, fill = after_stat(count))) +
  scale_fill_gradientn(colors = paldiscrete) +
  labs(title = "Distribution of US and NON-US based customers") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Other", "US customer")) +
theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))
plot2 <- ggplot(data = npdata) +
  geom_bar(mapping = aes(x = webOrder, fill = after_stat(count))) +
  scale_fill_gradientn(colors = paldiscrete) +
  labs(title = "Distribution of the order types") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))

plot3 <- ggplot(data = npdata) +
  geom_bar(mapping = aes(x = gender, fill = after_stat(count))) +
  scale_fill_gradientn(colors = paldiscrete) +
  labs(title = "Distribution of gender") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))

plot4 <- ggplot(data = npdata) +
  geom_bar(mapping = aes(x = isResAddr, fill = after_stat(count))) +
  scale_fill_gradientn(colors = paldiscrete) +
  labs(title = "Distribution of resident and non-resident customers") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 1)


## customers who aren't from any of the 15 sources
specified_columns <- c("sourceA", "sourceB", "sourceC","sourceD", "sourceE", "sourceM","sourceO", "sourceH", "sourceR",
                       "sourceS", "sourceT", "sourceU", "sourceP", "sourceX", "sourceW")
nrow(npdata[rowSums(npdata[, specified_columns] == 0) == 15, ])
npdata$sourceOther <- ifelse(rowSums(npdata[, specified_columns]) == 0, 1, 0)
nrow(npdata[npdata$sourceOther == 1, ])

## Plotting histogram to show no of customers from each source
## Calculate the sum of 1s for each source column
source_counts <- colSums(npdata[, c("sourceA", "sourceB", "sourceC", "sourceD", "sourceE",
                                    "sourceM", "sourceO", "sourceH", "sourceR", "sourceS",
                                    "sourceT", "sourceU", "sourceP", "sourceX", "sourceW", "sourceOther")])
source_counts_df <- data.frame(source = names(source_counts), count = source_counts)
## Create the histogram
pal2 <- wes_palette("Moonrise2", 3, type = "discrete")
plot_source_counts <- ggplot(data = source_counts_df, aes(x = source, y = count)) +
  geom_bar(stat = "identity", fill = pal2[2], color = pal2[3]) +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.9), 
            color = "black", size = 3) + 
  labs(title = "Customer count by Source") +
  xlab("Source") +
  ylab("Counts") +
  theme_minimal() +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(plot_source_counts)


########################################################################################################################################################
#                                    PREDICTOR ANALYSIS AND RELEVANCY
########################################################################################################################################################

################################
# CORRELATION ANALYSIS
################################
npdataCorr <- cor(npdata[,-1] %>% mutate_if(is.factor, as.numeric))
corrplot(npdataCorr, method = "color")
  
# simplified correlation plot
  corr_simple <- function(data=npdata,sig=0.3){
    corr <- cor(npdata %>% mutate_if(is.factor, as.numeric))
    corr[corr == 1] <- NA #drop perfect correlations
    corr <- as.data.frame(as.table(corr))
    corr <- subset(corr, abs(Freq) > sig) 
    corr <- corr[order(-abs(corr$Freq)),] 
    print(corr)
    mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
        corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method = "number")
  }
  corr_simple()
  
################################
# BIVARIATE ANALYSIS
################################
# firstUpdate vs lastUpdate
ggplot(data = npdata, aes(x = firstUpdate, y = lastUpdate)) +
  geom_point(color = pal ) +
  ggtitle("Distribution of firstUpdate vs lastUpdate") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# frequency vs purchase
purchaser_data <- npdata[npdata$purchase == 1, ]
non_purchaser_date <- npdata[npdata$purchase == 0, ]
ggplot(data = npdata, aes(x = frequency)) +
  geom_bar(aes(fill = factor(purchase))) + 
  scale_fill_manual(values = pal1) +
  ggtitle("Distribution of frequency vs purchase") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))
  
# frequency vs spending
ggplot(data = npdata, aes(x = frequency, y = spending)) +
  geom_point(color = pal )+
  ggtitle("Distribution of frequency vs spending") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")
      ,axis.line = element_line(color = "black"))
  
# frequency vs lastUpdate
ggplot(data = npdata, aes(x = frequency, y = lastUpdate)) +
  geom_point(color = pal )+
  ggtitle("Distribution of frequency vs lastUpdate") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")
      ,axis.line = element_line(color = "black"))
  
# sourceH vs isResAdder  
ggplot(data = npdata, aes(x = factor(isResAddr), fill = factor(sourceH))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = pal1[1:2]) +  # Adjust color palette indices as needed
  labs(x = "isResAddr", fill = "sourceH") +  
  ggtitle("SourceH vs. isResAddr") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# firstUpdate vs sourceW
ggplot(data = npdata, aes(x = firstUpdate)) +
  geom_bar(aes(fill = factor(sourceW))) + 
  scale_fill_manual(values = pal1[1:2]) +
  ggtitle("Distribution of firstUpdate vs sourceW") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

# lastUpdate vs sourceW
ggplot(data = npdata, aes(x = lastUpdate)) +
  geom_bar(aes(fill = factor(sourceW))) + 
  scale_fill_manual(values = pal1[1:2]) +
  ggtitle("Distribution of lastUpdate vs sourceW") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"))


################################
# VARIABLE SELECTION
################################ 
## removing sequenceNo
npdata <- npdata[,-1]

car::vif(lm(as.numeric(purchase) ~ USCustomer + frequency + lastUpdate + firstUpdate + webOrder + gender +
              isResAddr + sourceA + sourceB + sourceC + sourceD + sourceE + sourceM + sourceO + sourceH +
              sourceR + sourceS + sourceT + sourceU + sourceP + sourceX + sourceW, data=npdata))

car::vif(lm(spending ~ USCustomer + frequency + lastUpdate + firstUpdate + webOrder + gender +
              isResAddr + sourceA + sourceB + sourceC + sourceD + sourceE + sourceM + sourceO + sourceH +
              sourceR + sourceS + sourceT + sourceU + sourceP + sourceX + sourceW, data=npdata))

########################################################################################################################################################
#                                     DATA PARTITIONING
########################################################################################################################################################
## partitioning into training (40%), validation (35%), holdout (25%)
set.seed(35)
Y <- npdata[, c(23, 24)]
X <- npdata[, -c( 23, 24)] 
train.rows <- sample(rownames(X), nrow(X)*0.4)
valid.rows <- sample(setdiff(rownames(X), train.rows),
                     nrow(X)*0.35)
holdout.rows <- setdiff(rownames(X), union(train.rows, valid.rows))

train.df <- X[train.rows, ]
valid.df <- X[valid.rows, ]
holdout.df <- X[holdout.rows, ]


train.df.labels <- Y[train.rows, ]
valid.df.labels <- Y[valid.rows, ]
holdout.df.labels <- Y[holdout.rows, ]

########################################################################################################################################################
#                                     DATA NORMALIZATION
########################################################################################################################################################

train.df.scale.values <- preProcess(train.df, method=c("center", "scale"))
train.df.norm <- predict(train.df.scale.values, train.df)
valid.df.norm <- predict(train.df.scale.values, valid.df)
holdout.df.norm <- predict(train.df.scale.values, holdout.df)

########################################################################################################################################################
#                                   CLASSIFICATION MODEL SELECTION
########################################################################################################################################################
## Removing 'spending' column for all modelss while performing Classification as they are highly correlated
###############################
## (a) LOGISTIC REGRESSION
###############################

# Fit Logistic Regression model on training data
log_model <- glm(train.df.labels$purchase ~ ., family = binomial(), data = train.df.norm)
summary(log_model)

# Perform Step-wise regression to selecte statistically significant predictors
stepwise_model_class <- step(log_model, direction = "both")
summary(stepwise_model_class)

# Fit Logistic Regression model with significant predictors
log_model_2 <- glm(train.df.labels$purchase ~ sourceA + sourceH + sourceU + sourceR + sourceW + 
                     frequency + webOrder + isResAddr, family = binomial(), data = train.df.norm)
summary(log_model_2)

# Use the fitted model for evaluation on validation data
log_pred <- predict(log_model_2, valid.df.norm, type = "response")
conf_matrix <- confusionMatrix(factor(ifelse(log_pred > 0.5, 1, 0), levels = c(1,0)),
                              factor(valid.df.labels$purchase, levels = c(1,0)))
conf_matrix

# Calculating error cost to improve sensitivity
weights <- ifelse(train.df.labels$purchase == 1, 5, 1)
weights
# Fit Logistic Regression model using the correct weights and evaluate
log_model_3 <- glm(train.df.labels$purchase ~ sourceA + sourceH + sourceU + sourceR + sourceW + 
                     frequency + webOrder + isResAddr, family = binomial(), data = train.df.norm, 
                   weights = weights)                 
summary(log_model_3)
log_pred <- predict(log_model_3, valid.df.norm, type = "response")
conf_matrix_2 <- confusionMatrix(factor(ifelse(log_pred > 0.5, 1, 0), levels = c(1,0)),
                               factor(valid.df.labels$purchase, levels = c(1,0)))
conf_matrix_2
# The sensitivity has increased from 75% to 97% by correctly classifying purchasers
# The customers are ranked by their probabilities of being a purchaser from High to Low.
valid.df.norm.rank <- valid.df.norm
valid.df.norm.rank$actual <- valid.df.labels$purchase
valid.df.norm.rank$predicted_probabilities <- log_pred

###############################
## (b) NAIVE BAYES
###############################
# Fit Naive Bayes model on training data
nb_model <- naiveBayes(train.df.labels$purchase ~ . , data = train.df.norm)
nb_model
# Fit Naive Bayes model with significant predictors
nb_model_2 <- naiveBayes(train.df.labels$purchase ~  sourceA + sourceH + sourceU + sourceR + sourceW + 
                           frequency + webOrder + isResAddr, data = train.df.norm)
nb_model_2

# Use the fitted model for evaluation on validation data
nb_pred <- predict(nb_model_2, newdata = valid.df.norm)
nb_pred
# Convert predicted and actual outcomes to factors with explicit levels
nb_pred_factor <- factor(nb_pred, levels = c("1", "0"))
nb_actual_factor <- factor(valid.df.labels$purchase, levels = c("1", "0"))

# Confusion Matrix
confusionMatrix(nb_pred_factor, nb_actual_factor, positive = "1")

####################################
## CLASSIFICATION MODEL EVALUATION
####################################

# Cumulative gains chart
#_______________________

# For Logistic Regression
ranked_data_frame <- valid.df.norm.rank[order(-valid.df.norm.rank$predicted_probabilities),]
ranked_data_frame$cumulative_positives <- cumsum(ranked_data_frame$actual)
total_positives <- sum(ranked_data_frame$actual)
ranked_data_frame$percentage_of_gain <- ranked_data_frame$cumulative_positives / total_positives * 100
ranked_data_frame$percentage_of_cases <- seq_along(ranked_data_frame$actual) / nrow(ranked_data_frame) * 100

ggplot(ranked_data_frame, aes(x = percentage_of_cases, y = percentage_of_gain)) +
  geom_line(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Percentage of Cases", y = "Percentage of Positives Gained", 
       title = "Cumulative Gains Chart for Logistic Regression") +
  theme_minimal()

# For Naive Bayes
nb.valid.df.norm.rank <- valid.df.norm
nb.valid.df.norm.rank$actual <- valid.df.labels$purchase
nb.valid.df.norm.rank$predicted_probabilities <- nb_pred
nb.ranked_data_frame <- nb.valid.df.norm.rank[order(-nb.valid.df.norm.rank$predicted_probabilities),]
nb.ranked_data_frame$cumulative_positives <- cumsum(nb.valid.df.norm.rank$actual)
total_positives <- sum(nb.ranked_data_frame$actual)
nb.ranked_data_frame$percentage_of_positives <- nb.ranked_data_frame$cumulative_positives / total_positives * 100
nb.ranked_data_frame$percentage_of_cases <- seq_along(nb.ranked_data_frame$actual) / nrow(nb.ranked_data_frame) * 100

ggplot(nb.ranked_data_frame, aes(x = percentage_of_cases, y = percentage_of_positives)) +
  geom_line(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Percentage of Cases Targeted", y = "Percentage of Positives Captured", title = "Cumulative Gains Chart for Naive Bayes") +
  theme_minimal()

# Cumulative Lift chart
#_______________________

# For Logistic Regression
ranked_data_frame$cumulative_positives <- cumsum(ranked_data_frame$actual)
total_positives <- sum(ranked_data_frame$actual)
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

# For Naive Bayes
nb.ranked_data_frame$cumulative_positives <- cumsum(nb.ranked_data_frame$actual)
nb.total_positives <- sum(nb.ranked_data_frame$actual)
nb.ranked_data_frame$response_rate <- nb.ranked_data_frame$cumulative_positives / seq_along(nb.ranked_data_frame$actual)
nb.overall_response_rate <- nb.total_positives / nrow(nb.ranked_data_frame)
nb.ranked_data_frame$cumulative_lift <- nb.ranked_data_frame$response_rate / nb.overall_response_rate
nb.ranked_data_frame$percentage_of_cases <- seq_along(nb.ranked_data_frame$actual) / nrow(nb.ranked_data_frame) * 100

ggplot(nb.ranked_data_frame, aes(x = percentage_of_cases, y = cumulative_lift)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Percentage of Cases Targeted", y = "Cumulative Lift", 
       title = "Cumulative Lift Chart for Naive Bayes") +
  theme_minimal()
########################################################################################################################################################
#                                     REGRESSION MODEL SELECTION
########################################################################################################################################################
## considering only the loan purchasers as the spending for non-purchasers is zero

##################################
## (a) MULTIPLE LINEAR REGRESSION
##################################

# fit the model on training data
lm_model <- lm(train.df.labels$spending ~ ., data = train.df.norm)
summary(lm_model)
# Stepwise regression
stepwise_reg_model <- step(lm_model, direction = "both")
summary(stepwise_reg_model)
lm_model_2 <- lm(train.df.labels$spending ~ USCustomer + sourceA + 
                   sourceH + sourceR + sourceU + frequency + lastUpdate + isResAddr + 
                   sourceC, data = train.df.norm)
summary(lm_model_2)
# predict on validation data
lm_pred <- predict(lm_model_2, valid.df.norm)
all.residuals <- valid.df.labels$spending - lm_pred

ggplot() +
  geom_histogram(aes(x=all.residuals), fill="lightgray", color="grey") +
  labs(x="Residuals", y="Frequency")

summary(lm_pred)
summary(valid.df.labels$spending)
mean(valid.df.labels$spending)
cor(lm_pred, valid.df.labels$spending)

MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(valid.df.labels$spending, lm_pred)

##################################
## (b) REGRESSION TREE
##################################

# fit the model on training data
reg_tree_model<-rpart(train.df.labels$spending ~ ., data=train.df.norm)
reg_tree_model
library(rattle)
fancyRpartPlot(reg_tree_model, cex = 0.7)

# predict on validation data
reg_tree_pred<-predict(reg_tree_model, valid.df.norm)
summary(reg_tree_pred)
summary(valid.df.labels$spending)
cor(reg_tree_pred, valid.df.labels$spending)

MAE(valid.df.labels$spending, reg_tree_pred)


########################################################################################################################################################
#                                     PREDICTIONS
########################################################################################################################################################

## Using Classification tree as the classification model to make classifications on holdout set
predict_class_holdout <- predict(log_model_3,holdout.df.norm,type = "response")
predict_class_holdout

conf_matrix <- confusionMatrix(factor(ifelse(predict_class_holdout > 0.5, 1, 0), levels = c(1,0)),
                               factor(holdout.df.labels$purchase, levels = c(1,0)))
conf_matrix

## Add a column to the data frame with the predicted probability of purchase
holdout.df.norm$predicted_prob <- predict_class_holdout
## Add a column to the data frame with the adjusted probability of purchase
holdout.df.norm$adjusted_prob <- holdout.df.norm$predicted_prob * 0.1065


## Using regression tree as the regression  to make predictions on holdout set
predict_reg_holdout<-predict(lm_model_2, holdout.df.norm)
predict_reg_holdout
summary(holdout.df.labels$spending)
mean(holdout.df.labels$spending)
cor(predict_reg_holdout, holdout.df.labels$spending)

MAE(holdout.df.labels$spending, predict_reg_holdout)
mean(holdout.df.labels$spending)
## Add a column to the data frame with the predicted spending
holdout.df.norm$predicted_spending <- predict_reg_holdout

## Add a column to the data frame with the expected spending
holdout.df.norm$expected_spending <- holdout.df.norm$predicted_spending * holdout.df.norm$adjusted_prob

# cumulative lift chart
library(gains)
price <- holdout.df.labels$spending
gain <- gains(price, holdout.df.norm$expected_spending)

df <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumPrice=c(0, gain$cume.pct.of.total * sum(price))
)

ggplot(df, aes(x=ncases, y=cumPrice)) +
  geom_line(color = "steelblue") +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout.df)), cumPrice=c(0, sum(price))),
            color="red", linetype=2) + # adds baseline
  labs(x="# Cases", y="Cumulative expected spending", title="Cumulative gains chart") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#########################################################################
#baseline revenue
mailing_cost <- 180000 * 2
expected_purchasers <- 180000 * 0.053
average_spending <- mean(npdata$spending)
expected_sales <- expected_purchasers * average_spending
gross_profit = expected_sales - mailing_cost
# 619037.7
  
# model revenue
model.average_spending <- mean(holdout.df.norm$expected_spending)
model.expected_sales <- model.average_spending * 180000
model.gross_profit = model.expected_sales - mailing_cost
#1,630,902

