########################################################################################################################################################
## Author: Lavanya Govindarajan
## Date: May 08, 2024
########################################################################################################################################################

# install.packages("skimr")
# install.packages("purr")
# install.packages("mice")
# install.packages("VIM")
# install.packages("paletteer")
# install.packages("wesanderson")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("corrplot")
# install.packages("Boruta")
# install.packages("plotly")
# install.packages("caret")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("fastDummies")
# install.packages("rattle")
# install.packages("neuralnet")
library(skimr)
library(purrr)
library(mice)
library(VIM)
library(paletteer)
library(wesanderson)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(Boruta)
library(plotly)
library(caret)
library(e1071)
library(fastDummies)
library(rpart)
library(rpart.plot)
library(rattle)
library(neuralnet)

########################################################################################################################################################
#                                    DATA EXPLORATION
########################################################################################################################################################
#load the used mortgage dataset
mtg.df <- read.csv("Mortgage.csv", header = TRUE, sep = ",")

# exploring the structure and content of the data
dim(mtg.df)
head(mtg.df,10)
tail(mtg.df,10)

# understanding data types and statistics
str(mtg.df)
skim(mtg.df) 
summary(mtg.df)
## All the fields are of numeric. 

###########################
# MISSING VALUE ANALYSIS
###########################

# Check for missing values
mtg.df %>% 
  map(is.na) %>%
  map(sum)

# list the 270 observations with null values. Observations 39723 to 39737 and 49658
missing.df <- mtg.df[is.na(mtg.df$LTV_time), ]

# visualizing missing values
md.pattern(mtg.df, rotate.names = TRUE)
aggr(mtg.df, prop = FALSE, numbers = TRUE, cex.axis=0.6)

# Dropping the null values
mtg.df.imp <- mtg.df[!is.na(mtg.df$LTV_time), ]

# check again for NAs on the imputed dataset
mtg.df.imp %>% 
  map(is.na) %>%
  map(sum)
# all the missing values have been successfully dropped. 

###########################
# ZERO VALUE CHECK
###########################
# check for zeros
zeros <- colSums(mtg.df.imp == 0, na.rm = TRUE)
zeros

########################################################################################################################################################
#                                    DATA PRE-PROCESSING
########################################################################################################################################################
# grouping data based by customer with the latest time.
mtg.grouped.df <- as.data.frame(mtg.df.imp %>% arrange(id, desc(time)) %>% group_by(id) %>% filter(time == max(time)) %>% slice_tail() %>% ungroup())

# expected_loan_period : Represents the anticipated length of the loan period from origination to maturity.
# calculated by subtracting the origination time from the maturity time. 
# It provides an estimate of how long the loan is expected to last based on the loan terms.

# actual_loan_period: Denotes the observed length of the loan period from origination to the current observation time. 
# It is calculated by subtracting the origination time from the current observation time.
# It reflects the actual duration of the loan up to the present moment based on observed data.
# For customers who have defaulted, it would be the default_time.

# observed_loan_period_ptg: Represents the percentage of the loan period elapsed until the customer defaulted.
mtg.grouped.df$expected_loan_period <- mtg.grouped.df$mat_time - mtg.grouped.df$orig_time
mtg.grouped.df$observed_loan_period <- mtg.grouped.df$time - mtg.grouped.df$orig_time

# Excluding the customer 13374 to avoid divide by zero error
mtg.grouped.df[mtg.grouped.df$expected_loan_period == 0,]
mtg.grouped.df <- mtg.grouped.df[mtg.grouped.df$id != 13374, ]
mtg.grouped.df$observed_loan_period_ptg <- (mtg.grouped.df$observed_loan_period / mtg.grouped.df$expected_loan_period)*100

# Splitting the grouped data into active and default/pay-off customers
mtg.active.cust.df <- as.data.frame(mtg.grouped.df[mtg.grouped.df$status_time == 0, ])
mtg.inactive.cust.df  <- as.data.frame(mtg.grouped.df[mtg.grouped.df$status_time != 0, ])

# Only default customers
mtg.default.cust.df <- as.data.frame(mtg.inactive.cust.df[mtg.inactive.cust.df$default_time == 1, ])

# Creating a copy for cluster Analysis
mtg.grouped.cluster <- mtg.grouped.df

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
par(mfcol=c(4, 4))
boxplot(mtg.grouped.df$time, xlab="Time Stamp of Observation", ylab="Values", col=discrete_palette[1])
boxplot(mtg.grouped.df$orig_time, xlab="Origination Time Stamp", ylab="Values", col=discrete_palette[2])
boxplot(mtg.grouped.df$first_time, xlab="First Observation Time Stamp", ylab="Values", col=discrete_palette[3])
boxplot(mtg.grouped.df$mat_time, xlab="Maturity Time Stamp", ylab="Values", col=discrete_palette[4])
boxplot(mtg.grouped.df$balance_time, xlab="Outstanding Balance at Observation Time", ylab="Values", col=discrete_palette[1])
boxplot(mtg.grouped.df$LTV_time, xlab="Loan-to-Value Ratio at Observation Time (%)", ylab="Values", col=discrete_palette[2])
boxplot(mtg.grouped.df$interest_rate_time, xlab="Interest Rate at Observation Time (%)", ylab="Values", col=discrete_palette[3])
boxplot(mtg.grouped.df$hpi_time, xlab="House Price Index at Observation Time", ylab="Values", col=discrete_palette[4])
boxplot(mtg.grouped.df$gdp_time, xlab="Gross Domestic Product (GDP) Growth at Observation Time (%)", ylab="Values", col=discrete_palette[1])
boxplot(mtg.grouped.df$uer_time, xlab="Unemployment Rate at Observation Time (%)", ylab="Values", col=discrete_palette[2])
boxplot(mtg.grouped.df$balance_orig_time, xlab="Outstanding Balance at Origination Time", ylab="Values", col=discrete_palette[3])
boxplot(mtg.grouped.df$FICO_orig_time, xlab="FICO Score at Origination Time (%)", ylab="Values", col=discrete_palette[4])
boxplot(mtg.grouped.df$LTV_orig_time, xlab="Loan-to-Value Ratio at Origination Time (%)", ylab="Values", col=discrete_palette[1])
boxplot(mtg.grouped.df$Interest_Rate_orig_time, xlab="Interest Rate at Origination Time (%)", ylab="Values", col=discrete_palette[2])
boxplot(mtg.grouped.df$hpi_orig_time, xlab="House Price Index at Origination Time", ylab="Values", col=discrete_palette[3])

################################
# CATEGORICAL VARIABLE ANALYSIS
################################
yes_color <- "#8074A8FF"
no_color <- "#C46487FF" 

#REtype_CO_orig_time    
count_CO <- mtg.grouped.df %>%
  group_by(REtype_CO_orig_time) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
plot_CO <- ggplot(count_CO, aes(x = "", y = Percentage, fill = factor(REtype_CO_orig_time))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("0" = no_color, "1" = yes_color), labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Condominium") +
  theme(legend.position = "bottom")

# REtype_PU_orig_time
count_PU <- mtg.grouped.df %>%
  group_by(REtype_PU_orig_time) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
plot_PU <- ggplot(count_PU, aes(x = "", y = Percentage, fill = factor(REtype_PU_orig_time))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("0" = no_color, "1" = yes_color), labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Planned Urban Development")+
  theme(legend.position = "bottom")

# REtype_SF_orig_time
count_SF <- mtg.grouped.df %>%
  group_by(REtype_SF_orig_time) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
plot_SF <- ggplot(count_SF, aes(x = "", y = Percentage, fill = factor(REtype_SF_orig_time))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("0" = no_color, "1" = yes_color), labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Single Family") +
  theme(legend.position = "bottom")

# Arrange plots in a single row
grid.arrange(plot_CO, plot_PU, plot_SF, nrow = 1, top = "Distribution of Property Types")

# investor_orig_time
percentages_investor <- mtg.grouped.df %>%
  group_by(investor_orig_time) %>%
  summarize(percentage = n() / nrow(mtg.grouped.df) * 100)
ggplot(mtg.grouped.df, aes(x = factor(investor_orig_time), fill = factor(investor_orig_time))) +
  geom_bar(color = "#C46487FF") +
  geom_text(data = percentages_investor, aes(label = paste0(round(percentage, 1), "%"), y = percentage, group = investor_orig_time),
            position = position_stack(vjust = 300), color = "black", size = 5) +
  scale_fill_manual(values = discrete_palette) +
  labs(x = "Investor Borrower Type", y = "Count", title = "Distribution of Investor Borrower Type") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none") +
  scale_x_discrete(labels = c("No", "Yes"))

# default_time
percentages <- mtg.grouped.df %>%
  group_by(status_time) %>%
  summarize(percentage = n() / nrow(mtg.grouped.df) * 100)

ggplot(mtg.grouped.df, aes(x = factor(status_time), fill = factor(status_time))) +
  geom_bar(color = "#C46487FF") +
  geom_text(data = percentages, aes(label = paste0(round(percentage, 1), "%"), y = percentage, group = status_time),
            position = position_stack(vjust = 300), color = "black", size = 5) +
  scale_fill_manual(values = discrete_palette) +
  labs(x = "Borrower Status", y = "Count", title = "Distribution of Borrower Status") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none") +
  scale_x_discrete(labels = c("Active", "Default", "Payoff"))

    
################################
# NUMERICAL VARIABLE ANALYSIS
################################
# observed time 
summary(mtg.grouped.df["time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = time , fill = after_stat(count)), binwidth = 0.5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# orig_time 
summary(mtg.grouped.df["orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = orig_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# first_time 
summary(mtg.grouped.df["first_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = first_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of first time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# mat_time 
summary(mtg.grouped.df["mat_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = mat_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of mature time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# LTV_time 
summary(mtg.grouped.df["LTV_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = LTV_time, fill = after_stat(count)), binwidth = 10, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of LTV ratio at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# interest_rate_time 
summary(mtg.grouped.df["interest_rate_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = interest_rate_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of interest rates at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# hpi_time 
summary(mtg.grouped.df["hpi_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = hpi_time, fill = after_stat(count)), binwidth = 5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of HPI at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# balance_time
summary(mtg.grouped.df["balance_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = balance_time, fill = after_stat(count)), binwidth = 100000, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of Balance  at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# gdp_time
summary(mtg.grouped.df["gdp_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = gdp_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of GDP at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# uer_time
summary(mtg.grouped.df["uer_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = uer_time, fill = after_stat(count)), binwidth = 0.5, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of Unemployment rates at observed time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# FICO_orig_time
summary(mtg.grouped.df["FICO_orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = FICO_orig_time, fill = after_stat(count)), binwidth = 100, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of FICO at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# LTV_orig_time
summary(mtg.grouped.df["LTV_orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = LTV_orig_time, fill = after_stat(count)), binwidth = 10, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of LTV ratio at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# Interest_Rate_orig_time
summary(mtg.grouped.df["Interest_Rate_orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = Interest_Rate_orig_time, fill = after_stat(count)), binwidth = 1, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of interest rate at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# hpi_orig_time
summary(mtg.grouped.df["hpi_orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = hpi_orig_time, fill = after_stat(count)), binwidth = 10, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of HPI at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# balance_orig_time
# (<100000 -> 11432) <500000 -> 45590, 500k and 1M -> 4074, >1M 318
summary(mtg.grouped.df["balance_orig_time"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = balance_orig_time, fill = after_stat(count)), binwidth = 100000, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of Balance at origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# expected_loan_period
summary(mtg.grouped.df["expected_loan_period"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = expected_loan_period, fill = after_stat(count)), binwidth = 10, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of Expected Loan Period at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")

# observed_loan_period
summary(mtg.grouped.df["observed_loan_period"])
ggplot(data = mtg.grouped.df)+
  geom_histogram(mapping = aes(x = observed_loan_period, fill = after_stat(count)), binwidth = 10, color = "#FF8989")+ 
  scale_fill_gradientn(colors = spectrum_palette) +
  labs(title="Distribution of Observed Loan Period at loan origin time") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "none")


########################################################################################################################################################
#                                    PREDICTOR ANALYSIS AND RELEVANCY
########################################################################################################################################################

################################
# CORRELATION ANALYSIS
################################
# correlation plot
mtg_corr<- cor(mtg.grouped.df[, -1] %>% mutate_if(is.factor, as.numeric))
corrplot(mtg_corr, method = "color")
# simplified correlation plot
mtg_corr_simple <- function(data=mtg.grouped.df,sig=0.3){
  corr <- cor(mtg.grouped.df %>% mutate_if(is.factor, as.numeric))
  corr[corr == 1] <- NA
  corr <- as.data.frame(as.table(corr))
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method = "number")
}
mtg_corr_simple()

# target variables (default_time) correlation
numeric_df <- mtg.grouped.df[sapply(mtg.grouped.df, is.numeric)] 
numeric_df <- numeric_df[, -26]
target_default_cor <- cor(mtg.grouped.df$default_time, numeric_df)
corrplot(target_default_cor, method = "number")

################################
# VARIABLE SELECTION
################################
#1. Using Boruta
#_________________
set.seed(1234)
mtg.df.boruta <- Boruta(default_time ~ . -id - observed_loan_period_ptg, data = mtg.grouped.df, doTrace = 0)
print(mtg.df.boruta)
# printing important scores
mtg.df.boruta$ImpHistory[1:20,]
# box plot for important scores
df_long <- tidyr::gather(as.data.frame(mtg.df.boruta$ImpHistory), feature, measurement)
plot_ly(df_long, y = ~measurement, color = ~feature, type = "box") %>%
  layout(title="Box-and-whisker Plots",
         xaxis = list(title="Features"),
         yaxis = list(title="Importance"),
         showlegend=F)
# get the confirmed variables
mtg.df.predictor.boruta <- getSelectedAttributes(mtg.df.boruta, withTentative = F)
mtg.df.predictor.boruta


########################################################################################################################################################
#                                   DATA PARTITIONING
########################################################################################################################################################

## For Classification
## partitioning into training (60%) and validation (40%)
## Splitting output and predictor variables
set.seed(1234)
Y <- data.frame(default_time = mtg.inactive.cust.df[, 21])
X <- mtg.inactive.cust.df[, -c(1,21,22,23,24,25,26)] 

## creating training and validation partition indices
train.index <-sample(seq_len(nrow(mtg.inactive.cust.df)), size = 0.7*nrow(mtg.inactive.cust.df))
valid.index <- sample(setdiff(seq_len(nrow(mtg.inactive.cust.df)), train.index))

## creating training and validation partition dataset
mtg.train.df <- X[train.index, ]
mtg.valid.df <- X[valid.index, ]

mtg.train.df.labels <- Y[train.index, ]
mtg.valid.df.labels <- Y[valid.index, ]

## For Regression
set.seed(1234)
RY <- data.frame(observed_loan_period_ptg = mtg.default.cust.df[ , 26])
RX <- mtg.default.cust.df[, -c(1,21,22,23,24,25,26)] 

## creating training and validation partition indices
reg.train.index <-sample(seq_len(nrow(mtg.default.cust.df)), size = 0.7*nrow(mtg.default.cust.df))
reg.valid.index <- sample(setdiff(seq_len(nrow(mtg.default.cust.df)), reg.train.index))

## creating training and validation partition dataset
reg.mtg.train.df <- RX[reg.train.index, ]
reg.mtg.valid.df <- RX[reg.valid.index, ]

reg.mtg.train.df.labels <- RY[reg.train.index, ]
reg.mtg.valid.df.labels <- RY[reg.valid.index, ]

########################################################################################################################################################
#                                    CLASSIFICATION MODEL SELECTION
########################################################################################################################################################

###############################
## (a) CLASSIFICATION TREES
###############################
# Fit a tree model on training data
rpart_model <- rpart(mtg.train.df.labels  ~ . , data=mtg.train.df, method="class")
rpart_model
# plot the tree
rpart.plot(rpart_model, extra=1, fallen.leaves=FALSE)
# predict on validation data
rpart_pred <- predict(rpart_model, mtg.valid.df, type = "class")
# confusion matrix
class_conf_matrix <- confusionMatrix(factor(rpart_pred, levels = c(1,0)), factor(mtg.valid.df.labels, levels = c(1,0)), positive = "1")
class_conf_matrix

###############################
## (b) LOGISTIC REGRESSION
###############################

# Fit Logistic Regression model on training data
logistic_model <- glm(mtg.train.df.labels ~ ., family = binomial(), data = mtg.train.df)
# Evaluate goodness of fit
summary(logistic_model)
# Use the fitted model for evaluation on validation data
log_pred <- predict(logistic_model, newdata = mtg.valid.df, type = "response")
# Evaluate Predictive Accuracy
conf_matrix <- confusionMatrix(factor(ifelse(log_pred > 0.5, 1, 0), levels = c(1,0)),
                               factor(mtg.valid.df.labels, levels = c(1,0)))
conf_matrix

###############################
## (c) NAIVE BAYES
###############################
# Fit Naive Bayes model on training data
nb_model <- naiveBayes(mtg.train.df.labels ~ ., data = mtg.train.df)
nb_model
# Use the fitted model for evaluation on validation data
nb_pred <- predict(nb_model, newdata = mtg.valid.df)
# Convert predicted and actual outcomes to factors with explicit levels
nb_pred_factor <- factor(nb_pred, levels = c("1", "0"))
nb_actual_factor <- factor(mtg.valid.df.labels, levels = c("1", "0"))
# Evaluate Predictive Accuracy
# Confusion Matrix
confusionMatrix(nb_pred_factor, nb_actual_factor, positive = "1")

########################################################################################################################################################
#                                     REGRESSION MODEL SELECTION
########################################################################################################################################################

##################################
## (a) MULTIPLE LINEAR REGRESSION
##################################
# fit the model on training data
lm_model <- lm(reg.mtg.train.df.labels ~ ., data = reg.mtg.train.df)
# evaluate goodness of fit
summary(lm_model)

# optimizing the model by considering only the significant predictors
lm_model_1 <- lm(reg.mtg.train.df.labels ~ . - interest_rate_time - REtype_CO_orig_time - Interest_Rate_orig_time, data = reg.mtg.train.df)
summary(lm_model_1)

ggplot() +
  geom_histogram(aes(x=lm_model_1$residuals), fill="lightgray", color="grey") +
  labs(x="Residuals", y="Frequency")
summary(lm_model_1$residuals)

# predict on validation data
lm_pred <- predict(lm_model_1, reg.mtg.valid.df)
all.residuals <- reg.mtg.valid.df.labels - lm_pred

ggplot() +
  geom_histogram(aes(x=all.residuals), fill="lightgray", color="grey", binwidth = 4) +
  labs(x="Residuals", y="Frequency")
summary(all.residuals)

# Evaluate Predictive Accuracy
summary(lm_pred)
summary(reg.mtg.valid.df.labels)
mean(reg.mtg.valid.df.labels)
cor(lm_pred, reg.mtg.valid.df.labels)
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(reg.mtg.valid.df.labels, lm_pred)
forecast::accuracy(reg.mtg.valid.df.labels, lm_pred)

##################################
## (b) REGRESSION TREE
##################################

# fit the model on training data
reg_tree_model<-rpart(reg.mtg.train.df.labels ~ ., data=reg.mtg.train.df)
reg_tree_model
fancyRpartPlot(reg_tree_model, cex = 0.7)

# predict on validation data
reg_tree_pred<-predict(reg_tree_model, reg.mtg.valid.df)

# Evaluate Predictive Accuracy
summary(reg_tree_pred)
summary(reg.mtg.valid.df.labels)
cor(reg_tree_pred, reg.mtg.valid.df.labels)
MAE(reg.mtg.valid.df.labels, reg_tree_pred)
forecast::accuracy(reg.mtg.valid.df.labels, reg_tree_pred)

##################################
## (c) NEURAL NETWORKS
##################################

# fit the model on training data
set.seed(45)
nn_model <- neuralnet(reg.mtg.train.df.labels ~ ., data = reg.mtg.train.df, linear.output = T, hidden = 4)

nn_model$weights
plot(nn_model)

# predict on validation data
pred_nn <- compute(nn_model, reg.mtg.valid.df)

# Evaluate Predictive Accuracy
MAE(reg.mtg.valid.df.labels, pred_nn$net)
forecast::accuracy(reg.mtg.valid.df.labels, pred_nn$net)

########################################################################################################################################################
#                                     CLUSTER ANALYSIS
########################################################################################################################################################

## data pre-processing for cluster analysis
mtg.grouped.cluster <- mtg.grouped.cluster[, -c(1,2,3,4,5,20,21,22,24,25)]
# creating dummy variables for REMODEL
mtg.grouped.cluster <- dummy_cols(mtg.grouped.cluster, select_columns = "status_time", remove_selected_columns = TRUE)
## scaling data 
mtg.grouped.cluster.norm <- sapply(mtg.grouped.cluster, scale)

## applying k-means clustering algorithm by choosing 3 as the predefined no of clusters.
set.seed(1234)
mtg.clusters <- kmeans(mtg.grouped.cluster.norm, 3)

mtg.clusters$centers
mtg.clusters$cluster
mtg.clusters$size

## bar plot for normalized data                                                   
barplot(mtg.clusters$centers, beside = TRUE, ylab = "Index", xlab = "Value", col = discrete_palette[1:3],
        las = 3, cex.axis = 0.7, cex.names = 0.7,
        names.arg = c("balance", "LTV", "interest", "hpi", "gdp", "uer", "CO", "PU", "SF", "investor", 
                      "Obalance", "OFICO","OLTV","OInterest","loanPeriod","active", "default", "payoff"))
legend("topleft", ncol=3, cex = 1, legend = c("Cluster 1", "Cluster 2", "Cluster 3"), 
       fill = discrete_palette[1:3])

########################################################################################################################################################
#                                    PREDICTION
########################################################################################################################################################

## Predictions on the active customers
## selecting Naive Bayes for classification
predict_nb_active_cust <- predict(nb_model, newdata = mtg.active.cust.df[,-c(26,25,24,23,22,21,1)])
summary(predict_nb_active_cust)

## 3228 out of 8251 (39.12253%) customers identified as default
mtg.active.cust.todefault.df <- mtg.active.cust.df[predict_nb_active_cust == 1, ]
mtg.active.cust.toNotDefault.df <- mtg.active.cust.df[predict_nb_active_cust == 0, ]

## Visualizing the default customers distribution
## Add predicted target values back to the dataframe.
mtg.active.cust.df$predicted_target <- predict_nb_active_cust
mtg.active.cust.df$balance_time_millions <- mtg.active.cust.df$balance_time / 1000000
plot(mtg.active.cust.df$observed_loan_period, mtg.active.cust.df$balance_time_millions, 
     col = "darkgrey",
     xlab = "Time", ylab = "Outstanding Balance at Time (Millions)",
     main = "Time vs Balance Time")
default_points <- mtg.active.cust.df[mtg.active.cust.df$predicted_target == "1", ]
points(default_points$observed_loan_period, default_points$balance_time_millions, col = discrete_palette[3])


## selecting Multiple Linear Regression for prediction
predict_reg_active_cust<-predict(lm_model, mtg.active.cust.todefault.df)
predict_reg_active_cust

## Add a column to the data frame with the predicted default_payoff_percentage
mtg.active.cust.todefault.df$default_loan_period_ptg <- predict_reg_active_cust
## Taking 25% as the threshold for classifying as high or low risk, all the default customers are of high default risk
nrow(mtg.active.cust.todefault.df[mtg.active.cust.todefault.df$default_loan_period_ptg < 25, ])

# Calculate percentage of customers within the range 25-35
customers_within_range <- sum(mtg.active.cust.todefault.df$default_loan_period_ptg >= 25 & mtg.active.cust.todefault.df$default_loan_period_ptg <= 35) / nrow(mtg.active.cust.todefault.df) * 100
customers_within_range

# Distribution of the default time percentage
ggplot() +
  geom_histogram(data = mtg.active.cust.todefault.df, aes(x=default_loan_period_ptg, fill = (default_loan_period_ptg >= 25 & default_loan_period_ptg <= 35)), color="grey", binwidth = 0.5) +
  scale_fill_manual(values = c("grey", discrete_palette[3]), name = NULL, labels = c("Other", "Majority defaults")) +
  theme_minimal() +
  labs(x="Default Time Percentage", y="Frequency") +
  annotate("text", x = 30, y = 50, label = paste0(round(customers_within_range, 2), "%"), color = "black") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.position = "bottom")











