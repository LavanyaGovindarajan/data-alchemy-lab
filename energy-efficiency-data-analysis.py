import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns

####################################################################################################################
#                               STEP 1: Datset Introduction                                                        #
####################################################################################################################

# Dataset Link: https://www.kaggle.com/datasets/elikplim/eergy-efficiency-dataset
# The “Energy Efficiency” dataset was downloaded from Kaggle.com. 
# The dataset is a rich compilation of data points curated with the aim of understanding and optimizing energy usage
# patterns in buildings. 
# It comprises of 768 observations and 10 fields (all of which are numeric) with two responses(Heating Load and Cooling Load).
# The aim is to use the eight features to predict the Cooling_load.

#Field description            
#Relative_Compactness         : measures the compactness of a building. A higher compactness often means a lower surface area 
#                               exposed to the environment.
#Surface_Area                 : total exterior surface area of the building.              
#Wall_Area                    : total area of all the exterior walls of a building.
#Roof_Area                    : total area of the roof of a building.
#Overall_Height               : measurement of the building's height.
#Orientation                  : directional alignment of the building (North, East, South, West).
#Glazing_Area                 : total area of the building that is covered by windows or other glazing systems.
#Glazing_Area_Distribution    : distribution of windows/glazing area on different faces of the building.
#Heating_Load [response]      : amount of energy required to heat the building to a comfortable temperature. 
#Cooling Load [response]      : amount of energy required to cool the building to a comfortable temperature.

import os
#To find the present working directory
os.getcwd()

####################################################################################################################
#                               STEP 2: EDA                                                                        #
####################################################################################################################
#Read the CSV file into the dataframe df
df = pd.read_csv('energy_efficiency_data.csv')
#Display the first 10 rows of the dataframe
df.head(10)

#Display the last 10 rows of the dataframe
df.tail(10)

#View the Summary statistics
df.describe()

#Check for null or missing values
print(df.isnull().sum()) #There are no null values

#Check for duplicate values
duplicate_count = df.duplicated().sum()
print(duplicate_count)#There are no duplicates

# Creating dummy variables for 'Orientation' and 'Glazing_Area_Distribution' as they both represent categorical data
orientation_dummies = pd.get_dummies(df['Orientation'], prefix='Ort', drop_first=True)
glazingdist_dummies = pd.get_dummies(df['Glazing_Area_Distribution'], prefix='GlaDist', drop_first=True)

# Add the dummy variables to the original DataFrame
df = pd.concat([df, orientation_dummies], axis=1)
df = pd.concat([df, glazingdist_dummies], axis=1)

# Now, 'df' contains new columns for each category of 'Orientation'
df.head()

#Generating Histograms for each variable
variables = df.columns.tolist()
for variable in variables:
    plt.figure(figsize=(8, 4))
    plt.hist(df[variable], bins=10)
    plt.title('Histogram')
    plt.xlabel(variable)
    plt.ylabel('Frequency')
    plt.show()  

#Visualizing the correlation between variables using a heatmap
fig, ax = plt.subplots(figsize=(20,10))  
sns.heatmap(df.corr(), annot=True, cmap='coolwarm', vmin=-1, vmax=1)
plt.title('Correlation Heatmap')
plt.show()

# Some of the strong correlations w.r.t Cooling_Loadfrom the heatmap are as following
# Strong positive correlation between Cooling_Load and Overall_Height
# Strong positive correlation between Cooling_Load and Relative_Compactness
# Strong positive correlation between Cooling_Load and Wall_Area
# Strong negative correlation between Cooling_Load and Surface_Area
# Strong negative correlation between Cooling_Load and Roof_Area
  
#Here, we are trying to determine the effects of various factors on the Cooling Load, hence the dependant variable would be 'Cooling_Load'.
#Generating scatter plots of the dependant variable with all the other independant variables.
#Categorical variable breaks this, so it is removed.

def create_scatter_plots(df, y_variable):
    x_variables = df.columns.tolist()
    categorical_variables = ['Ort_3', 'Ort_4', 'Ort_5', 'GlaDist_1', 'GlaDist_2', 'GlaDist_3', 'GlaDist_4', 'GlaDist_5']
    x_variables = [x for x in x_variables if x not in categorical_variables]
    for x_variable in x_variables:
        if x_variable != y_variable:
            x = df[x_variable]
            y = df[y_variable]

            slope, intercept = np.polyfit(x, y, 1)
            trend_line = slope * x + intercept

            plt.scatter(x, y)
            plt.plot(x, trend_line, color='orange', label='Trend Line')
            plt.xlabel(x_variable)
            plt.ylabel(y_variable)
            plt.title(f"Scatter Plot: {x_variable} vs {y_variable}")
            plt.legend()
            plt.show()
            
create_scatter_plots(df,'Cooling_Load')

####################################################################################################################
#                               STEP 3: Performing Linear Regression                                               #
####################################################################################################################

# Following a step-wise backward approach.
# Adding all the variables and analyzing the r^2, adjusted r^2, regression co-efficients and p-values for the 
# dependant variable 'Cooling_Load'

model1 = sm.OLS.from_formula('Cooling_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Glazing_Area + Orientation + Ort_3 + Ort_4 + Ort_5 + Glazing_Area_Distribution + GlaDist_1 + GlaDist_2 + GlaDist_3 + GlaDist_4 + GlaDist_5', df)
results1 = model1.fit() #Runs the regression
results1.summary()

# From the model result, we can derive the following:

# (a) R-squared and Adjusted R-squared are used to assess the goodness-of-fit of the model or how close the data are to the 
# fitted regression line. Both are at 0.89 and 0.88 respectively, indicating a good fit. The adjusted R-squared is very close to the 
# R-squared, which suggests that the model is not overly complex given the data.

# (b) Looking at the coefficient column for variables with significant impacts, each unit increase in the Relative_Compactness, 
# the dependent variable(y) decreases by 70.7877 units, all else being equal. Similarly, for each unit increase in Glazing_Area, 
# y increases by 13.25 units and for each unit increase in Overall_Height, y increase by 4.28 units. Additionally Ort_3(East), Ort_4(South), and Ort_5(West)
# also show negative correlation with the dependent variable(y)

# (c) A smaller p-value (<0.05 typically) indicates strong evidence against the null hypothesis, suggesting the predictor is 
# meaningful. The predictor Glazing_Area_Distribution are not statistically significant at the 5% level, suggesting that they do not have a meaningful effect on the dependent variable.

# On analyzing the regression co-efficients and p-values of the independant variables from model1 results, retaining only
# the variables with significant impacts on the dependant variable.

modelOrt = sm.OLS.from_formula('Cooling_Load ~ Relative_Compactness + Overall_Height + Glazing_Area + Ort_3 + Ort_4 + Ort_5' , df) 
resultsOrt = modelOrt.fit() #Runs the regression
resultsOrt.summary()

# Orientation variables don't have meaningful effect as they have higher p-values, hence removing them for model2.
model2 = sm.OLS.from_formula('Cooling_Load ~ Relative_Compactness + Overall_Height + Glazing_Area', df) 
results2 = model2.fit()
results2.summary()

####################################################################################################################
#                               STEP 4: Model generation                                                           #
####################################################################################################################
# (a) Linear Log regression

# Applying log tranformation for the dependant variables 
df['log_Relative_Compactness'] = np.log(df['Relative_Compactness'])
df['log_Overall_Height'] = np.log(df['Overall_Height'])

# Glazing_Area has zeros causing divide by zero errors. Adding a small positive value 1e-10 will help overcome the error.
df['Glazing_Area'] = df['Glazing_Area'] + 1e-10
df['log_Glazing_Area'] = np.log(df['Glazing_Area'])


# create the model for Linear Log regression
linear_log_model = sm.OLS.from_formula('Cooling_Load ~ log_Relative_Compactness + log_Overall_Height + log_Glazing_Area', df) 
linear_log_results = linear_log_model.fit()
linear_log_results.summary()

# (b) Log Linear regression

# Applying log tranformation for the independant variable
df['log_Cooling_Load'] = np.log(df['Cooling_Load'])

# create the model for Log Linear regression
log_linear_model = sm.OLS.from_formula('log_Cooling_Load ~ Relative_Compactness + Overall_Height + Glazing_Area', df) 
log_linear_results = log_linear_model.fit()
log_linear_results.summary()

# (a) Log Log regression

# using the dependant and independant variables that were previously transformed with log.
log_log_model = sm.OLS.from_formula('log_Cooling_Load ~ log_Relative_Compactness + log_Overall_Height + log_Glazing_Area', df) 
log_log_results = log_log_model.fit()
log_log_results.summary()

# Comparing Models

models = ['model1','model2','linear_log_model','log_linear_model','log_log_model']
r_squared = [results1.rsquared, results2.rsquared, linear_log_results.rsquared, log_linear_results.rsquared, log_log_results.rsquared]
adj_r_squared = [results1.rsquared_adj, results2.rsquared_adj, linear_log_results.rsquared_adj, log_linear_results.rsquared_adj, log_log_results.rsquared_adj]
aic = [results1.aic, results2.aic, linear_log_results.aic, log_linear_results.aic, log_log_results.aic]
bic = [results1.bic, results2.bic, linear_log_results.bic, log_linear_results.bic, log_log_results.bic]

data = {
    'Model': models,
    'R-squared': r_squared,
    'Adjusted R-squared': adj_r_squared,
    'AIC': aic,
    'BIC': bic
}
df_stats = pd.DataFrame(data)
print(df_stats)

# log_linear_model seems to be the best model.
# As it provides the highest R-squared and Adjusted R-squared values, indicating superior explained variance, and 
# it also has a lower AIC and BIC, suggesting a better model fit with fewer complexities and better prediction.

####################################################################################################################
#                             STEP 5: Analyzing the Fitted values and Residuals                                    #
####################################################################################################################
yhat = log_linear_results.fittedvalues
e = log_linear_results.resid

#Adding fitted values and residuals to original dataframe
df['yhat'] = yhat
df['e'] = e
df.head()

#Plotting the residuals against each X variable
x_vars =['Relative_Compactness', 'Surface_Area', 'Wall_Area', 'Roof_Area', 'Overall_Height', 'Orientation', 'Glazing_Area','Glazing_Area_Distribution']
for i in x_vars:
    sns.lmplot(x=i,y='e', fit_reg=True, data=df) 
    plt.show()
#There aren't any claer pattern or trend, hence confirming homoscedasticity.

#Plotting the residuals against fitted values
sns.lmplot(x='e',y='yhat', fit_reg=True, data=df)
plt.show()

# generating histogram for residuals
plt.hist(df['e'])
plt.show()
# the residuals exhibited a bell-shaped curve, indicating a good fit to the normal distribution.

# Mean of residuals
mean_residuals = np.mean(df['e'])
print('Mean of residuals:', mean_residuals)
# The mean of residuals is very close to zero indicating good predictions.


####################################################################################################################
#                               STEP 6: Predictions                                                                #
####################################################################################################################
# (a) Small Relative compactness [Spacious houses]
# 0.10, 500, 230, 100, 5, 2, 0.1, 2

# (b) Small Relative compactness and Large Overall_Height [Spacious and Tall houses]
# 0.10, 500, 230, 100, 13, 3, 0.1, 2

# (c) Small Relative compactness and Large Glazing area [Spacious and houses with more windows, glass doors etc]
# 0.10, 500, 230, 100, 5, 2, 0.9, 2

# (d) Large Relative compactness [Compact homes]
# 0.99, 250, 230, 100, 5, 2, 0.1, 2

# (e) Large Relative compactness, Large Overall_Height and Large Glazing_Area [Compact, tall and houses with more glazing area]
# 0.99, 1500, 1000, 100, 13, 2, 0.9, 2

predictions = pd.DataFrame({'Relative_Compactness': [0.10,0.10,0.10,0.99,0.99],
                            'Surface_Area': [500,500,500,250,1500],
                            'Wall_Area':[230,230,230,1000,230],
                            'Roof_Area':[100,100,100,100,100],
                            'Overall_Height':[5,13,5,5,13],
                            'Orientation':[2,3,2,2,2],
                            'Glazing_Area':[0.1,0.1,0.9,0.1,0.9],
                            'Glazing_Area_Distribution':[2,2,2,2,2]})

predictions['estimated_Cooling_Load'] = log_linear_results.predict(predictions)
print(predictions['estimated_Cooling_Load'])

####################################################################################################################
#                               STEP 7: Conclusion                                                                #
####################################################################################################################
# The analysis of the Building Energy Efficiency dataset reveals key insights into the factors that significantly 
# influence a building's cooling load. Particularly, Relative Compactness, Overall Height, and Glazing Area are seen 
# to notably impact energy demands.

# However, the above study reveals an intriguing hierarchy amongst these factors. Overall Height emerges as the most 
# influential determinant of cooling load, superseding the effects of both Relative Compactness and Glazing Area. 
# This suggests that the vertical dimension of a building potentially holds more sway in energy efficiency considerations
# than its spatial compactness or the proportion of glazing used.

# Therefore, to navigate towards a more energy-efficient architecture, the insights suggest a careful balance in building
# design. A preferred strategy would involve creating a relatively compact structure to minimize heat exchange, keeping
# the height at a moderate level to control thermal stratification and solar exposure, and thoughtfully incorporating 
# glazing to benefit from natural light without significantly increasing the cooling load.