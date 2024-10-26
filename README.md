# **Analysis of Customer Tenure in a Telecom Company**

## **Project Overview**

This project investigates factors influencing customer tenure within a telecom company, aiming to identify variables that impact the duration customers remain with the service. This analysis could aid in understanding customer retention and in shaping marketing or product decisions within telecom sectors.

## **Objectives**
The primary objectives of this project include:  
- **Identify key factors** that correlate with customer tenure.  
- **Construct a predictive model** using multiple linear regression to estimate tenure based on relevant features.  
- **Evaluate models** using Ridge and Lasso regression for enhanced accuracy and interpretability.

## **Dataset**
- **Source**: Collected from [Google Dataset Search](https://datasetsearch.research.google.com).
- **Description**: The dataset contains 1039 observations with 35 variables, including demographic data, service usage, and service options.

## **Methodology**
1. **Data Preprocessing**:  
   - Transform categorical variables and perform descriptive statistics.
   - Categorize tenure into levels (e.g., Low, Middle, High) to understand associations with categorical predictors.

2. **Analysis of Associations**:  
   - Use statistical tests (e.g., Cramer’s V, Biserial Correlation) to assess dependencies between tenure and factors like age, gender, offers, contract types, and service features.

3. **Model Building**:  
   - Fit a **Multiple Linear Regression Model** with significant predictors.
   - Address multicollinearity using **Variance Inflation Factor (VIF)** and **Eigen Value System**.
   - Implement **Ridge and Lasso regression** to improve model stability and interpretability.

4. **Model Validation**:  
   - Test residuals for normality and evaluate model fit on 30% of test data.
   - Compare models based on Mean Squared Error (MSE) and **Adjusted R-squared** values.

## **Results**
- **Significant Predictors**: Key predictors include `Monthly Charges`, `Total Charges`, `Gender`, `Offers`, `Contract`, and `Device Protection Plan`.
- **Model Performance**:  
   - The **Multiple Linear Regression Model** explained approximately 97% of the variance (Adjusted R-squared = 0.9686).
   - **Lasso Regression** showed the lowest Mean Squared Error, indicating a potentially better fit for predictions.

## **Conclusions**
This project concludes that factors like **Monthly Charges, Offers, and Contract Duration** are substantial indicators of customer tenure. Understanding these factors provides valuable insights for strategic planning in customer retention.

## **How to Run the Code**
1. Clone the repository.
2. Ensure necessary libraries are installed (e.g., `R`, `glmnet`).
3. Run `analysis_script.R` to execute the model and generate output.
