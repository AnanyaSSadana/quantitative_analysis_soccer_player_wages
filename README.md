# quantitative_analysis_soccer_player_wages
Project Overview
This project provides a thorough quantitative analysis of factors influencing soccer players' wages, focusing on the predictive power of contract length and various player attributes using data from the FIFA video game series.

Objectives
To analyze the wage disparity between players with long-term contracts (valid until 2026 or later) and those with shorter-term contracts.
To investigate the impact of age, international reputation, contract length, potential, and value on player wages.

Methodology
Data Preparation: Cleaned and formatted a comprehensive dataset for analysis. Implemented log transformation to stabilize variance and normalize wage data.
Statistical Analysis: Conducted Shapiro-Wilk tests, Q-Q plot analysis, variance tests, and non-parametric tests to assess normality and variance of wage data.
Modeling: Applied Generalized Linear Models (GLM) to evaluate the impact of various predictors on wages.
Missing Value Analysis: Performed GLM on datasets with missing values handled by mean and median imputation to assess the robustness of the models.

Results
Hypothesis 1: Found no significant difference in wages between long and short-term contract players, indicating contract length does not significantly affect wages.
Hypothesis 2: Identified age, international reputation, potential, and value as significant predictors of wages, while contract length was not significant.
