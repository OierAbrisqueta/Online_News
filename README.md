# Online News Popularity Analysis

The goal of this project is to address the key drivers of the virality of an article. In order to achieve the dataset: "Online News Popularity" from the UCI Machine Learning Repository has been analyzed. Shares has been the metric used to represent virality. The analysis encompasses data cleaning, exploratory data analysis, regression modeling, dimensionality reduction, and clustering to uncover underlying patterns in social media engagement.

## Project Structure

- **`data/`**: Contains the dataset.
- **`scripts/`**: R scripts containing the data processing and modeling.
  - `1stLinearRegression.R` & `2ndLinearRegressionModel.R`: Linear regression models analyzing the impact of article content, how subjective an article is and the editorial strategy.
  - `Logistic_Regression.R`: Logistic regression models (including the use of natural cubic splines). The main objective of these models is to understand the impact of keywords and media on the virality of an article. For that we classify articles in viral and non viral.
  - `PCA.R`: Principal Component Analysis (PCA) used to reduce the dimensionality of content.
  - `CA.R`: Correspondence Analysis (CA) exploring the relationship between categorical variables.
- **`reports/`**: PDF documents containing detailed findings, interpretations, and methodology.
  - **`Written report 1.pdf`**: Details the descriptive analysis, data cleaning, and regression models.
  - **`Written report 2.pdf`**: Details dimensionality reduction and clustering techniques (K-Means and Hierarchical).

## Key Findings

1. **Keywords > Multimedia:** An excellent utilization of keywords increases the odds of an article becoming viral, whereas the quantity of multimedia elements has a lower and with less prediction capability.
2. **Sentiment and Subjectivity:** The emotional tone and subjectivity of an article are among the most informative dimensions, with global subjectivity acting as a positive driver for shares.
3. **Editorial Strategy:** There is a clear divide in publication patterns: weekends are heavily associated with Lifestyle content, while weekdays are more uniformly distributed with a lean toward Business and World news.
