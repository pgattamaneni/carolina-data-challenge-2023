# Carolina Data Challenge 2023

## Health Science Track 

### Team Members

- Andrew Burnette
- Lillian Lecca
- Pooja Gattamaneni

### Dataset

Dataset Name: Diabetes 130-US hospitals for years 1999-2008
Dataset Source: [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/296/diabetes+130-us+hospitals+for+years+1999-2008)

### Description 

Our dataset comprises a decade's worth of clinical care data, spanning from 1999 to 2008, obtained from 130 US hospitals and integrated delivery networks. Each record corresponds to a patient diagnosed with diabetes who underwent various medical procedures, including laboratory tests and medication, during a hospital stay lasting up to 14 days. The primary objective is to predict early patient readmission within 30 days of discharge.

This challenge holds significant clinical importance due to several reasons. Despite well-established evidence demonstrating improved clinical outcomes for diabetic patients who receive preventive and therapeutic interventions, many patients still do not receive adequate care. This inadequacy can be attributed in part to the inconsistent management of diabetes within hospital settings, leading to suboptimal glycemic control. The failure to provide appropriate diabetes care not only increases hospital costs, as patients are often readmitted, but also raises the risk of complications and mortality among diabetic patients.

### Data Exploration

Our initial data exploration efforts included the following:

```
- Generated an HTML file containing descriptive statistics using Sweetviz.
- Addressed missing values within the dataset.
```
### Feature Engineering

To enhance our model's performance and interpretability, we conducted feature engineering, which involved the following:


```
- Utilized domain knowledge to engineer features based on the 'diag 1,' 'diag 2,' and 'diag 3' columns.
- Created a novel feature to quantify the severity of the disease.
```

### Model Building

Our model building process encompassed the following steps:

```
- Conducted hypothesis testing using Logistic Regression with Maximum Likelihood Estimation.
- Employed a Gradient Boosting Model, specifically LightGBM, for predictive modeling.

```

### Inferences from the Model

Our model yielded several valuable insights, including:

```
- Calculated the lift ratio to assess the effectiveness of our predictive model.
- Developed interactive dashboards for both readmission control and general data analysis.
```

