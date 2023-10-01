# Carolina Data Challenge 2023

## Health Science Track 

### Dataset

Diabetes 130-US hospitals for years 1999-2008

https://archive.ics.uci.edu/dataset/296/diabetes+130-us+hospitals+for+years+1999-2008

### Description 

The dataset represents ten years (1999-2008) of clinical care at 130 US hospitals and integrated delivery networks. Each row concerns hospital records of patients diagnosed with diabetes, who underwent laboratory, medications, and stayed up to 14 days. The goal is to determine the early readmission of the patient within 30 days of discharge. The problem is important for the following reasons. Despite high-quality evidence showing improved clinical outcomes for diabetic patients who receive various preventive and therapeutic interventions, many patients do not receive them. This can be partially attributed to arbitrary diabetes management in hospital environments, which fail to attend to glycemic control. Failure to provide proper diabetes care not only increases the managing costs for the hospitals (as the patients are readmitted) but also impacts the morbidity and mortality of the patients, who may face complications associated with diabetes.

### Data Exploration

```
- Generated a HTML file with descriptive statistics using sweetviz
- Missing Values

```
### Feature Engineering

```
- Using domain knowledge, feature engineered diag 1, diag 2 and diag 3 columns
- Extracted a new feature called severity of the disease

```

### Model Building

```
- Hypothesis testing using Logistic Regression - Maximum Likelihood Estimation
- Gradient Boosting Model using lgbm

```

### Inferences from the Model

```
- Lift ratio 
- Dashboards for Readmission control and General
```

