"""
Segmentation
Analysis

"""

import pandas as pd
import numpy as np

from sklearn.tree import DecisionTreeClassifier

df = pd.read_csv('data_prob.csv')
df.head(5)
df['prob'].describe()

# Define segmentation thresholds
thresholds = [0.15, 0.25]

# Create a function to categorize predictions
def categorize_probability(prob):
    if prob < thresholds[0]:
        return 'Low'
    elif prob < thresholds[1]:
        return 'Medium'
    else:
        return 'High'

# Apply the categorization function to create the segment column
df['segment'] = df['prob'].apply(categorize_probability)
df['segment'].value_counts()

# Initialize a linear regression model
model = DecisionTreeClassifier()

# Create dictionaries to store feature importance rankings for each segment
feature_importance_rankings = {
    'Low': {},
    'Medium': {},
    'High': {}
}

# Iterate through segments and calculate feature importance for each
for segment in ['Low', 'Medium', 'High']:
    segment_data = df[df['segment'] == segment]
    X = segment_data[['number_inpatient', 'number_emergency', 'num_lab_procedures', 'number_diagnoses', 'time_in_hospital', 'num_medications']]
    y = segment_data['prob']

    # Fit the linear regression model
    model.fit(X, y)

    # Calculate feature importance as absolute coefficients
    importances = np.abs(model.coef_)

    # Store feature importance scores in the dictionary
    feature_importance_rankings[segment] = dict(zip(X.columns, importances))

# # Print feature importance rankings for each segment
# for segment, importance_ranking in feature_importance_rankings.items():
#     print(f"Segment: {segment}")
#     sorted_importance = sorted(importance_ranking.items(), key=lambda x: x[1], reverse=True)
#     for feature, importance in sorted_importance:
#         print(f"{feature}: {importance}")
#     print()

result_data = []
for segment, importance_ranking in feature_importance_rankings.items():
    for feature, importance in importance_ranking.items():
        result_data.append({'Segment': segment, 'Feature': feature, 'Rank Score': importance})

result_df = pd.DataFrame(result_data)
print(result_df)
result_df.to_csv('segment_data.csv', index=False)

# Encode 'A1Cresult' into numerical categories
category_mapping = {'>8': 3, '>7': 2, 'normal': 1, 'none': 0}
df['A1CresultNumeric'] = df['A1Cresult'].map(category_mapping)

# Define segmentation thresholds (customize based on your data)
low_threshold = 0.3
medium_threshold = 0.6

# Apply the categorization function to create the 'ProbabilitySegment' column
df['ProbabilitySegment'] = df['prob'].apply(categorize_probability)

# Group data by 'A1cresultNumeric' and 'ProbabilitySegment'
segmented_data = df.groupby(['A1CresultNumeric', 'prob'])

# Perform analysis and interpretation within each segment
for segment, segment_data in segmented_data:
    print(f"Segment: {segment}")
    print(segment_data)  # Example: Calculate statistics for insights