## Import libraries
import pandas as pd
import numpy as np
import os
import pickle

# Define paths
data_dir = "test_data"
output_dir = "test_outputs_co2"
model_dir = "trained_models_co2"

# Variables to be used
predictors = ['rpm', 'wheel_acc', 'slope']
response = 'co2'

# Vehicle name
vehicle = "030_Ford_Focus_2012_(2.0L_Auto)"

# Load trained model
best_model = pickle.load(open(os.path.join(model_dir, vehicle + '.pickle'), "rb"))

# Load test data
data_test = pd.read_csv(os.path.join(data_dir, vehicle + '.csv'))
model_features = list(best_model.feature_names_in_)
X_test, y_test = data_test[model_features], data_test[response]

# Get predictions for test data
y_test_pred = best_model.predict(X_test)
y_test_pred = np.maximum(y_test_pred, np.zeros(len(y_test)))           

# Write outputs
out_df = pd.DataFrame({'true_co2' : y_test, 'pred_co2' : y_test_pred}) 
out_df.to_csv(output_dir + '/' + vehicle + '_test_output.csv') 