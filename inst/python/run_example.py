## Import libraries
from inst.python.utils_example import *

import pandas as pd
import numpy as np
import os
import torch
from torch.utils.data import TensorDataset, DataLoader

root="inst/python/"

# Define paths
data_dir=root + "example_data_standardized"
output_dir=root + "example_output"
model_dir=root + "example_model"

# Define data related parameters
batch_sz = 1
output_sz = 1
seq_sz = 0

# Check device
device = torch.device('cpu')
print('Device available now:', device, '\n')

# Load test data
vehicle = "Geoh"
filename = "Geoh_vehicle.csv"
print(os.path.join(data_dir, filename))
data = pd.read_csv(os.path.join(data_dir, filename), delimiter=',')

# Prepare test data for model testing
X_test_scaled, y_test = data.drop(columns='co2'), data['co2'].squeeze()
X_test_LSTM, y_test_LSTM = lstm_data_transform(X_test_scaled, y_test.to_numpy().reshape(-1, 1), seq_sz)
data_test = TensorDataset(torch.Tensor(X_test_LSTM), torch.Tensor(y_test_LSTM))
test_loader = DataLoader(data_test, batch_size=batch_sz, shuffle=False, drop_last=False)
input_sz = X_test_scaled.shape[1]

# Load trained model
checkpoint = torch.load(model_dir + '/model.pth')

# Test the model
test_loss, test_mae, true, pred = test(test_loader, batch_sz, input_sz, output_sz, vehicle, device, checkpoint, output_dir)

print('Complete')
