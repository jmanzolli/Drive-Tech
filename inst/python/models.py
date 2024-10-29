import pandas as pd
import numpy as np
import pickle

def predict_fcr_model(datapath, data_test):
    best_model = pickle.load(open(datapath, "rb"))

    model_features = list(best_model.feature_names_in_)
    X_test, y_test = data_test[model_features], data_test['fcr']

    y_test_pred = best_model.predict(X_test)
    y_test_pred = np.maximum(y_test_pred, np.zeros(len(y_test)))           

    out_df = pd.DataFrame({'true_fcr' : y_test, 'pred_fcr' : y_test_pred}) 

    return out_df

def predict_co2_model(datapath, data_test):
    best_model = pickle.load(open(datapath, "rb"))

    model_features = list(best_model.feature_names_in_)
    X_test, y_test = data_test[model_features], data_test['co2']

    y_test_pred = best_model.predict(X_test)
    y_test_pred = np.maximum(y_test_pred, np.zeros(len(y_test)))           

    out_df = pd.DataFrame({'true_co2' : y_test, 'pred_co2' : y_test_pred}) 

    return out_df