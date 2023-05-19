# load libraries
import pandas as pd
from io import BytesIO
import tensorflow as tf
import numpy as np
import os
from google.cloud import storage


def standardize(dataframe):
   
    dtypes = list(zip(dataframe.dtypes.index, map(str, dataframe.dtypes)))
  # Normalize numeric columns.
    for column, dtype in dtypes:
        if dtype == 'float32':
            dataframe[column] -= dataframe[column].mean()
            dataframe[column] /= dataframe[column].std()
    return dataframe


def preprocess(dataframe, cat_columns, numeric_columns):
    """Converts categorical features to numeric. Removes unused columns.

    Args:
      dataframe: Pandas dataframe with raw data

    Returns:
      Dataframe with preprocessed data
    """

    # 1h-encode
    for col in cat_columns:
        dataframe = dataframe.join(pd.get_dummies(dataframe[col]))
        dataframe = dataframe.drop(columns=[col])

    # Convert integer valued (numeric) columns to floating point
    dataframe = dataframe.astype('float32')

    return dataframe


if __name__ == '__main__':

    _LABEL_COLUMN = 'Trip Seconds'

    df = pd.read_csv('/mlops_demo/taxi.csv')

    # drop NA / clean data
    df['Trip Start Timestamp'] = pd.to_datetime(df['Trip Start Timestamp'])

    df['start_dow'] = df['Trip Start Timestamp'].dt.day_name()

    df['start_hr'] = df['Trip Start Timestamp'].dt.hour

    x_cols = ["Pickup Centroid Latitude", "Pickup Centroid Longitude", "Dropoff Centroid Longitude",
            "Dropoff Centroid Latitude", 'start_hr',
            'start_dow', 'Trip Miles']

    df = df.dropna(subset=x_cols+['Trip Seconds'])

    df['Trip Seconds'] = df['Trip Seconds'].str.replace(',', '').astype(int)

    df = df.loc[df['Trip Seconds'] < 5000, :]

    df = df[df['Trip Seconds'] > 0]

    train_df = df.loc[:, x_cols+['Trip Seconds']]

    categorical_columns = ['start_dow']
    numeric_columns = ["Pickup Centroid Latitude", "Pickup Centroid Longitude",
            "Dropoff Centroid Longitude",
            "Dropoff Centroid Latitude", 'start_hr', 'Trip Miles', 'Trip Seconds']

    prepped_train_df = preprocess(train_df,categorical_columns,numeric_columns)
    
    # Split train and test data with labels.
    # The pop() method will extract (copy) and remove the label column from the dataframe
    train_x, train_y = prepped_train_df, prepped_train_df.pop(_LABEL_COLUMN)

    # Join train_x and eval_x to normalize on overall means and standard
    # deviations. Then separate them again.
    train_x = standardize(train_x)

    # Reshape label columns for use with tf.data.Dataset
    train_y = np.asarray(train_y).astype('float32').reshape((-1, 1))

    model = tf.keras.models.load_model('mlops_demo/')
    
    import json

    #mae = int(round(model.evaluate(train_x,train_y),0))
    
    mae = int(1e6)
    
    #with open('model_metrics.json', 'w') as json_file:
        #mae_dict = {'new_mae':mae}
        #json.dump(mae_dict, json_file)

    storage_client = storage.Client()
    bk = storage_client.get_bucket('taxi-mlops-demo-kubeflowpipelines-default')
    blob = bk.get_blob('mlops_demo/model_metrics.json') 
    fileData = json.loads(blob.download_as_string())   
    curr_mae = fileData['curr_mae']
    
    file1 = open("output.txt","a")
    
    if mae > curr_mae:
        file1.write("retrain")
    else:
        file1.write("no_rt")
        

