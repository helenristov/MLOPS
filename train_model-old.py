##load libraries
import pandas as pd
from io import BytesIO
import os
from datetime import datetime, date, time, timedelta
import keras
import numpy
import tensorflow as tf

## Transform Data Frame before feeding the data to Neural Network
#def transform(df):
# TODO

## Convert Pandas Data Frame to Tensorflow Data Set
#def df_to_ds(df, shuffle=True, batch_size=32):
# TODO


def make_input_fn(data_df, label_df, num_epochs=2, shuffle=True, batch_size=32):
    def input_function():
        ds = tf.data.Dataset.from_tensor_slices((dict(data_df), label_df))
        if shuffle:
            ds = ds.shuffle(1000)
        ds = ds.batch(batch_size).repeat(num_epochs)
        return ds
    return input_function


def json_serving_input_fn():
    """Build the serving inputs."""
    inputs = {}
    for feat in feature_columns:
        inputs[feat] = tf.compat.v1.placeholder(shape=[None], dtype=feat.dtype)

    return tf.estimator.export.ServingInputReceiver(inputs, inputs)


if __name__ == '__main__':
    
    df = pd.read_csv('/mlops-demo/taxi.csv')
    
    # drop NA / clean data
    df['Trip Start Timestamp'] = pd.to_datetime(df['Trip Start Timestamp'])
    
    df['start_dow'] = df['Trip Start Timestamp'].dt.day_name()
    
    df['start_time_seconds'] = (pd.to_timedelta(df['Trip Start Timestamp'].dt.time.astype(str)) -pd.Timedelta("0 days")).dt.total_seconds()
    
    x_cols = ["Pickup Community Area","Dropoff Community Area", "Company",'start_time_seconds',
    'start_dow','Trip Miles']
    
    df = df.dropna(subset=x_cols+['Trip Seconds'])
    
    df['Trip Seconds'] = df['Trip Seconds'].str.replace(',','').astype(int)
    
    df = df[df['Trip Seconds'] > 0]
    
    ## create our training data sets with the independent vars. 
    ## X = Independent Vars: taxi cab company, pickup community area, and drop off community 
    ## Y = Var we are trying to predict : trip time in seconds

    X_run_cols = [x in ["Pickup Community Area","Dropoff Community Area", "Company",'start_time_seconds','start_dow','Trip Miles'] for x in df.columns]
    Y_col = df.columns.str.contains("trip seconds", case = False)
    
    X = df.iloc[:, X_run_cols]
    Y = df.iloc[:, Y_col]
   
    random_seed = 123
    tf.random.set_seed(random_seed)
    numpy.random.seed(random_seed)

    x_train = X
    y_train = Y
    
    ## The TF linear estimator uses both numeric and categorical features. 
    
    CATEGORICAL_COLUMNS = ['Company','start_dow']
    NUMERIC_COLUMNS = ['Pickup Community Area', 'Dropoff Community Area','start_time_seconds','Trip Miles']

    feature_columns = []
    for feature_name in CATEGORICAL_COLUMNS:
        vocabulary = x_train[feature_name].unique()
    
    feature_columns.append(tf.feature_column.categorical_column_with_vocabulary_list(feature_name, vocabulary))

    for feature_name in NUMERIC_COLUMNS:
        feature_columns.append(tf.feature_column.numeric_column(feature_name, dtype=tf.float32))
    
    train_input_fn = make_input_fn(x_train, y_train)
    
    linear_est = tf.estimator.LinearRegressor(feature_columns=feature_columns)
    linear_est.train(train_input_fn)
    
    #inputFn = tf.estimator.export.build_raw_serving_input_receiver_fn(
    #tf.feature_column.make_parse_example_spec(feature_columns))
    
    json_serving_input_fn = json_serving_input_fn()
    
    OUTDIR = 'my_model'
    modelBasePath = os.path.join(OUTDIR, "model")
    modelPath = linear_est.export_saved_model(modelBasePath, inputFn)
    