# load libraries
import pandas as pd
from io import BytesIO
import tensorflow as tf
import numpy as np
import os


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


def input_fn(features, labels, shuffle, num_epochs, batch_size):
    """Generates an input function to be used for model training.

  Args:
    features: numpy array of features used for training or inference
    labels: numpy array of labels for each example
    shuffle: boolean for whether to shuffle the data or not (set True for
      training, False for evaluation)
    num_epochs: number of epochs to provide the data for
    batch_size: batch size for training

  Returns:
    A tf.data.Dataset that can provide data to the Keras model for training or
      evaluation
  """
    if labels is None:
        inputs = features
    else:
        inputs = (features, labels)
    dataset = tf.data.Dataset.from_tensor_slices(inputs)

    if shuffle:
        dataset = dataset.shuffle(buffer_size=len(features))

  # We call repeat after shuffling, rather than before, to prevent separate
  # epochs from blending together.
    dataset = dataset.repeat(num_epochs)
    dataset = dataset.batch(batch_size)
    return dataset


def create_keras_model(input_dim, learning_rate):
    """Creates Keras Model for Binary Classification.

  Args:
    input_dim: How many features the input has
    learning_rate: Learning rate for training

  Returns:
    The compiled Keras model (still needs to be trained)
  """
    Dense = tf.keras.layers.Dense
    model = tf.keras.Sequential(
          [
              Dense(5, activation=tf.nn.relu, kernel_initializer='normal',
                  input_shape=(input_dim,)),
              Dense(3, activation=tf.nn.relu, kernel_initializer='normal',
                  input_shape=(input_dim,)),
              Dense(1, kernel_initializer='normal')
              ])

          # Custom Optimizer:
  # https://www.tensorflow.org/api_docs/python/tf/train/RMSPropOptimizer
  # optimizer = tf.keras.optimizers.Ftrl(
     # lr=learning_rate)

  # Compile Keras model
    model.compile(
          loss="mean_squared_error", optimizer='adam')
    return model


if __name__ == '__main__':

    _LABEL_COLUMN = 'Trip Seconds'

    ### Hyperparameters for training ###

    # This the training batch size
    BATCH_SIZE = 32

    # This is the number of epochs (passes over the full training data)
    NUM_EPOCHS = 20

    # Define learning rate.
    LEARNING_RATE = .05

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

    random_seed = 42
    tf.random.set_seed(random_seed)
    np.random.seed(random_seed)

    prepped_train_df = preprocess(train_df,categorical_columns,numeric_columns)
    
    # Split train and test data with labels.
    # The pop() method will extract (copy) and remove the label column from the dataframe
    train_x, train_y = prepped_train_df, prepped_train_df.pop(_LABEL_COLUMN)

    # Join train_x and eval_x to normalize on overall means and standard
    # deviations. Then separate them again.
    train_x = standardize(train_x)

    # Reshape label columns for use with tf.data.Dataset
    train_y = np.asarray(train_y).astype('float32').reshape((-1, 1))

    # Pass a numpy array by using DataFrame.values
    training_dataset = input_fn(features=train_x.values,
            labels=train_y,
            shuffle=True,
            num_epochs=NUM_EPOCHS,
            batch_size=BATCH_SIZE)

    # The TF linear estimator uses both numeric and categorical features.

    num_train_examples, input_dim = train_x.shape
    print('Number of features: {}'.format(input_dim))
    print('Number of examples: {}'.format(num_train_examples))

    keras_model = create_keras_model(
           input_dim=input_dim,
           learning_rate=LEARNING_RATE)

    history = keras_model.fit(training_dataset,
           epochs=NUM_EPOCHS,
           steps_per_epoch=500,
           verbose=1)

    OUTDIR = 'my_model'
    #modelBasePath = os.path.join(OUTDIR, "model")
    #keras_model.save(modelBasePath)
    keras_model.save(OUTDIR)
    
    import json
    
    mae = int(round(keras_model.evaluate(train_x,train_y),0))
    
    with open('my_model/model_metrics.json', 'w') as json_file:
        mae_dict = {'curr_mae':mae}
        json.dump(mae_dict, json_file)


