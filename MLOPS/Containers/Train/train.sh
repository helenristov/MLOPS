#!/bin/bash

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: ./train.sh  bucket-name"
    exit
fi

BUCKET=$1

# Install production dependencies.
pip3 install --upgrade pip
pip3 install -r /mlops_demo/requirements.txt

# copy in latest taxi file
gsutil cp gs://mlops_model_input/data_input/taxi_latest.csv taxi.csv

# Create model
python3 /mlops_demo/train_model.py

# directory containing trainer package in Docker image
# see Dockerfile
CODEDIR=/mlops_demo/my_model
OUTDIR=gs://${BUCKET}/mlops_demo/

gsutil -m rm -rf $OUTDIR || true

gsutil cp -r my_model gs://${BUCKET}/mlops_demo

# note --stream-logs above so that we wait for job to finish
# write output file for next step in pipeline
echo $OUTDIR > /output.txt