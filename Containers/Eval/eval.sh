#!/bin/bash

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: ./eval.sh  bucket-name"
    exit
fi

BUCKET=$1

# Install production dependencies.
pip3 install --upgrade pip
pip3 install -r /mlops_demo/requirements.txt

# copy in latest taxi file
gsutil cp gs://mlops_model_input/data_input/taxi_latest.csv /mlops_demo/taxi.csv

#copy in latest model
gsutil cp -R gs://taxi-mlops-demo-kubeflowpipelines-default/mlops_demo .

# Create model
python3 /mlops_demo/eval_model.py

# directory containing trainer package in Docker image
# see Dockerfile
#CODEDIR=/mlops_demo/my_model
#OUTDIR=gs://${BUCKET}/mlops_demo/eval

#gsutil -m rm -rf $OUTDIR || true

#gsutil cp model_metrics.json gs://${BUCKET}/mlops_demo/eval/model_metrics.json

# note --stream-logs above so that we wait for job to finish
# write output file for next step in pipeline
#echo $OUTDIR > /output.txt