#!/bin/bash -e

# chmod 744 *.sh

PROJECT_ID=$(gcloud config config-helper --format "value(configuration.properties.core.project)")
BUCKET="${PROJECT_ID}-kubeflowpipelines-default"   # taxi-mlops-demo-kubeflowpipelines-default

# gsutil cp gs://mlops_model_input/data_input/Taxi_Trips_3mo.csv taxi.csv

../build_container.sh
docker pull gcr.io/${PROJECT_ID}/mlops-demo-eval:latest
docker run -t gcr.io/${PROJECT_ID}/mlops-demo-eval:latest ${BUCKET}