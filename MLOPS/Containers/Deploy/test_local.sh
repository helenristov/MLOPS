#!/bin/bash -e

# chmod 744 *.sh

PROJECT_ID=$(gcloud config config-helper --format "value(configuration.properties.core.project)")
BUCKET="${PROJECT_ID}-kubeflowpipelines-default"  

../build_container.sh

docker run -t gcr.io/${PROJECT_ID}/mlops-demo-deploy gs://${BUCKET}/mlops_demo/ mlops_demo latest ${BUCKET}