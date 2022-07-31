#!/bin/bash -e

if [ "$#" -ne 1 ]; then
    echo "Usage: ./deploy_cloudrun.sh pipelines_host"
    echo "  eg:  ./deploy_cloudrun.sh 27fb1c8073599e32-dot-us-central1.pipelines.googleusercontent.com"
    exit
fi

PROJECT=$(gcloud config get-value project)
BUCKET="${PROJECT}-kubeflowpipelines-default"  # gs://dao-aa-sandbox-8vn4-kubeflowpipelines-default
REGION=us-central1
PIPELINES_HOST=$1
#HPARAM_JOB=$2

# build the container for Cloud Run
../build_container.sh

# deploy Cloud Run
gcloud run deploy mlops-demo \
   --platform=managed --region=${REGION} \
   --image gcr.io/${PROJECT}/mlops-demo-pipeline \
   --set-env-vars PROJECT=${PROJECT},BUCKET=${BUCKET},PIPELINES_HOST=${PIPELINES_HOST} # ,HPARAM_JOB=${HPARAM_JOB}
