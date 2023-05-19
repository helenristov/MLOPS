#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: ./deploy.sh modeldir modelname modelversion" 
    exit
fi

MODEL_LOCATION=$1 
MODEL_NAME=$2
MODEL_VERSION=$3

echo "modeldir: $1"
echo "modelname: $2"
echo "modelversion: $3"
echo "bucket: $4"

TFVERSION=2.4
REGION=us-central1

# Set the region
$(gcloud config set ai_platform/region "$REGION")

# create the model if it doesn't already exist
modelname=$(gcloud ai-platform models list | grep -w "$MODEL_NAME")
echo $modelname
if [ -z "$modelname" ]; then
   echo "Creating model $MODEL_NAME"
   gcloud ai-platform models create ${MODEL_NAME}
else
   echo "Model $MODEL_NAME already exists"
fi

# delete the model version if it already exists
modelver=$(gcloud ai-platform versions list --model "$MODEL_NAME" | grep -w "$MODEL_VERSION")
echo $modelver

if [ "$modelver" ]; then
   echo "Deleting version $MODEL_VERSION"
   yes | gcloud ai-platform versions delete ${MODEL_VERSION} --model ${MODEL_NAME}
   sleep 10
fi

echo "Creating version $MODEL_VERSION from $MODEL_LOCATION"
gcloud ai-platform versions create ${MODEL_VERSION} \
       --model ${MODEL_NAME} --origin ${MODEL_LOCATION} \
       --framework 'TENSORFLOW' --runtime-version $TFVERSION #--staging-bucket 'gs://${BUCKET}'
       
echo $MODEL_NAME > /model.txt
echo $MODEL_VERSION > /version.txt
echo $MODEL_LOCATION > /modeldir.txt
