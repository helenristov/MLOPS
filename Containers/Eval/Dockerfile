# Inherit from a container that already has gcloud installed
FROM google/cloud-sdk:latest 

RUN mkdir -p /mlops_demo

# TODO Get dataset for training
#COPY taxi.csv /mlops_demo/

COPY eval_model.py eval.sh requirements.txt /mlops_demo/

RUN ["chmod", "+x", "/mlops_demo/eval.sh"]

ENTRYPOINT ["bash", "./mlops_demo/eval.sh"]