FROM google/cloud-sdk:latest

RUN mkdir -p /predict-exit && cd /predict-exit
COPY deploy.sh ./

RUN ["chmod", "+x", "deploy.sh"]

ENTRYPOINT ["bash", "./deploy.sh"]
