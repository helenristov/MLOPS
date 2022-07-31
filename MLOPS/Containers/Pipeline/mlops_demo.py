#!/usr/bin/env python3

import kfp.dsl as dsl
import os

class ObjectDict(dict):
    def __getattr__(self, name):
        if name in self:
          return self[name]
        else:
          raise AttributeError("No such attribute: " + name)

@dsl.pipeline(
    name='POC Pipeline for taxi-mlops-demo model', 
    description='Taxi MLOps Demo model'
)

def train_and_deploy(
    project='taxi-mlops-demo',
    bucket='gs://taxi-mlops-demo-kubeflowpipelines-default'
):
    """Pipeline to retrain and deploy mlops_demo ML model only"""
    # Create dictionaries that correspond to output of previous steps
    preprocess = ObjectDict({
        'outputs': {
            'bucket': bucket,
            'project': project
        }
    })           
    # actual pipeline we want to run
    deploy = train_and_deploy_helper(preprocess)

def train_and_deploy_helper(preprocess):
    """Helper function called from the two pipeline functions"""    
    # Step 1: Train the model some more, but on the pipelines cluster itself
    train = dsl.ContainerOp(
      name='train',
      # image needs to be a compile-time string
      image='gcr.io/taxi-mlops-demo/mlops-demo-train:latest',
      arguments=[
        preprocess.outputs['bucket']
      ],
      file_outputs={'train': '/output.txt'}
    )
    # Kubeflow takes care of scheduling the job only when the necessary resources are available
    #train.set_memory_request('2G')
    #train.set_cpu_request('1')
    
    # Disable component caching - Uncomment line below to enable
    train.execution_options.caching_strategy.max_cache_staleness = "P0D"

    # Step 2: Deploy the trained model to Cloud ML Engine
    deploy = dsl.ContainerOp(
      name='deploy',
      # image needs to be a compile-time string
      image='gcr.io/taxi-mlops-demo/mlops-demo-deploy:latest',
      arguments=[ 
         train.outputs['train'], # model dir
         'mlops_demo', 
         'latest'
      ],
      file_outputs={
        'model': '/model.txt',
        'version': '/version.txt',
        'modeldir': '/modeldir.txt'  
      }
    )
    # Disable component caching - Uncomment line below to enable
    deploy.execution_options.caching_strategy.max_cache_staleness = "P0D"
    
    return deploy                

def train_and_deploy_app():
    """Invoked from Cloud Run (or Cloud function), it launches a Pipeline on kfp"""
    import kfp
    import sys
    
    PIPELINES_HOST = os.environ.get('PIPELINES_HOST', "Environment variable PIPELINES_HOST not set")
    PROJECT = os.environ.get('PROJECT', "Environment variable PROJECT not set")
    BUCKET = os.environ.get('BUCKET', "Environment variable BUCKET not set")
    client = kfp.Client(host=PIPELINES_HOST)
    args = {
       'project' : PROJECT, 
        'bucket' : BUCKET,
    }
    pipeline = client.create_run_from_pipeline_func(train_and_deploy, args)
    return 'Train and Deploy job Launched!'
