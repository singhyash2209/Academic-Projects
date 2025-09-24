import boto3
import os

def lambda_handler(event, context):
    s3 = boto3.client('s3')
    bucket = 'mlpipelineexample'
    key = 'model/model.joblib'
    local_path = '/tmp/model.joblib'

    try:
        s3.download_file(bucket, key, local_path)
        size = os.path.getsize(local_path)
        return {
            'statusCode': 200,
            'body': f"Model downloaded successfully! Size: {size} bytes"
        }
    except Exception as e:
        return {
            'statusCode': 500,
            'body': f"Error fetching model: {str(e)}"
        }
