# Forecasting 3-Month Crude Palm Oil (CPO) Prices - Simple API

## Steps to Start API
1. Place entire folder (named Kenanga) in home directory
2. Make a copy of *cpo_apirun.py* in Airflow DAGs folder
3. Start Airflow Admin UI and trigger *cpo_apirun* DAG to run

## Obtain Forecasts
1. Run http://127.0.0.1:8080/predict?refit=0 on browser
2. Set refit=1 to refit the model (may take some time to complete)


Note:
* This setup requires PostgreSQL 12, R, Python and Airflow to be installed in local machine
* The outputs are T+1, T+2 and T+3 forecasts in array, unboxed JSON
* To study the entire flow of this exercise, repeat "Steps to Start API" by copying *cpo_localrun.py* and trigger *cpo_localrun* DAG
