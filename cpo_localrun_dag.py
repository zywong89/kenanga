import airflow
from airflow.operators.bash_operator import BashOperator
from airflow.utils.dates import days_ago
from airflow.models import DAG
from datetime import datetime, timedelta

args = {
	'owner': 'zhenyao',
	'start_date': datetime(2020, 11, 26, 7, 15),
	'email': ['zywong1989@yahoo.com.my'],
	'email_on_failure': False,
	'email_on_retry': False
}

dag = DAG(
	dag_id = 'cpo_localrun',
	description = 'Obtain CPO Price Forecast Locally',
	default_args = args,
	schedule_interval = timedelta(minutes = 30),
	concurrency = 1, max_active_runs = 1,
	catchup = False
)

path = '~/Kenanga'

task_0 = BashOperator(
	task_id = 'define_path',
	bash_command = 'echo ' + path,
	dag = dag
)

task_1 = BashOperator(
	task_id = 'database_schema_creation',
	bash_command = 'psql -d postgres -U postgres -p 5432 -a -f ' + path + '/Scripts/database_schema_creation.sql',
	dag = dag
)

task_2 = BashOperator(
	task_id = 'e2e_predictive_modelling',
	bash_command = 'Rscript --vanilla ' + path + '/localRun.R ' + path,
	dag = dag
)

task_0 >> task_1 >> task_2
