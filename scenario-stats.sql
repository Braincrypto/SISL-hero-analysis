select
	scenario_id,
	count(distinct user_token)
from output_response
group by scenario_id