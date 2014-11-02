-- Stats
select 
	user_token, 
	min(response_speed), 
	max(cue_id) ,
	count(1)
from output_response 
group by 
	user_token
having 
	max(cue_id) = 4500 
	and count(1) > 10000;

-- Get Ids
select 
	user_token
from output_response 
group by 
	user_token
having 
	max(cue_id) = 4500 
	and count(1) > 10000;

