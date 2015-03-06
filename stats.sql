-- Stats
select 
	user_token 
	-- ,min(response_speed) 
	-- ,max(cue_id)
	-- ,count(1)
	-- ,avg(response_speed)
from output_response
where scenario_id between 14 and 22
group by 
	user_token
having 
	count(1) > 10000
	and max(cue_id) = 3564
    and avg(response_speed) > 0.70
;

-- Get Ids
select 
	user_token
from output_response 
group by 
	user_token
having 
	max(cue_id) = 4500 
	and count(1) > 10000;

