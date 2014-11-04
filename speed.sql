SELECT 
	user_token, 
	MIN(response_speed), 
	MAX(response_speed), 
	MAX(cue_id) 
FROM output_response 
WHERE 
	response_type = 'speed-change' 
GROUP BY 
	user_token;

SELECT 
	user_token, 
	response_speed
FROM g_sisl_hero.output_response 
WHERE 
	response_type = 'speed-change'
ORDER BY 
	user_token, response_id;