CREATE TABLE `analysis_output_response` (
    `response_id` int(11) NOT NULL,
    `user_token` varchar(256) NOT NULL,
    `scenario_id` int(11) NOT NULL,
    `batch_id` int(11) NOT NULL,
    `cue_id` int(11) NOT NULL,
    `event_time_ms` int(11) NOT NULL,
    `response_type` varchar(256) NOT NULL,
    `response_value` varchar(256) NOT NULL,
    `response_dist_norm` float NOT NULL,
    `response_speed` float NOT NULL,
    PRIMARY KEY (`response_id`)
)  ENGINE=InnoDB AUTO_INCREMENT=1027201 DEFAULT CHARSET=latin1;

-- Kept tokens
CREATE TEMPORARY TABLE kept_token AS (
SELECT 
	user_token AS user_token
FROM output_response 
GROUP BY 
	user_token
HAVING 
	MAX(cue_id) = 4500 
	AND COUNT(1) > 10000
);

-- Feeding table
INSERT INTO analysis_output_response
SELECT 
	response_id,
    user_token,
    scenario_id,
    batch_id,
    cue_id,
    event_time_ms,
	response_type,
    response_value,
    response_dist_norm,
    response_speed
FROM output_response ores
WHERE tle.trial_list_id = 3 AND
	user_token IN (
		SELECT
			user_token
		FROM kept_token
	);


