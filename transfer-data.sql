INSERT INTO `braincrypto`.`trial_list_event`
SELECT
    NULL AS trial_list_event_id,
    trial_list_id AS trial_list_id,
	`event_id` AS event_id,
    `cue_id` AS cue_id,
    `event_type` AS event_type,
    `event_value` AS event_value,
    `dist_norm` AS dist_norm,
    `duration_time_ms` AS duration_time_ms,
    `event_category` AS event_category,
    `time_to_target_ms` AS time_to_target
FROM `braincrypto`.`trial_list_buffer`;
