INSERT INTO parameter
SELECT 
NULL AS parameter_id, 
0.5 AS middle_padding, 
1 AS letter_show,
'white' AS letter_color,
1 AS letter_size,
'S D F J K L' AS `keys`,
6 AS num_keys,
'#1f77b4 #ff7f0e #2ca02c #d62728 #9467bd #8c564b' AS cue_colors,
5 AS cue_size,
'#8dc63f' AS pos_color,
'#D00000' AS neg_color,
10 AS ratio_bubble,
1 AS check_every,
50 AS `interval`,
30 AS batch_size,
12 AS speed_lookback,
1.05 AS speed_fraction,
0.85 AS speed_up_threshold,
0.65 AS speed_down_threshold,
2000 AS time_to_elapse,
0.08 AS target_offset,
0.04 AS target_buffer,
0 AS secure,
'' AS layout_type,
0 AS target_y_location
;

UPDATE parameter SET target_buffer=0.03, cue_size=0.03, target_offset=0.1 WHERE parameter_id=2;

select * from parameter;
