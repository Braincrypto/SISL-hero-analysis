data <- subset(mturk_data.learning.trial_log, scenario_id == 16)
ts <- get_trial_signature(trial_log, 8, seqlen)

plot_curves(ts, ycolumn="correct.offset.computed.mn", xcolumn="seq.cue.id")
