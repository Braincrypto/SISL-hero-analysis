data <- subset(mturk_data, scenario_id == 16)
trial_log <- get_trial_log(data, trial_list, block_structure, 2000, num_cues)

get_trial_signature <- function(trial_log, structure, seqlen) {
  filtered_log <- subset(trial_log, category == 1 & block_structure == structure)
  
  return(do.call(
    rbind, 
    by(filtered_log, 
       filtered_log$ppt, 
       function(filtered_events) {
         nb_seq <- nrow(filtered_events) / seqlen
         cue_seq <- rep(1:seqlen, nb_seq)
         filtered_events["seq.cue.id"] <- cue_seq
         filtered_events["correct.offset.computed"] <- filtered_events$correct.offset + 
           filtered_events$incorrect.bestoffset +
           filtered_events$incorrect.ind * filtered_events$experimental.dist
         df_agg <- aggregate(data=filtered_events, 
                            cbind(correct.ind, correct.offset, correct.offset.computed) ~ seq.cue.id + ppt + category + block_structure, 
                             FUN=function(x) c(mn=mean(x), sd=sd(x)))
         return(do.call(data.frame, df_agg));
       }))
  )  
}
ts <- get_trial_signature(trial_log, 8, seqlen)

plot_curves(ts, ycolumn="correct.offset.computed.mn", xcolumn="seq.cue.id")
