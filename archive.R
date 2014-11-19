# TO DO
# Double check the category assignment from the trial list correct
check_trial_list <- function(trial_list, seq_length) {
  sequence_repeated <- subset(trial_list, event_category==1)
  all_sequences <- matrix(sequence_repeated$event_value, ncol=seq_length, byrow=T)
  return(all_sequences)
}

# Extracted sequences
seqs <- check_trial_list(trial_list, 30)
dist_seqs <- count(seqs)


# Summarize pertformance based on category for each value in the block_structure column
# Also, we need to summarize the recognition data


# 
# 
# 
# if (all(cue_zone.enter$trial_value[44:55]==cue_zone.enter$trial_value[56:67])){
#   Sequence = cue_zone.enter$trial_value[44:55]
# }
# # ------ TRIAL LOG SETUP --------
# # Here, I want to setup the trials that the participant received. This will be a condensed version of the event log
# # So, instead of discrete events, we'll have an event log that is trial based
# #
# # First, let's pull the trial row information and trial values from the cues that appeared/entered the zone
# trial_log<-cue_zone.enter[,c('trial_row_id','trial_value')]
# # And the associated block structure
# 
# # I need to find iterations of the sequence to generate the category index
# # I know we have 7 demo trials and 24 practice, then we start at the next trial
# start_trial = (1+7+24)
# potential_sequence_starts<-seq(from=start_trial,by=12,to=length(trial_log$value))
# training_sequence_starts <-unlist(lapply(potential_sequence_starts,function(x){all(trial_log$value[x:(x+11)]==Sequence)}))
# category<-matrix(data=mapply(rep,times=12,x=training_sequence_starts),nrow=(length(potential_sequence_starts)*12),ncol=1)
# category<-c(rep(0,31),category)
# trial_log$category<-category
# # ------ I need to log the recognition ratings -----
# # Then I can compare the order of ratings to the presentation order
# recognition_ratings<-events$event_value[which(events$event_type=='DIALOG_RATING')]
# recognition_order<-trial_log$category[which(trial_log$block_structure==8)][seq(from=1,by=24,to=120)]
# recognition_log<-data.frame(cbind(rep(trial_log$ppt[1],times=length(recognition_ratings)),seq(from=1,by=1,to=length(recognition_ratings)),recognition_order,recognition_ratings)) 
# names(recognition_log)<-c('ppt','order','category','rating')
# return(trial_log)
# #return(recognition_log)