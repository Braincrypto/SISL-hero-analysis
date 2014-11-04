library(plyr)
library(reshape)
library(ggplot2)

# Computing aggregates at the user/cue level
get_user_aggregate <- function(events, trial_list){
  SeqLength <- 30
  # events needs to be sorted by timestamp
  ordered_events <- arrange(events, event_time_ms)
  
  # cue-created values aren't guaranteed to be in order.
  # These need to be sorted by cue_id
  # This serves to confirm the trial list they were given
  cue_appear <- subset(ordered_events, response_type=='cue-created')
  cue_appear_idordered <- arrange(cue_appear, cue_id)
  if (sum(abs(cue_appear$response_id - cue_appear_idordered$response_id)) > 0) {
    sprintf("Cues did not appear in the right order for user", unique(events$user_token))
  }
  
  # I want to subset correct/incorrect responses
  keys.correct.detail <- subset(ordered_events, response_type == 'keydown-hit')
  keys.incorrect.detail <- subset(ordered_events, response_type == 'keydown-miss')
  
  # For each TRIAL/CUE, give me the number of correct/incorrect responses
  keys.correct.sum <- count(keys.correct.detail$cue_id)
  keys.incorrect.sum <- count(keys.incorrect.detail$cue_id)
  
  # we stick this all in a trial log
  trial_log <- as.data.frame(matrix(data=0, nrow=4500, ncol=10))
  names(trial_log) <- c('correct', 'incorrect', 'missed', 'score', 
                       'trial', 'cue_value', 'response_count', 'ppt', 'block_structure')
  trial_log$correct[keys.correct.sum$x] <- keys.correct.sum$freq
  trial_log$incorrect[keys.incorrect.sum$x] <- keys.incorrect.sum$freq
  trial_log$missed <- as.integer(trial_log$correct==0&trial_log$incorrect==0)
  trial_log$score <- as.integer(trial_log$correct==1&trial_log$incorrect==0)
  trial_log$trial <- 1:4500
  trial_log$cue_id <- as.integer(cue_appear$response_value)
  trial_log$category <- subset(trial_list, event_type == 'cue')$event_category
  trial_log$response_count <- trial_log$correct+trial_log$incorrect
  trial_log$ppt <- unique(ordered_events$user_token)
  trial_log$block_structure <- c(rep(0,30),sort(rep(1:8,540)),rep(9,SeqLength*5))
  
  return(trial_log)
}

get_trial_log <- function(mturk_data, trial_list) {
  return(do.call(
      rbind, 
      by(mturk_data, 
         mturk_data$user_token, 
         function(x) {return(get_user_aggregate(x, trial_list))}))
  )  
}

compute_stats <- function(trial_log) {
  df_agg <- aggregate(data=trial_log, cbind(missed, correct, incorrect) ~ block_structure + category + ppt, FUN=function(x) c(mn =mean(x), sd=sd(x) ))
  return(do.call(data.frame, df_agg))
}

plot_correct_curves <- function(stats) {
  m <- ggplot(stats, aes(block_structure, correct, group=interaction(ppt, category)))
  m + geom_line(aes(colour = factor(interaction(category)))) + 
    geom_point(aes(colour = factor(interaction(category))))
  # geom_point(aes(colour = factor(interaction(category, ppt))))
}

plot_curves <- function(stats, column) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, column]
  m <- ggplot(tempstats, aes(block_structure, Y, group=interaction(ppt, category)))
  m + geom_line(aes(colour = factor(interaction(category)))) + 
    geom_point(aes(colour = factor(interaction(category)))) +
    ylab(column)
  # geom_point(aes(colour = factor(interaction(category, ppt))))
}

# Reading data
mturk_data <- read.csv('data/analysis_output_response.csv', header=T)
trial_list <- read.csv('data/trial_list_event.csv')

# apply patch
mturk_data$response_type <- mturk_data$response_type_corrected
# remove cheater
mturk_data <- mturk_data[-which(mturk_data$user_token %in% c('37XITHEISXIFWGSIWHB75ALT3HCCR5', 'A366DNZBOBFEC8-308Q0PEVB9M05JIWUA77PSJUABG9IT')), ]

# which participants ran the study
ppts.run <- unique(mturk_data$user_token)
# How many people actually finished the experiment?
ppts.finished <- mturk_data$user_token[grep('recogRating.5',mturk_data$response_type)]
# Create data frame of everyone who finished (THESE PEOPLE DESERVE CREDIT/MONEY)
mturk_data.finished <- subset(mturk_data,mturk_data$user_token%in%ppts.finished)
# Now I want the cue lists
mturk_data.cues <- subset(mturk_data.finished,mturk_data.finished$response_type=='cue-created')
# And need to check if we got all the batches!
count.of.cues.created <- count(mturk_data.cues$user_token)
# completed actually refers to if the data set is complete (we have all the batches)
ppts.complete <- count.of.cues.created$x[count.of.cues.created$freq==4500]
# This is the subset of the data with all the usable data
mturk_data.complete <-subset(mturk_data,mturk_data$user_token%in%ppts.complete)


# Now I want to create a readable/scorable trial log
mturk_data.trial_log <- get_trial_log(mturk_data.complete, trial_list)
stats <- compute_stats(mturk_data.trial_log)
plot_curves(stats, column="correct.mn")
plot_curves(stats, column="correct.sd")
plot_curves(stats, column="incorrect.mn")
plot_curves(stats, column="incorrect.sd")

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
