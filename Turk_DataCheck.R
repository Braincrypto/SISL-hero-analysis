library(plyr)

# Computing aggregates at the user/cue level
CreateTrialLog <- function(events){
  SeqLength <- 30
  # events needs to be sorted by timestamp
  ordered_events <- arrange(events, event_time_ms)
  
  # cue-created values aren't guaranteed to be in order.
  # These need to be sorted by cue_id
  # This serves to confirm the trial list they were given
  cue_appear <- subset(events, response_type=='cue-created')
  # I want to subset correct/incorrect responses
  keys.correct.detail <- subset(events, response_type=='keydown-hit')
  keys.incorrect.detail <- subset(events, response_type=='keydown-miss')
  
  # For each TRIAL/CUE, give me the number of correct/incorrect responses
  keys.correct.sum <- count(keys.correct.detail$cue_id)
  keys.incorrect.sum <- count(keys.incorrect.detail$cue_id)
  
  # we stick this all in a trial log
  trialLog <- as.data.frame(matrix(data=0, nrow=4500, ncol=9))
  names(trialLog) <- c('correct', 'incorrect', 'missed', 'score', 
                       'trial', 'cue_value', 'response_count', 'ppt', 'block_structure')
  trialLog$correct[keys.correct.sum$x] <- keys.correct.sum$freq
  trialLog$incorrect[keys.incorrect.sum$x] <- keys.incorrect.sum$freq
  trialLog$missed <- as.integer(trialLog$correct==0&trialLog$incorrect==0)
  trialLog$score <- as.integer(trialLog$correct==1&trialLog$incorrect==0)
  trialLog$trial <- 1:4500
  trialLog$cue_value <- cue_appear$response_value
  trialLog$response_count <- trialLog$correct+trialLog$incorrect
  trialLog$ppt <- unique(events$user_token)
  trialLog$block_structure <- c(rep(0,30),sort(rep(1:8,540)),rep(9,SeqLength*5))
  
  return(trialLog)
}

# Start of code
# reading in the turk data
turkData <- read.csv('data/analysis_output_response.csv', header=T)
# apply patch
turkData$response_type <- turkData$response_type_corrected

# which participants ran the study
ppts.run<-unique(turkData$user_token)
# How many people actually finished the experiment?
ppts.finished<-turkData$user_token[grep('recogRating.5',turkData$response_type)]
# Create data frame of everyone who finished (THESE PEOPLE DESERVE CREDIT/MONEY)
turkData.finished<-subset(turkData,turkData$user_token%in%ppts.finished)
# Now I want the cue lists
turkData.cues<-subset(turkData.finished,turkData.finished$response_type=='cue-created')
# And need to check if we got all the batches!
count.of.cues.created<-count(turkData.cues$user_token)
# completed actually refers to if the data set is complete (we have all the batches)
ppts.complete<-(count.of.cues.created$x[count.of.cues.created$freq==4500])
# This is the subset of the data with all the usable data
turkData.complete<-subset(turkData,turkData$user_token%in%ppts.complete)
# Now I want to create a readable/scorable trial log
turkData.TrialLog<-do.call(rbind,by(turkData.complete,turkData.complete$user_token,CreateTrialLog))

# I need to make sure the trial_list and cue_events match up, so I need to load this in.
# Plus, it has info on the sequence, which will make my life easier.
trial_list<-read.csv('data/trial_list_event.csv')
trial_list_cues<-subset(trial_list,event_type=='cue')

# This will return true if the trial lists are all matching
all(turkData.TrialLog$cue_value==rep(trial_list_cues$event_value,dim(turkData.TrialLog)[1]/4500))

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
