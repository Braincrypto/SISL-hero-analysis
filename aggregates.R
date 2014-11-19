library(plyr)
library(reshape)
library(ggplot2)

# Computing aggregates at the user/cue level
get_user_aggregate <- function(events, trial_list){
  print(sprintf("[INFO] Token %s", unique(events$user_token)))
  SeqLength <- 30
  # events needs to be sorted by timestamp
  ordered_events <- arrange(events, event_time_ms)
  
  # cue-created values aren't guaranteed to be in order.
  # These need to be sorted by cue_id
  # This serves to confirm the trial list they were given
  cue_appear <- subset(ordered_events, response_type=='cue-created')
  cue_appear_idordered <- arrange(cue_appear, cue_id)
  
  # check ordering
  if (sum(abs(cue_appear$response_id - cue_appear_idordered$response_id)) > 0) {
    print(sprintf("[CRITICAL] Cues did not appear in the right order for user", unique(events$user_token)))
  }
  
  # check distances
  events.dialog <- which(trial_list$event_type!='cue')
  cues.before.dialog <- trial_list$cue_id[events.dialog-1]
  timing <- cue_appear
  theoretical_diff <- subset(trial_list, event_type=='cue' & !cue_id%in%cues.before.dialog)$dist_norm
  time_diff <- timing$event_time_ms[-1] - timing$event_time_ms[1:nrow(timing)-1]
  dist_diff <- time_diff * timing$response_speed[1:nrow(timing)-1] / 2000
  dist_diff <- dist_diff[-cues.before.dialog]
  
  diff <- dist_diff - theoretical_diff
  absdiff <- abs(diff)
  # plot(timing$event_time_ms[-cues.before.dialog], absdiff)
  print(sprintf("[INFO] Cues appear difference with reality: mean %f, std %f, max %f", mean(absdiff), sd(absdiff), max(absdiff)))
  
  
  # I want to subset correct/incorrect responses
  all_events <- merge(x = ordered_events, y = trial_list, by = "cue_id", all.x = T)
  
  keys.correct.detail <- subset(all_events, response_type == 'keydown-hit')
  keys.incorrect.detail <- subset(all_events, response_type == 'keydown-miss')
  keys.almostcorrect.details <- subset(keys.incorrect.detail, response_value == event_value)
  
  # For each TRIAL/CUE, give me the number of correct/incorrect responses
  keys.correct.sum <- count(keys.correct.detail$cue_id)
  keys.incorrect.sum <- count(keys.incorrect.detail$cue_id)
  keys.almostcorrect.sum <- count(keys.almostcorrect.details$cue_id)
  
  closest_to_zero <- function(x) {
    ord <- order(abs(x))
    return(x[ord[1]])
  }
  cues.correct.offset <- aggregate(response_dist_norm ~ cue_id, data=keys.correct.detail, FUN=closest_to_zero)
  cues.incorrect.offset <- aggregate(response_dist_norm ~ cue_id, data=keys.incorrect.detail, FUN=closest_to_zero)
  
  # we stick this all in a trial log
  trial_log <- as.data.frame(matrix(data=0, nrow=4500, ncol=17))
  names(trial_log) <- c('correct.freq', 'almostcorrect.freq', 'incorrect.freq', 
                        'correct.ind' , 'almostcorrect.ind', 'allcorrect.ind', 'incorrect.ind', 'missed.ind', 
                        'score', 'cue_id', 'speed', 'correct.offset', 'incorrect.bestoffset',
                        'cue_number', 'response_count', 'ppt', 'block_structure')
  trial_log$correct.freq[keys.correct.sum$x] <- keys.correct.sum$freq
  trial_log$almostcorrect.freq[keys.almostcorrect.sum$x] <- keys.almostcorrect.sum$freq
  trial_log$incorrect.freq[keys.incorrect.sum$x] <- keys.incorrect.sum$freq
  
  trial_log$correct.ind <- as.integer(trial_log$correct.freq==1)
  trial_log$incorrect.ind <- as.integer(trial_log$incorrect.freq>0)
  trial_log$almostcorrect.ind <- as.integer(trial_log$correct.freq==0 & trial_log$almostcorrect.freq>0)
  trial_log$allcorrect.ind <- as.integer(trial_log$correct.freq==1 | trial_log$almostcorrect.freq>0)
  trial_log$missed.ind <- as.integer(trial_log$correct.freq==0 & trial_log$incorrect.freq==0)
  
  trial_log$score <- as.integer(trial_log$correct.ind==1 & trial_log$incorrect.ind==0)

  trial_log$cue_id <- 1:4500
  trial_log$correct.offset[cues.correct.offset$cue_id] <- cues.correct.offset$response_dist_norm
  trial_log$incorrect.bestoffset[cues.incorrect.offset$cue_id] <- cues.incorrect.offset$response_dist_norm
  trial_log$speed[cue_appear$cue_id] <- cue_appear$response_speed
  trial_log$cue_number <- as.integer(cue_appear$response_value)
  trial_log$category <- subset(trial_list, event_type == 'cue')$event_category
  trial_log$response_count <- trial_log$correct.freq + trial_log$incorrect.freq
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
