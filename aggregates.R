library(plyr)
library(reshape)

#  == Computing aggregates at the user/cue level ==
get_user_aggregate <- function(events, trial_list, block_structure, time_to_show=2000, ncues=4500){
  current_scenario_id = unique(events$scenario_id)
  current_trial_list = subset(trial_list, scenario_id == current_scenario_id)
  
  print(sprintf("[INFO] Token %s", unique(events$user_token)))
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
  events.dialog <- which(current_trial_list$event_type!='cue')
  cues.before.dialog <- current_trial_list$cue_id[events.dialog-1]
  timing <- cue_appear
  theoretical_diff <- subset(current_trial_list, event_type=='cue' & !cue_id%in%cues.before.dialog)$dist_norm
  time_diff <- timing$event_time_ms[-1] - timing$event_time_ms[1:nrow(timing)-1]
  dist_diff <- time_diff * timing$response_speed[1:nrow(timing)-1] / time_to_show
  dist_diff <- dist_diff[-cues.before.dialog]
  
  diff <- dist_diff - theoretical_diff
  absdiff <- abs(diff)
  # plot(timing$event_time_ms[-cues.before.dialog], absdiff)
  print(sprintf("[INFO] Cues appear difference with reality: mean %f, std %f, max %f", mean(absdiff), sd(absdiff), max(absdiff)))
  
  
  # I want to subset correct/incorrect responses
  all_events <- merge(x = ordered_events, y = current_trial_list, by = "cue_id", all.x = T)
  
  keys.correct.detail <- subset(all_events, response_type == 'keydown-hit')
  keys.incorrect.detail <- subset(all_events, response_type == 'keydown-miss')
  keys.almostcorrect.details <- subset(keys.incorrect.detail, as.numeric(response_value) == as.numeric(event_value))
  
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
  trial_log <- as.data.frame(matrix(data=0, nrow=ncues, ncol=20))
  names(trial_log) <- c('correct.freq', 'almostcorrect.freq', 'incorrect.freq', 
                        'correct.ind' , 'almostcorrect.ind', 'allcorrect.ind', 'incorrect.ind', 'missed.ind', 
                        'score', 'cue_id', 'speed', 'correct.offset', 'incorrect.bestoffset',
                        'cue_number', 'response_count', 
                        'experimental.dist', 'theoretical.dist',
                        'ppt', 'block_structure', 'scenario_id')
  trial_log$correct.freq[keys.correct.sum$x] <- keys.correct.sum$freq
  trial_log$almostcorrect.freq[keys.almostcorrect.sum$x] <- keys.almostcorrect.sum$freq
  trial_log$incorrect.freq[keys.incorrect.sum$x] <- keys.incorrect.sum$freq
  
  trial_log$correct.ind <- as.integer(trial_log$correct.freq==1)
  trial_log$incorrect.ind <- as.integer(trial_log$incorrect.freq>0)
  trial_log$almostcorrect.ind <- as.integer(trial_log$correct.freq==0 & trial_log$almostcorrect.freq>0)
  trial_log$allcorrect.ind <- as.integer(trial_log$correct.freq==1 | trial_log$almostcorrect.freq>0)
  trial_log$missed.ind <- as.integer(trial_log$correct.freq==0 & trial_log$incorrect.freq==0)
  
  trial_log$score <- as.integer(trial_log$correct.ind==1 & trial_log$incorrect.ind==0)

  trial_log$cue_id <- 1:ncues
  trial_log$correct.offset[cues.correct.offset$cue_id] <- cues.correct.offset$response_dist_norm
  trial_log$incorrect.bestoffset[cues.incorrect.offset$cue_id] <- cues.incorrect.offset$response_dist_norm
  trial_log$speed[cue_appear$cue_id] <- cue_appear$response_speed
  trial_log$cue_number <- as.integer(cue_appear$response_value)
  trial_log$category <- subset(current_trial_list, event_type == 'cue')$event_category
  trial_log$response_count <- trial_log$correct.freq + trial_log$incorrect.freq
  trial_log$experimental.dist[-cues.before.dialog] <- dist_diff
  trial_log$theoretical.dist[-cues.before.dialog] <- theoretical_diff
  trial_log$ppt <- unique(ordered_events$user_token)
  
  # trial_log$block_structure <- c(rep(0, SeqLength), sort(rep(1:8, 540)), rep(9, SeqLength*5))
  trial_log$block_structure <- block_structure
  trial_log$scenario_id <- current_scenario_id
  
  return(trial_log)
}

get_trial_log <- function(mturk_data, trial_list, block_structure, time_to_show, ncues) {
  return(do.call(
      rbind, 
      by(mturk_data, 
         mturk_data$user_token, 
         function(x) {return(get_user_aggregate(x, trial_list, block_structure, time_to_show, ncues))}))
  )  
}

# == Computing stats over the logs ==

compute_stats <- function(trial_log, vars=c("correct.ind", "incorrect.ind", "almostcorrect.ind", "missed.ind")) {
  fmlstr <- paste("cbind(", paste(vars, collapse=", "), ") ~ 
                       block_structure + category + ppt", sep="")
  df_agg <- aggregate(data=trial_log, 
                      formula(fmlstr), 
                      FUN=function(x) c(mn=mean(x), sd=sd(x) ))
  return(do.call(data.frame, df_agg))
}

compute_stats_diff <- function(stats, vars=c("correct.ind", "incorrect.ind", "almostcorrect.ind", "missed.ind")) {
  fmlstr <- paste("cbind(", paste(vars, collapse=", "), ") ~ 
                  block_structure + ppt", sep="")
  return(aggregate(data=stats, 
                   formula(fmlstr), 
                   FUN=function(x) x[2]-x[1]))
}

compute_general_stats_diff <- function(stats, vars=c("correct.ind", "incorrect.ind", "almostcorrect.ind", "missed.ind")) {
  fmlstr <- paste("cbind(", paste(vars, collapse=", "), ") ~ 
                  block_structure", sep="")
  return(aggregate(data=stats, 
                   formula(fmlstr), 
                   FUN=mean))
}

# == Compute signature stats over logs ==

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