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