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

plot_curves <- function(stats, column) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, column]
  m <- ggplot(tempstats, aes(block_structure, Y, group=interaction(ppt, category)))
  m + geom_line(aes(colour = factor(interaction(category)))) + 
    geom_point(aes(colour = factor(interaction(category)))) +
    ylab(column)
}

plot_diff_curves <- function(stats, column) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, column]
  m <- ggplot(tempstats, aes(block_structure, Y, group=interaction(ppt)))
  m + geom_line(aes(colour = factor(interaction(ppt)))) + 
    geom_point(aes(colour = factor(interaction(ppt)))) +
    ylab(column)
}

plot_general_diff_curves <- function(stats, column) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, column]
  m <- ggplot(tempstats, aes(block_structure, Y))
  m + geom_line() + 
    geom_point() +
    ylab(column)
}

plot_offset_hist <- function(stats, column) {
  tempstats <- stats
  tempstats[, "X"] <- stats[, column]
  m = ggplot(tempstats, 
             aes(x=X, color=as.factor(round(speed,1))))
  m + geom_density(alpha=.01, kernel='triangular')
}

stats <- compute_stats(
  mturk_data.trial_log, c("correct.ind", "incorrect.ind", "almostcorrect.ind", "allcorrect.ind", "missed.ind"))
stats_diff <- compute_stats_diff(
  stats, c("correct.ind.mn", "incorrect.ind.mn", "almostcorrect.ind.mn", "allcorrect.ind.mn", "missed.ind.mn"))
stats_general_diff <- compute_general_stats_diff(
  stats_diff, c("correct.ind.mn", "incorrect.ind.mn", "almostcorrect.ind.mn", "allcorrect.ind.mn", "missed.ind.mn"))

plot_curves(stats, column="correct.ind.mn")
plot_curves(stats, column="correct.ind.sd")
plot_curves(stats, column="incorrect.ind.mn")
plot_curves(stats, column="incorrect.ind.sd")
plot_curves(stats, column="almostcorrect.ind.mn")
plot_curves(stats, column="almostcorrect.ind.sd")
plot_curves(stats, column="allcorrect.ind.mn")
plot_curves(stats, column="allcorrect.ind.sd")
plot_curves(stats, column="missed.ind.mn")
plot_curves(stats, column="missed.ind.sd")

plot_diff_curves(stats_diff, column="correct.ind.mn")
plot_diff_curves(stats_diff, column="incorrect.ind.mn")
plot_diff_curves(stats_diff, column="almostcorrect.ind.mn")
plot_diff_curves(stats_diff, column="allcorrect.ind.mn")
plot_diff_curves(stats_diff, column="missed.ind.mn")

plot_general_diff_curves(stats_general_diff, column="correct.ind.mn")
plot_general_diff_curves(stats_general_diff, column="almostcorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, column="allcorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, column="incorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, column="missed.ind.mn")

plot_offset_hist(subset(mturk_data.trial_log, correct.ind == 1), column="correct.offset")
plot_offset_hist(subset(mturk_data.trial_log, incorrect.ind == 1), column="incorrect.bestoffset")
