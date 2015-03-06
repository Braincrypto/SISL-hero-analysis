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

plot_curves <- function(stats, ycolumn, xcolumn="block_structure") {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, ycolumn]
  tempstats[, "X"] <- stats[, xcolumn]
  m <- ggplot(tempstats, aes(X, Y, group=interaction(ppt, category)))
  m + geom_line(aes(colour = factor(interaction(ppt, category)))) + 
    geom_point(aes(colour = factor(interaction(ppt, category)))) +
    ylab(ycolumn) + scale_x_discrete()
}

plot_diff_curves <- function(stats, ycolumn) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, ycolumn]
  m <- ggplot(tempstats, aes(block_structure, Y, group=interaction(ppt)))
  m + geom_line(aes(colour = factor(interaction(ppt)))) + 
    geom_point(aes(colour = factor(interaction(ppt)))) +
    ylab(ycolumn) + scale_x_discrete()
}

plot_general_diff_curves <- function(stats, ycolumn) {
  tempstats <- stats
  tempstats[, "Y"] <- stats[, ycolumn]
  m <- ggplot(tempstats, aes(block_structure, Y))
  m + geom_line() + 
    geom_point() +
    ylab(ycolumn) + scale_x_discrete()
}

plot_density_per_speed <- function(stats, xcolumn, xmin, xmax, binwidth=0.005, normalize=T) {
  tempstats <- stats
  tempstats[, "X"] <- stats[, xcolumn] 
  if (normalize) {
    tempstats$X <- tempstats$X * 2000 / stats$speed
  }
  m = ggplot(tempstats, 
             aes(x=X, color=as.factor(round(speed,1))))
  m + geom_density(alpha=0.01, kernel='triangular') + coord_cartesian(xlim=c(xmin, xmax))
}

plot_hist_per_speed <- function(stats, xcolumn, xmin, xmax, binwidth=0.005, normalize=T) {
  tempstats <- stats
  tempstats[, "X"] <- stats[, xcolumn] 
  if (normalize) {
    tempstats$X <- tempstats$X * 2000 / stats$speed
  }
  m = ggplot(tempstats, 
             aes(x=X, color=as.factor(round(speed,1))))
  m + geom_freqpoly(binwidth=binwidth) + coord_cartesian(xlim=c(xmin, xmax))
}

stats <- compute_stats(
  mturk_data.trial_log, c("correct.ind", "incorrect.ind", "almostcorrect.ind", "allcorrect.ind", "missed.ind", "speed"))
stats_diff <- compute_stats_diff(
  stats, c("correct.ind.mn", "incorrect.ind.mn", "almostcorrect.ind.mn", "allcorrect.ind.mn", "missed.ind.mn"))
stats_general_diff <- compute_general_stats_diff(
  stats_diff, c("correct.ind.mn", "incorrect.ind.mn", "almostcorrect.ind.mn", "allcorrect.ind.mn", "missed.ind.mn"))

plot_curves(stats, ycolumn="correct.ind.mn")
plot_curves(stats, ycolumn="correct.ind.sd")
plot_curves(stats, ycolumn="incorrect.ind.mn")
plot_curves(stats, ycolumn="incorrect.ind.sd")
plot_curves(stats, ycolumn="almostcorrect.ind.mn")
plot_curves(stats, ycolumn="almostcorrect.ind.sd")
plot_curves(stats, ycolumn="allcorrect.ind.mn")
plot_curves(stats, ycolumn="allcorrect.ind.sd")
plot_curves(stats, ycolumn="missed.ind.mn")
plot_curves(stats, ycolumn="missed.ind.sd")

plot_curves(stats, ycolumn="speed.mn")
plot_curves(stats, ycolumn="speed.sd")

plot_diff_curves(stats_diff, ycolumn="correct.ind.mn")
plot_diff_curves(stats_diff, ycolumn="incorrect.ind.mn")
plot_diff_curves(stats_diff, ycolumn="almostcorrect.ind.mn")
plot_diff_curves(stats_diff, ycolumn="allcorrect.ind.mn")
plot_diff_curves(stats_diff, ycolumn="missed.ind.mn")

plot_general_diff_curves(stats_general_diff, ycolumn="correct.ind.mn")
plot_general_diff_curves(stats_general_diff, ycolumn="almostcorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, ycolumn="allcorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, ycolumn="incorrect.ind.mn")
plot_general_diff_curves(stats_general_diff, column="missed.ind.mn")

plot_density_per_speed(subset(mturk_data.trial_log, correct.ind == 1), xcolumn="correct.offset", -300, 300)
plot_density_per_speed(subset(mturk_data.trial_log, incorrect.ind == 1), xcolumn="incorrect.bestoffset", -500, 500)

plot_hist_per_speed(subset(mturk_data.trial_log, experimental.dist != 0), xcolumn="theoretical.dist", 0, .6, normalize=F)
plot_hist_per_speed(subset(mturk_data.trial_log, experimental.dist != 0), xcolumn="experimental.dist", 0, .6, normalize=F)
