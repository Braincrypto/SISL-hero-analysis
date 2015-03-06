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
