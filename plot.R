library(ggplot2)

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