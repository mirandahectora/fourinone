#' Create a Four-in-One Residuals Plot
#'
#' This function creates a set of residual plots which can
#' be used to ascertain the applicability of a given model
#' to a data set. The function's output and the included plots
#' are based off of Minitab®'s "Four in One" residual plots, 
#' which each contain a normal quantile plot, a residual plot 
#' comparing predicted residuals to their actual value, a residual 
#' plot comparing predicted residuals to the order in which they 
#' are observed, and a histogram. 
#'
#' @param model A given fitted model object, such as, but not limited to,
#' those generated using lm() and aov().
#' @param plotcolor An optional value for the color of points shown on
#' each plot.
#' @param nppcolor An optional value for the color of the line shown
#' in the normal quantile plot.
#' @param yintcolor optional value for the color of the y-intercept
#' line shown in the residual plots.
#' @return A plot containing a normal quantile plot, a histogram, and two
#' redisual plots.
#' @export
four_in_one <- function(model, plotcolor, npplinecolor, yintcolor) {

  if(hasArg(plotcolor)) {

    defaultcolor <- plotcolor

  } else {

    defaultcolor <- "#3A9AB2"

  }

  if(hasArg(npplinecolor)) {

    npplinecolor <- npplinecolor

  } else {

    npplinecolor <- "darkgrey"

  }


  if(hasArg(yintcolor)) {

    yintcolor <- yintcolor

  } else {

    yintcolor <- "darkgrey"

  }


  npp <- ggplot(mapping = aes(sample = model$residuals)) +
    stat_qq_line(color = npplinecolor) +
    stat_qq_point(size = 2, color = defaultcolor) +
    xlab("Normal Residuals") +
    ylab("Sample Reisduals") +
    ggtitle("Normal Probability Plot") +
    theme_bw()

  vsfits <- x |>
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_hline(yintercept = 0, color = yintcolor) +
    geom_point(color = defaultcolor) +
    theme_bw() +
    xlab("Fitted Value") +
    ylab("Residual Value") +
    ggtitle("Versus Fits")

  histogram <- ggplot(x, aes(x = model$residuals)) +
    geom_histogram(fill = defaultcolor, col = I("white")) +
    theme_bw() +
    xlab("Residual Value") +
    ylab("Frequency") +
    ggtitle("Histogram")

  vsorder_data <- as.data.frame(model$residuals) |>
    mutate(order = 1:n()) |>
    rename(residuals = "model$residuals")

  vsorder <- vsorder_data |>
    ggplot(aes(x = order, y = residuals)) +
    geom_hline(yintercept = 0,  color = yintcolor) +
    geom_line(color = defaultcolor, size = 0.4) +
    geom_point(color = defaultcolor) +
    theme_bw() +
    xlab("Observation Order") +
    ylab("Residual Value") +
    ggtitle("Versus Order")

  ggarrange(npp, vsfits, histogram, vsorder) |>
    annotate_figure(top = text_grob("Residual Plots for Regression",
                                    face = "bold"))

}

