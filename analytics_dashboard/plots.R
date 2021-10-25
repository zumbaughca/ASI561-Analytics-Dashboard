y_label_map = list(
  "wt" = "Weight, kg",
  "hh" = "Hip Height, cm",
  "adg" = "ADG, kg/d",
  "adfi" = "Average Daily Feed Intake, kg/d",
  "gf" = "Gain : Feed"
)

plot_means = function(df, x_var, y_var, xlabel, ylabel, error = NULL) {
  p = ggplot(data = df, aes(x = x_var,
                            y = y_var)) +
    geom_col(fill = '#512888') +
    geom_errorbar(aes(ymin = y_var - error,
                      ymax = y_var + error),
                  width = 0.2) +
    xlab(xlabel) +
    ylab(y_label_map[[ylabel]]) +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 1),
      axis.text = element_text(size = 18,
                               face = "bold"),
      axis.title = element_text(size = 18,
                                face = "bold")
    )
  return(p)
}