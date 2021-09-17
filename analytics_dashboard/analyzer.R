library(lsmeans)
library(readxl)
library(tidyverse)

df <- read_xlsx('www/ASI560_S2021_final_data.xlsx')

# Reference class declaration to hold the variables of interest and the model
# fit. This class contains getter methods to access the model summary, the ANOVA
# summary, and the predicted means of each group.
Model <- setRefClass("Model",
                    fields = c(
                      variable = "character",
                      day = "character",
                      fit = "lm"
                    ),
                    methods = list(
                      fit_model = function() {
                        form = as.formula(paste(variable, "~", "Trt"))
                        mod = lm(form, data = df)
                        fit <<- mod
                      },
                      get_model_summary = function() {
                        fit_summary = summary(fit)
                        coefs = round(fit_summary$coefficients, 4)
                        table = cbind(rownames(coefs), coefs)
                        colnames(table)[1] = "Term"
                        return(table)
                      },
                      get_anova_table = function() {
                        aov = round(anova(fit), 4)
                        table = cbind(rownames(aov), aov)
                        colnames(table)[1] = "Source"
                        return(table)
                      },
                      get_means = function() {
                        means = lsmeans(fit, ~ Trt)
                        table = summary(means)
                        table[, 2:ncol(table)] = round(table[, 2:ncol(table)], 4)
                        return(table)
                      },
                      get_pairwise = function() {
                        means = lsmeans(fit, pairwise ~ Trt)
                        return(means$contrasts)
                      }
                    ))

plot_means = function(df, x_var, y_var, xlabel, ylabel, error = NULL) {
  p = ggplot(data = df, aes(x = x_var,
                              y = y_var)) +
    geom_col(fill = '#512888') +
    geom_errorbar(aes(ymin = y_var - error,
                      ymax = y_var + error),
                  width = 0.2) +
    xlab(xlabel) +
    ylab(ylabel) +
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
