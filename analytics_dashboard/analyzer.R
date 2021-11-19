library(lsmeans)
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)

df <- read_xlsx('www/ASI560_F21_growth_data.xlsx') %>%
  filter(!is.na(trt) & !is.na(block) & !is.na(wt14_lbs)) %>%
  mutate(block = as.factor(block),
         trt = as.factor(trt),
         wt14 = wt14_lbs / 2.20462,
         hh14 = hh14_in * 2.54,
         adg14 = (wt14 - wt0) / 14,
         delta_hh14 = (hh14 - hh0),
         wt28 = wt28_lbs / 2.20462,
         hh28 = hh28_in * 2.54,
         adg28 = (wt28 - wt0) / 28,
         wt42 = wt42_lbs / 2.20462,
         hh42 = hh42_in * 2.54,
         adg42 = (wt42 - wt0) / 42)

# Reference class declaration to hold the variables of interest and the model
# fit. This class contains getter methods to access the model summary, the ANOVA
# summary, and the predicted means of each group.
Model <- setRefClass("Model",
                    fields = c(
                      variable = "character",
                      day = "character",
                      fit = "lmerModLmerTest"
                    ),
                    methods = list(
                      fit_model = function() {
                        if (!((variable == "adg") & (day == "0"))) {
                          dv <- paste(variable, day, sep = "")
                          form = as.formula(paste(dv, "~", "trt + (1|block)"))
                          mod <- lmer(form, data = df)
                          fit <<- mod
                        }
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
                        means = lsmeans(fit, ~ trt)
                        table = summary(means)
                        table[, 2:ncol(table)] = round(table[, 2:ncol(table)], 4)
                        return(table)
                      },
                      get_pairwise = function() {
                        means = lsmeans(fit, pairwise ~ trt)
                        return(means$contrasts)
                      }
                    ))

