library(tidyverse)
library(gtsummary)
library(survey)

# READING THE DATA
db <-
  read.csv('vignettes/for_tamy.csv') %>%
  mutate(truth = as.numeric(factor(truth)) - 1)

# SPECIFYING DESIGN
dsgn <- svydesign(ids = ~psu,
                  strat = ~strat,
                  weights = ~weight,
                  nest = TRUE,
                  data = db)

# ROC CURVE - WITHOUT SE
roc <-  svyroc(dsgn,
       prediction = "predicted",
       label = "truth")

# ROC CURVE - BOOTSTRAP SE
roc <-  svyroc(dsgn,
               prediction = "predicted",
               label = "truth",
               se_method = "bootstrap",
               n_boot = 200)

# ROC CURVE - ASYMPTOTIC SE
roc <-  svyroc(dsgn,
               prediction = "predicted",
               label = "truth",
               se_method = "asymptotic")

# PLOTTING ROC CURVE
roc$roc %>%
  ggplot(aes(x = fpr, y = tpr)) +
  geom_step() +
  geom_rect(aes(ymin = tpr_ci_ll,
                ymax = tpr_ci_ul,
                xmin = fpr,
                xmax = lead(fpr),
                fill = "95% Asymptotic Confidence Interval"), alpha = .5)




