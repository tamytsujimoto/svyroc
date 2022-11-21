estimate_mu_pi1 <- function(weight){

  N_hat <- sum(weight)
  mu_pi1_hat <- sum(weight^2)/N_hat - 1

  return(mu_pi1_hat)

}

#' Function to compute design-adjusted variance for ROC curve
#'
#' @param roc_data output from svyroc_estimate
#' @param design survey.design object specifying the complex survey design
#' @param fpr_grid NULL (default) or numeric vector ranging from 0 to 1 specifying the desired cutpoints for the FPR.
#' If set to NULL, the cutpoints are defined by the unique values of the predictor
#' @export

svyroc_var <- function(roc_data,
                       design,
                       fpr_grid){

  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")

  roc_svy <-
    roc_data$roc %>%
    mutate(fpr_grid = cut(fpr, c(-Inf,fpr_grid), include.lowest = TRUE, labels = fpr_grid)) %>%
    group_by(fpr_grid) %>%
    summarise(fpr = max(fpr),
              tpr = max(tpr),
              cutoff = min(cutoff))

  c <- roc_svy$cutoff
  fp <- roc_svy$fpr
  tp <- roc_svy$tpr

  n <- dim(design$variables)[1]
  wt <- 1/(design$prob)
  mu_pi1 <- estimate_mu_pi1(wt)
  p1 <- svymean(~D, design)[1]
  sampling_frac <- n/sum(wt)

  dsgnD1 <- subset(design, D == 1)
  dsgnD0 <- subset(design, D == 0)

   f0 =
     svysmooth(~X, design = dsgnD0, bandwidth = 0.0001)$X %>%
     as_tibble() %>%
     mutate(aux = cut(x, c(-Inf, c), include.lowest = TRUE)) %>%
     group_by(aux) %>%
     filter(row_number() == n()) %>%
     select(aux, f0 = y)
  #
   f1 =
     svysmooth(~X, design = dsgnD1, bandwidth = 0.0001)$X %>%
     as_tibble() %>%
     mutate(aux = cut(x, c(-Inf, c), include.lowest = TRUE)) %>%
     group_by(aux) %>%
     filter(row_number() == n()) %>%
     select(aux, f1 = y)
  #
    ratio <-
      f1 %>%
      full_join(f0, by = 'aux')
  #
    roc_svy <-
      roc_svy %>%
      mutate(aux = cut(cutoff, c(-Inf,cutoff), include.lowest = TRUE)) %>%
      left_join(ratio, by = "aux") %>%
      fill(f0, f1) %>%
      #filter(fpr > 0) %>%
      mutate(
        ratio = f1/f0,
        tpr_var = (1/n)*(sampling_frac*(1+mu_pi1))*((1-p1)^(-1)*ratio^2*fpr*(1-fpr) + p1^(-1)*tpr*(1-tpr)),
        tpr_ci_ll = tpr - 1.96*sqrt(tpr_var),
        tpr_ci_ul = tpr + 1.96*sqrt(tpr_var)) %>%
      select(-c(fpr, aux, f0, f1, ratio)) %>%
      rename(fpr = fpr_grid) %>%
      mutate(fpr = as.numeric(as.character(fpr)))

    roc_data$roc <- roc_svy

    # COMPUTING CI FOR AUC

    roc_data$auc <- data.frame(auc = roc_data$auc,
                               auc_ci_ll = svyroc_auc(roc_svy$fpr, roc_svy$tpr_ci_ll),
                               auc_ci_ul = svyroc_auc(roc_svy$fpr, roc_svy$tpr_ci_ul))

  return(roc_data)

}
