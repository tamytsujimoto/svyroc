#' Estimate a survey-weighted ROC curve
#'
#' Constructs the ROC curve to a data from complex survey design with asymptotic design-based standard error
#'
#' @param design survey.design object specifying the complex survey design
#' @param prediction character value specifying the name of the variable containing the predicted value
#' @param label character value specifying the name of the binary response encoded 0 (control) and 1 (cases)
#' @param fpr_grid NULL (default) or numeric vector ranging from 0 to 1 specifying the desired cutpoints for the FPR.
#' If set to NULL, the cutpoints are defined by the unique values of the predictor
#' @param se_method NULL (default) if standard error is not necessary; "asymptotic" to compute asymptotic SE, and "bootstrap" for bootstrap SE
#' @param n_boot number of bootstrap replicates used to compute SE when se_method = "bootstrap"
#'
#' @return list containing the ROC curve and the corresponding AUC
#' @export

svyroc <- function(design,
                   prediction,
                   label,
                   fpr_grid = NULL,
                   se_method = NULL,
                   n_boot = 250) {

  # ESTIMATING ROC CURVE AND AUC
  df_roc <- svyroc_estimate(design,
                            prediction,
                            label,
                            fpr_grid)

   # ESTIMATING VARIANCE

  if(!is.null(se_method)){

    if(is.null(fpr_grid)) fpr_grid <- seq(0,1,.01)

      if(se_method == "asymptotic") {

        design$variables <-
          design$variables %>%
          rename(X = all_of(prediction),
                 D = all_of(label))

        df_roc <- svyroc_var(df_roc, design, fpr_grid)

      } else if(se_method == "bootstrap") {

        var_boot <- svyroc_var_boot(design = design,
                                    prediction = prediction,
                                    label = label,
                                    n_boot = n_boot,
                                    fpr_grid = fpr_grid)

        df_roc$roc <-
          df_roc$roc %>%
          mutate(fpr = cut(fpr, c(-Inf,fpr_grid), include.lowest = TRUE, labels = fpr_grid)) %>%
          group_by(fpr) %>%
          summarise(tpr = max(tpr),
                    cutoff = min(cutoff)) %>%
          mutate(fpr = as.numeric(as.character(fpr))) %>%
          left_join(var_boot$roc, by = 'fpr')

        df_roc$auc <-
          data.frame(auc = df_roc$auc) %>%
          bind_cols(select(var_boot$auc, contains('_ci_')))


      } else{stop("se_method not listed")}

  }

  return(df_roc)
}
