#' Estimate a survey-weighted ROC curve
#'
#' @param design survey.design object specifying the complex survey design
#' @param prediction character value specifying the name of the variable containing the predicted value
#' @param label character value specifying the name of the binary response encoded 0 (control) and 1 (cases)
#' @param fpr_grid NULL (default) or numeric vector ranging from 0 to 1 specifying the desired cutpoints for the FPR.
#' If set to NULL, the cutpoints are defined by the unique values of the predictor
#'
#' This function estimates the ROC curve non-parametrically while accounting for the complex sampling design
#'
#' @return A list containing the ROC curve and the corresponding AUC

svyroc_estimate <- function(design,
                            prediction,
                            label,
                            fpr_grid = NULL) {

  data <- design$variables
  D <- pull(data, label)
  X <- pull(data, prediction)

  #if(unique(D) != c(0,1)) stop("label variable must be c(0,1)")

  wt <- 1/design$prob
  wt <- wt*(!is.na(X))

  X.order <- order(X, decreasing = TRUE, na.last = NA)

  TTT <- X[X.order]
  TPF <- cumsum(wt[X.order]*(D[X.order] == 1))/sum(wt*(D == 1))
  FPF <- cumsum(wt[X.order]*(D[X.order] == 0))/sum(wt*(D == 0))

  dups <- rev(duplicated(rev(TTT)))
  tp <- TPF[!dups]
  fp <- FPF[!dups]
  cutoffs <- TTT[!dups]

  tp <- ifelse(tp > 1, 1, tp)
  fp <- ifelse(fp > 1, 1, fp)

  roc <-
    data.frame(fpr = c(0,fp),
               tpr = c(0,tp),
               cutoff = c(Inf, cutoffs))

  ################
  # COMPUTING AUC
  ################

  auc <- svyroc_auc(roc$fpr, roc$tpr)

  ##########################################
  # COMPUTING ROC VALUES FOR SPECIFIED GRID
  ##########################################

  if(!is.null(fpr_grid)) {

    roc <-
      roc %>%
      mutate(fpr_grid = cut(fpr, c(-Inf,{{fpr_grid}}), include.lowest = TRUE, labels = {{fpr_grid}})) %>%
      group_by(fpr_grid) %>%
      summarise(tpr = max(tpr),
                cutoff = min(cutoff)) %>%
      rename(fpr = fpr_grid) %>%
      mutate(fpr = as.numeric(as.character(fpr)))

  }

  return(list(roc = roc, auc = auc))

}
