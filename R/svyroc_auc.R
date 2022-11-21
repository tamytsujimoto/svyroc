#' Function to compute Area Under the Curve using trapezoid method
#'
#' @param fpr a numeric vector with false positive rate (FPR) from the ROC curve
#' @param tpr a numeric vector with true positive rate (TPR) from the ROC curve
#'
#' A function that computes the numeric value under the ROC curve with trapezoidal rule
#'
#' @return a numeric value for the area under the ROC curve (AUC-ROC)
#' @export

svyroc_auc <- function(fpr, tpr){

  auc <- 0
  for (i in 2:length(fpr)) {
    auc <- auc + 0.5 * (fpr[i] - fpr[i - 1]) * (tpr[i] + tpr[i - 1])
  }

  return(auc)

}
