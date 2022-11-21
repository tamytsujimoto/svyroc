dsgn_boot <- function(design){

  if(design$has.strata){
    d <-
      substitute(expression(
        as.list(as.data.frame(repwt)) %>%
          map(~mutate(design$variables, bweight = .x)) %>%
          map(~svydesign(id = a,
                         strata = b,
                         weight = ~bweight,
                         data = .x))),
        list(a = design$call$id,
             b = design$call$strata))
  } else{
    d <-
      substitute(expression(
        as.list(as.data.frame(repwt)) %>%
          map(~mutate(design$variables, bweight = .x)) %>%
          map(~svydesign(id = a,
                         weight = ~bweight,
                         data = .x))),
        list(a = design$call$id))
  }


  return(eval.parent(d))

}

svyroc_var_boot <- function(design,
                            prediction,
                            label,
                            n_boot,
                            fpr_grid){

  # Bootstrap samples
  wt <- 1/design$prob
  boot <- as.svrepdesign(design, type="bootstrap", replicates=n_boot, compress = FALSE)

  # Bootstrap weights
  repwt <- boot$repweights * wt
  colnames(repwt) <- paste0('RepWt_', 1:ncol(repwt))

  # Computing roc for bootstrap samples
  svyroc_boot <-
    eval(dsgn_boot(design)) %>%
    map(~svyroc_estimate(design = .,
                         prediction = prediction,
                         label = label,
                         fpr_grid = fpr_grid))

  # bootstrap roc
  roc_boot <-
    svyroc_boot %>%
    map_dfr(function(l) return(l$roc)) %>%
    group_by(fpr) %>%
    summarise(tpr_var_boot = var(tpr),
              tpr_ci_ll = quantile(tpr, .025),
              tpr_ci_ul = quantile(tpr, .975)) %>%
    ungroup

  # bootstrap auc
  auc_boot <-
    data.frame(auc = map_dbl(svyroc_boot, function(l) return(l$auc))) %>%
    summarise(auc_var_boot = var(auc),
              auc_ci_ll = quantile(auc, .025),
              auc_ci_ul = quantile(auc, .975)) %>%
    ungroup

  return(list(roc = roc_boot, auc = auc_boot))

}
