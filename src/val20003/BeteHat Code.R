bhat <-
  function(est, stderrest, rmse, dfres)
  {
    kvalue <- (stderrest^2)/rmse
    est.hat <- sum(est/kvalue)/sum(1/kvalue)
    sigma.hat <- sum(rmse * dfres)/sum(dfres)
    SSnull <- sum((est - est.hat)^2/kvalue)
    F.obs <- (SSnull/(length(est) - 1))/sigma.hat
    prb.F.obs <- 1 - pf(F.obs, (length(est) - 1), sum(dfres))
    ret.val <- list("F-statistic" = F.obs,
                    "Prob(F-statistic)" = prb.F.obs,
                    "MS Error Pooled" = sigma.hat)
    return(ret.val)
  }

