# test_cpv() works correctly

    $pval
    [1] 0.0695
    
    $critical
           5%       10%       90%       95% 
    -1.633993 -1.263570  1.312383  1.658018 
    

# simes() works correctly

    $reject
    [1] FALSE
    
    $alpha
    [1] 0.1
    
    $details
      pvals  alpha_adj reject
    1  0.15 0.03333333  FALSE
    2  0.20 0.06666667  FALSE
    3  0.30 0.10000000  FALSE
    

---

    $reject
    [1] TRUE
    
    $alpha
    [1] 0.1
    
    $details
      pvals  alpha_adj reject
    1  0.01 0.03333333   TRUE
    2  0.20 0.06666667  FALSE
    3  0.50 0.10000000  FALSE
    

# multi_cutoff() works correctly

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 | x1 + z2 
    Iterations:  0 
    Final selection:  Outliers found:  9     Outliers proportion:  0.009 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 | x1 + z2 
    Iterations:  0 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    

