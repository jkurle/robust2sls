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
      pvals  alpha_adj reject_adj
    1  0.15 0.03333333      FALSE
    2  0.20 0.06666667      FALSE
    3  0.30 0.10000000      FALSE
    

---

    $reject
    [1] TRUE
    
    $alpha
    [1] 0.1
    
    $details
      pvals  alpha_adj reject_adj
    1  0.01 0.03333333       TRUE
    2  0.20 0.06666667      FALSE
    3  0.50 0.10000000      FALSE
    

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
    

# proptest() works correctly

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  4 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  32     Outliers proportion:  0.032 
    
    $gamma0.06
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  7 
    Final selection:  Outliers found:  39     Outliers proportion:  0.039 
    
    $gamma0.07
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  44     Outliers proportion:  0.044 
    
    $gamma0.08
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  53     Outliers proportion:  0.053 
    
    $gamma0.09
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  83     Outliers proportion:  0.083 
    
    $gamma0.1
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  93     Outliers proportion:  0.093 
    

---

              iter_test iter_act gamma         t      type       pval alpha reject
    gamma0.01         0        0  0.01 -1.498485 two-sided 0.13400726  0.05  FALSE
    gamma0.02         0        0  0.02 -1.738476 two-sided 0.08212701  0.05  FALSE
    gamma0.03         0        0  0.03 -1.519491 two-sided 0.12863889  0.05  FALSE
    gamma0.04         0        0  0.04 -2.085486 two-sided 0.03702521  0.05   TRUE
    gamma0.05         0        0  0.05 -1.518281 two-sided 0.12894347  0.05  FALSE
    gamma0.06         0        0  0.06 -1.645175 two-sided 0.09993369  0.05  FALSE
    gamma0.07         0        0  0.07 -1.375557 two-sided 0.16895896  0.05  FALSE
    gamma0.08         0        0  0.08 -1.509687 two-sided 0.13112324  0.05  FALSE
    gamma0.09         0        0  0.09  0.000000 two-sided 1.00000000  0.05  FALSE
    gamma0.1          0        0  0.10  0.175567 two-sided 0.86063411  0.05  FALSE

---

              iter_test iter_act gamma          t      type       pval alpha reject
    gamma0.01         1        1  0.01 -1.4387783 two-sided 0.15021334  0.05  FALSE
    gamma0.02         1        1  0.02 -1.4056773 two-sided 0.15981998  0.05  FALSE
    gamma0.03         1        1  0.03 -1.4772377 two-sided 0.13961194  0.05  FALSE
    gamma0.04         1        1  0.04 -2.0041688 two-sided 0.04505198  0.05   TRUE
    gamma0.05         1        1  0.05 -1.4227549 two-sided 0.15480722  0.05  FALSE
    gamma0.06         1        1  0.06 -1.7920213 two-sided 0.07312955  0.05  FALSE
    gamma0.07         1        1  0.07 -1.2321766 two-sided 0.21788313  0.05  FALSE
    gamma0.08         1        1  0.08 -2.1233583 two-sided 0.03372384  0.05   TRUE
    gamma0.09         1        1  0.09 -0.3043243 two-sided 0.76088085  0.05  FALSE
    gamma0.1          1        1  0.10 -0.1950643 two-sided 0.84534264  0.05  FALSE

---

                iter_test iter_act gamma          t      type       pval alpha
    gamma0.01 convergence        4  0.01 -1.6123662 two-sided 0.10688228  0.05
    gamma0.02 convergence        3  0.02 -1.2189020 two-sided 0.22288139  0.05
    gamma0.03 convergence        3  0.03 -1.1937121 two-sided 0.23259059  0.05
    gamma0.04 convergence        3  0.04 -1.5147268 two-sided 0.12984159  0.05
    gamma0.05 convergence        6  0.05 -1.6510595 two-sided 0.09872642  0.05
    gamma0.06 convergence        7  0.06 -1.6741743 two-sided 0.09409635  0.05
    gamma0.07 convergence      200  0.07 -1.8332990 two-sided 0.06675812  0.05
    gamma0.08 convergence      200  0.08 -1.7058333 two-sided 0.08803911  0.05
    gamma0.09 convergence        6  0.09 -0.4002577 two-sided 0.68896671  0.05
    gamma0.1  convergence      200  0.10 -0.3651571 two-sided 0.71499415  0.05
              reject
    gamma0.01  FALSE
    gamma0.02  FALSE
    gamma0.03  FALSE
    gamma0.04  FALSE
    gamma0.05  FALSE
    gamma0.06  FALSE
    gamma0.07  FALSE
    gamma0.08  FALSE
    gamma0.09  FALSE
    gamma0.1   FALSE

---

              iter_test iter_act gamma          t      type      pval alpha reject
    gamma0.01         3        3  0.01 -1.6181489 one-sided 0.9471847  0.05  FALSE
    gamma0.02         3        3  0.02 -1.2367199 one-sided 0.8919045  0.05  FALSE
    gamma0.03         3        3  0.03 -1.2308956 one-sided 0.8908190  0.05  FALSE
    gamma0.04         3        3  0.04 -1.5932008 one-sided 0.9444424  0.05  FALSE
    gamma0.05         3        3  0.05 -1.6774488 one-sided 0.9532726  0.05  FALSE
    gamma0.06         3        3  0.06 -2.0214423 one-sided 0.9783830  0.05  FALSE
    gamma0.07         3        3  0.07 -1.5956969 one-sided 0.9447218  0.05  FALSE
    gamma0.08         3        3  0.08 -1.7630925 one-sided 0.9610576  0.05  FALSE
    gamma0.09         3        3  0.09 -0.5471043 one-sided 0.7078465  0.05  FALSE
    gamma0.1          3        3  0.10 -1.0276042 one-sided 0.8479320  0.05  FALSE

---

              iter_test iter_act gamma          t      type       pval alpha reject
    gamma0.01         5        5  0.01 -1.6126772 two-sided 0.10681465  0.01  FALSE
    gamma0.02         5        5  0.02 -1.2208033 two-sided 0.22216050  0.01  FALSE
    gamma0.03         5        5  0.03 -1.1993882 two-sided 0.23037705  0.01  FALSE
    gamma0.04         5        5  0.04 -1.5298316 two-sided 0.12605842  0.01  FALSE
    gamma0.05         5        5  0.05 -1.6794772 two-sided 0.09305909  0.01  FALSE
    gamma0.06         5        5  0.06 -1.7183770 two-sided 0.08572787  0.01  FALSE
    gamma0.07         5        5  0.07 -2.1945682 two-sided 0.02819459  0.01  FALSE
    gamma0.08         5        5  0.08 -1.7252248 two-sided 0.08448697  0.01  FALSE
    gamma0.09         5        5  0.09 -0.4261803 two-sided 0.66997644  0.01  FALSE
    gamma0.1          5        5  0.10 -0.5638430 two-sided 0.57286096  0.01  FALSE

---

      iter_test iter_act gamma         t      type      pval alpha reject
    1         1        1  0.05 -1.552096 two-sided 0.1206392   0.1  FALSE

---

      iter_test iter_act gamma         t      type      pval alpha reject
    1         1        1  0.05 -1.552096 one-sided 0.9396804   0.1  FALSE

# counttest() works correctly

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  4 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  32     Outliers proportion:  0.032 
    
    $gamma0.06
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  7 
    Final selection:  Outliers found:  39     Outliers proportion:  0.039 
    
    $gamma0.07
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  44     Outliers proportion:  0.044 
    
    $gamma0.08
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  53     Outliers proportion:  0.053 
    
    $gamma0.09
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  83     Outliers proportion:  0.083 
    
    $gamma0.1
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  200 
    Final selection:  Outliers found:  93     Outliers proportion:  0.093 
    

---

              iter_test iter_act gamma num_act num_exp      type      pval alpha
    gamma0.01         0        0  0.01       6      10 two-sided 0.2656770  0.05
    gamma0.02         0        0  0.02      14      20 two-sided 0.2170493  0.05
    gamma0.03         0        0  0.03      24      30 two-sided 0.3146255  0.05
    gamma0.04         0        0  0.04      31      40 two-sided 0.1779894  0.05
    gamma0.05         0        0  0.05      43      50 two-sided 0.3576795  0.05
    gamma0.06         0        0  0.06      52      60 two-sided 0.3326227  0.05
    gamma0.07         0        0  0.07      63      70 two-sided 0.4371423  0.05
    gamma0.08         0        0  0.08      72      80 two-sided 0.4016203  0.05
    gamma0.09         0        0  0.09      90      90 two-sided 1.0000000  0.05
    gamma0.1          0        0  0.10     101     100 two-sided 0.9202780  0.05
              reject
    gamma0.01  FALSE
    gamma0.02  FALSE
    gamma0.03  FALSE
    gamma0.04  FALSE
    gamma0.05  FALSE
    gamma0.06  FALSE
    gamma0.07  FALSE
    gamma0.08  FALSE
    gamma0.09  FALSE
    gamma0.1   FALSE

---

              iter_test iter_act gamma num_act num_exp      type       pval alpha
    gamma0.01         1        1  0.01       5      10 two-sided 0.15054444  0.05
    gamma0.02         1        1  0.02      13      20 two-sided 0.14401442  0.05
    gamma0.03         1        1  0.03      21      30 two-sided 0.11928773  0.05
    gamma0.04         1        1  0.04      26      40 two-sided 0.02633286  0.05
    gamma0.05         1        1  0.05      39      50 two-sided 0.13673055  0.05
    gamma0.06         1        1  0.06      45      60 two-sided 0.05254593  0.05
    gamma0.07         1        1  0.07      59      70 two-sided 0.20897985  0.05
    gamma0.08         1        1  0.08      60      80 two-sided 0.02509043  0.05
    gamma0.09         1        1  0.09      87      90 two-sided 0.79222703  0.05
    gamma0.1          1        1  0.10      98     100 two-sided 0.88081167  0.05
              reject
    gamma0.01  FALSE
    gamma0.02  FALSE
    gamma0.03  FALSE
    gamma0.04   TRUE
    gamma0.05  FALSE
    gamma0.06  FALSE
    gamma0.07  FALSE
    gamma0.08   TRUE
    gamma0.09  FALSE
    gamma0.1   FALSE

---

                iter_test iter_act gamma num_act num_exp      type        pval
    gamma0.01 convergence        4  0.01       4      10 two-sided 0.056294298
    gamma0.02 convergence        3  0.02      13      20 two-sided 0.144014422
    gamma0.03 convergence        3  0.03      21      30 two-sided 0.119287726
    gamma0.04 convergence        3  0.04      26      40 two-sided 0.026332860
    gamma0.05 convergence        6  0.05      32      50 two-sided 0.008727532
    gamma0.06 convergence        7  0.06      39      60 two-sided 0.005375416
    gamma0.07 convergence      200  0.07      44      70 two-sided 0.001210411
    gamma0.08 convergence      200  0.08      53      80 two-sided 0.001708474
    gamma0.09 convergence        6  0.09      83      90 two-sided 0.493239774
    gamma0.1  convergence      200  0.10      93     100 two-sided 0.515706168
              alpha reject
    gamma0.01  0.05  FALSE
    gamma0.02  0.05  FALSE
    gamma0.03  0.05  FALSE
    gamma0.04  0.05   TRUE
    gamma0.05  0.05   TRUE
    gamma0.06  0.05   TRUE
    gamma0.07  0.05   TRUE
    gamma0.08  0.05   TRUE
    gamma0.09  0.05  FALSE
    gamma0.1   0.05  FALSE

---

              iter_test iter_act gamma num_act num_exp      type      pval alpha
    gamma0.01         3        3  0.01       4      10 one-sided 0.9896639  0.05
    gamma0.02         3        3  0.02      13      20 one-sided 0.9609880  0.05
    gamma0.03         3        3  0.03      21      30 one-sided 0.9647154  0.05
    gamma0.04         3        3  0.04      26      40 one-sided 0.9924336  0.05
    gamma0.05         3        3  0.05      33      50 one-sided 0.9956071  0.05
    gamma0.06         3        3  0.06      37      60 one-sided 0.9994236  0.05
    gamma0.07         3        3  0.07      50      70 one-sided 0.9948595  0.05
    gamma0.08         3        3  0.08      56      80 one-sided 0.9980168  0.05
    gamma0.09         3        3  0.09      82      90 one-sided 0.8140138  0.05
    gamma0.1          3        3  0.10      84     100 one-sided 0.9536794  0.05
              reject
    gamma0.01  FALSE
    gamma0.02  FALSE
    gamma0.03  FALSE
    gamma0.04  FALSE
    gamma0.05  FALSE
    gamma0.06  FALSE
    gamma0.07  FALSE
    gamma0.08  FALSE
    gamma0.09  FALSE
    gamma0.1   FALSE

---

              iter_test iter_act gamma num_act num_exp      type         pval alpha
    gamma0.01         5        5  0.01       4      10 two-sided 0.0562942979  0.01
    gamma0.02         5        5  0.02      13      20 two-sided 0.1440144221  0.01
    gamma0.03         5        5  0.03      21      30 two-sided 0.1192877265  0.01
    gamma0.04         5        5  0.04      26      40 two-sided 0.0263328596  0.01
    gamma0.05         5        5  0.05      32      50 two-sided 0.0087275321  0.01
    gamma0.06         5        5  0.06      39      60 two-sided 0.0053754156  0.01
    gamma0.07         5        5  0.07      40      70 two-sided 0.0001566919  0.01
    gamma0.08         5        5  0.08      54      80 two-sided 0.0025073073  0.01
    gamma0.09         5        5  0.09      83      90 two-sided 0.4932397737  0.01
    gamma0.1          5        5  0.10      90     100 two-sided 0.3419450172  0.01
              reject
    gamma0.01  FALSE
    gamma0.02  FALSE
    gamma0.03  FALSE
    gamma0.04  FALSE
    gamma0.05   TRUE
    gamma0.06   TRUE
    gamma0.07   TRUE
    gamma0.08   TRUE
    gamma0.09  FALSE
    gamma0.1   FALSE

---

      iter_test iter_act gamma num_act num_exp      type      pval alpha reject
    1         1        1  0.05      38      50 two-sided 0.1030514   0.1  FALSE

---

      iter_test iter_act gamma num_act num_exp      type      pval alpha reject
    1         1        1  0.05      38      50 one-sided 0.9660451   0.1  FALSE

# multi_cutoff_to_fodr_vec() works correctly

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  4 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  32     Outliers proportion:  0.032 
    
    $gamma0.06
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  7 
    Final selection:  Outliers found:  39     Outliers proportion:  0.039 
    
    $gamma0.07
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  20 
    Final selection:  Outliers found:  44     Outliers proportion:  0.044 
    
    $gamma0.08
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  20 
    Final selection:  Outliers found:  52     Outliers proportion:  0.052 
    
    $gamma0.09
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  83     Outliers proportion:  0.083 
    
    $gamma0.1
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  20 
    Final selection:  Outliers found:  92     Outliers proportion:  0.092 
    

---

      gamma0.01   gamma0.02   gamma0.03   gamma0.04   gamma0.05   gamma0.06 
    -0.12649111 -0.18973666 -0.18973666 -0.28460499 -0.22135944 -0.25298221 
      gamma0.07   gamma0.08   gamma0.09    gamma0.1 
    -0.22135944 -0.25298221  0.00000000  0.03162278 

---

     gamma0.01  gamma0.02  gamma0.03  gamma0.04  gamma0.05  gamma0.06  gamma0.07 
    -0.1897367 -0.2213594 -0.2846050 -0.4427189 -0.5692100 -0.6640783 -0.8221922 
     gamma0.08  gamma0.09   gamma0.1 
    -0.8854377 -0.2213594 -0.2529822 

# sumtest() works correctly

      iter_test         t      type       pval alpha reject
    1         0 -2.183462 two-sided 0.02900181  0.05   TRUE

---

      iter_test         t      type      pval alpha reject
    1         1 -1.899785 one-sided 0.9712693  0.05  FALSE

---

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  4 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  32     Outliers proportion:  0.032 
    

---

        iter_test         t      type       pval alpha reject
    1 convergence -1.705318 two-sided 0.08813521  0.05  FALSE

---

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  20     Outliers proportion:  0.02 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  29     Outliers proportion:  0.029 
    

---

      iter_test         t      type       pval alpha reject
    1         0 -2.255126 two-sided 0.02412544  0.05   TRUE

# suptest() works correctly

      iter_test test_value    pval alpha reject
    1         0   0.284605 0.08934  0.05  FALSE

---

      iter_test test_value    pval alpha reject
    1         1  0.4427189 0.09846   0.1   TRUE

---

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  4 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  3 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  6 
    Final selection:  Outliers found:  32     Outliers proportion:  0.032 
    

---

        iter_test test_value    pval alpha reject
    1 convergence    0.56921 0.12024  0.05  FALSE

# globaltest() works correctly

    $gamma0.01
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  4     Outliers proportion:  0.004 
    
    $gamma0.02
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  13     Outliers proportion:  0.013 
    
    $gamma0.03
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  21     Outliers proportion:  0.021 
    
    $gamma0.04
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  26     Outliers proportion:  0.026 
    
    $gamma0.05
    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: y ~ x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6 
    Iterations:  2 
    Final selection:  Outliers found:  36     Outliers proportion:  0.036 
    

---

    $reject
    [1] FALSE
    
    $global_alpha
    [1] 0.05
    
    $tests
              iter_test iter_act gamma         t      type       pval alpha reject
    gamma0.01         0        0  0.01 -1.498485 two-sided 0.13400726  0.05  FALSE
    gamma0.02         0        0  0.02 -1.738476 two-sided 0.08212701  0.05  FALSE
    gamma0.03         0        0  0.03 -1.519491 two-sided 0.12863889  0.05  FALSE
    gamma0.04         0        0  0.04 -2.085486 two-sided 0.03702521  0.05   TRUE
    gamma0.05         0        0  0.05 -1.518281 two-sided 0.12894347  0.05  FALSE
              alpha_adj reject_adj
    gamma0.01      0.05      FALSE
    gamma0.02      0.02      FALSE
    gamma0.03      0.03      FALSE
    gamma0.04      0.01      FALSE
    gamma0.05      0.04      FALSE
    

---

    $reject
    [1] FALSE
    
    $global_alpha
    [1] 0.05
    
    $tests
              iter_test iter_act gamma num_act num_exp      type      pval alpha
    gamma0.01         0        0  0.01       6      10 two-sided 0.2656770  0.05
    gamma0.02         0        0  0.02      14      20 two-sided 0.2170493  0.05
    gamma0.03         0        0  0.03      24      30 two-sided 0.3146255  0.05
    gamma0.04         0        0  0.04      31      40 two-sided 0.1779894  0.05
    gamma0.05         0        0  0.05      43      50 two-sided 0.3576795  0.05
              reject alpha_adj reject_adj
    gamma0.01  FALSE      0.03      FALSE
    gamma0.02  FALSE      0.02      FALSE
    gamma0.03  FALSE      0.04      FALSE
    gamma0.04  FALSE      0.01      FALSE
    gamma0.05  FALSE      0.05      FALSE
    

---

    $reject
    [1] FALSE
    
    $global_alpha
    [1] 0.05
    
    $tests
                iter_test iter_act gamma         t      type       pval alpha
    gamma0.01 convergence        5  0.01 -1.612366 two-sided 0.10688228  0.05
    gamma0.02 convergence       20  0.02 -1.218902 two-sided 0.22288139  0.05
    gamma0.03 convergence        4  0.03 -1.458982 two-sided 0.14457020  0.05
    gamma0.04 convergence        5  0.04 -1.731116 two-sided 0.08343101  0.05
    gamma0.05 convergence        7  0.05 -1.651060 two-sided 0.09872642  0.05
              reject alpha_adj reject_adj
    gamma0.01  FALSE      0.03      FALSE
    gamma0.02  FALSE      0.05      FALSE
    gamma0.03  FALSE      0.04      FALSE
    gamma0.04  FALSE      0.01      FALSE
    gamma0.05  FALSE      0.02      FALSE
    

