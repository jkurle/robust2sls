# beta_inf produces the correct output

                        Estimate Std. Error H0Std. Error    t value  H0t value
    (Intercept)      106.8576319  60.181815    74.465326  1.7755801  1.4349985
    Education         -4.5159140   3.484161     4.311090 -1.2961265 -1.0475110
    Infant.Mortality   0.3738319   1.677304     2.075395  0.2228766  0.1801257
                     Pr(>|t|) t dist H0Pr(>|t|) t dist Pr(>|t|) normal
    (Intercept)            0.0830484         0.1586928      0.03790108
    Education              0.2020092         0.3008549      0.09746595
    Infant.Mortality       0.8247120         0.8579202      0.41181578
                     H0Pr(>|t|) normal
    (Intercept)              0.1512875
    Education                0.2948640
    Infant.Mortality         0.8570539
    attr(,"iteration")
    [1] 1
    attr(,"type of correction")
    [1] "iteration m = 1"

---

                        Estimate Std. Error H0Std. Error    t value  H0t value
    (Intercept)      106.8576319  60.181815    74.756614  1.7755801  1.4294071
    Education         -4.5159140   3.484161     4.327954 -1.2961265 -1.0434294
    Infant.Mortality   0.3738319   1.677304     2.083513  0.2228766  0.1794238
                     Pr(>|t|) t dist H0Pr(>|t|) t dist Pr(>|t|) normal
    (Intercept)            0.0830484         0.1602837      0.03790108
    Education              0.2020092         0.3027183      0.09746595
    Infant.Mortality       0.8247120         0.8584677      0.41181578
                     H0Pr(>|t|) normal
    (Intercept)              0.1528873
    Education                0.2967495
    Infant.Mortality         0.8576049
    attr(,"iteration")
    [1] 1
    attr(,"type of correction")
    [1] "iteration m = 1"

---

                       Estimate Std. Error H0Std. Error    t value  H0t value
    (Intercept)      95.7277242  36.739495    47.646113  2.6055809  2.0091403
    Education        -4.5592498   2.563448     3.324442 -1.7785616 -1.3714330
    Infant.Mortality  0.7232015   1.117688     1.449489  0.6470514  0.4989356
                     Pr(>|t|) t dist H0Pr(>|t|) t dist Pr(>|t|) normal
    (Intercept)           0.01291875        0.05147961     0.004585931
    Education             0.08310804        0.17807953     0.037655833
    Infant.Mortality      0.52138915        0.62062751     0.258799340
                     H0Pr(>|t|) normal
    (Intercept)             0.04452226
    Education               0.17024000
    Infant.Mortality        0.61782476
    attr(,"iteration")
    [1] 2
    attr(,"type of correction")
    [1] "iteration m = 2"

---

                       Estimate Std. Error H0Std. Error    t value  H0t value
    (Intercept)      95.7277242  36.739495    48.415234  2.6055809  1.9772232
    Education        -4.5592498   2.563448     3.378106 -1.7785616 -1.3496466
    Infant.Mortality  0.7232015   1.117688     1.472887  0.6470514  0.4910095
                     Pr(>|t|) t dist H0Pr(>|t|) t dist Pr(>|t|) normal
    (Intercept)           0.01291875        0.05511586     0.004585931
    Education             0.08310804        0.18491196     0.037655833
    Infant.Mortality      0.52138915        0.62617145     0.258799340
                     H0Pr(>|t|) normal
    (Intercept)             0.04801641
    Education               0.17712938
    Infant.Mortality        0.62341971
    attr(,"iteration")
    [1] 2
    attr(,"type of correction")
    [1] "fixed point"

# beta_test_avar() produces the correct output

                     (Intercept)   Education Infant.Mortality
    (Intercept)        47882.854 -2487.88923      -1198.00966
    Education          -2487.889   160.48927         47.73417
    Infant.Mortality   -1198.010    47.73417         37.19403
    attr(,"type of avar")
    [1] "iteration m = 1"

---

                     (Intercept)   Education Infant.Mortality
    (Intercept)        47882.854 -2487.88923      -1198.00966
    Education          -2487.889   160.48927         47.73417
    Infant.Mortality   -1198.010    47.73417         37.19403
    attr(,"type of avar")
    [1] "iteration m = 1"

---

                     (Intercept)   Education Infant.Mortality
    (Intercept)       25657.0672 -1487.98610       -664.16825
    Education         -1487.9861   124.90781         23.13644
    Infant.Mortality   -664.1682    23.13644         23.74552
    attr(,"type of avar")
    [1] "iteration m = 2"

---

                     (Intercept)   Education Infant.Mortality
    (Intercept)       28923.4651 -1677.42142       -748.72342
    Education         -1677.4214   140.80981         26.08194
    Infant.Mortality   -748.7234    26.08194         26.76856
    attr(,"type of avar")
    [1] "fixed point"

---

                     (Intercept)   Education Infant.Mortality
    (Intercept)       28117.8434 -1630.69925       -727.86880
    Education         -1630.6992   136.88775         25.35546
    Infant.Mortality   -727.8688    25.35546         26.02296
    attr(,"type of avar")
    [1] "iteration m = 3"

---

                     (Intercept)   Education Infant.Mortality
    (Intercept)       28923.4651 -1677.42142       -748.72342
    Education         -1677.4214   140.80981         26.08194
    Infant.Mortality   -748.7234    26.08194         26.76856
    attr(,"type of avar")
    [1] "fixed point"

# beta_t() produces the correct output

                robust m = 1     full se diff   t value  Pr(>|z|)    Pr(>z)
    (Intercept)     106.8576 99.66821 31.9184 0.2252438 0.8217896 0.4108948
                   Pr(<z)
    (Intercept) 0.5891052
    attr(,"type of avar")
    [1] "iteration m = 1"

---

                robust m = 1     full se diff   t value  Pr(>|z|)    Pr(>z)
    (Intercept)     106.8576 99.66821 31.9184 0.2252438 0.8217896 0.4108948
                   Pr(<z)
    (Intercept) 0.5891052
    attr(,"type of avar")
    [1] "iteration m = 1"

---

              robust m = 2     full  se diff    t value  Pr(>|z|)    Pr(>z)
    Education     -4.55925 -3.71929 1.630219 -0.5152434 0.6063829 0.6968085
                 Pr(<z)
    Education 0.3031915
    attr(,"type of avar")
    [1] "iteration m = 2"

---

              robust m = 2     full  se diff    t value Pr(>|z|)    Pr(>z)
    Education     -4.55925 -3.71929 1.730882 -0.4852782 0.627479 0.6862605
                 Pr(<z)
    Education 0.3137395
    attr(,"type of avar")
    [1] "fixed point"

---

                robust m = 1     full   se diff    t value  Pr(>|z|)    Pr(>z)
    (Intercept)   106.857632 99.66821 31.918398  0.2252438 0.8217896 0.4108948
    Education      -4.515914 -3.71929  1.847881 -0.4311011 0.6663948 0.6668026
                   Pr(<z)
    (Intercept) 0.5891052
    Education   0.3331974
    attr(,"type of avar")
    [1] "iteration m = 1"

---

                robust m = 1     full   se diff    t value  Pr(>|z|)    Pr(>z)
    (Intercept)   106.857632 99.66821 31.918398  0.2252438 0.8217896 0.4108948
    Education      -4.515914 -3.71929  1.847881 -0.4311011 0.6663948 0.6668026
                   Pr(<z)
    (Intercept) 0.5891052
    Education   0.3331974
    attr(,"type of avar")
    [1] "iteration m = 1"

---

                     robust m = 2       full   se diff    t value  Pr(>|z|)
    (Intercept)        95.7277242 99.6682118 23.364397 -0.1686535 0.8660692
    Education          -4.5592498 -3.7192903  1.630219 -0.5152434 0.6063829
    Infant.Mortality    0.7232015  0.5669987  0.710791  0.2197591 0.8260587
                        Pr(>z)    Pr(<z)
    (Intercept)      0.5669654 0.4330346
    Education        0.6968085 0.3031915
    Infant.Mortality 0.4130294 0.5869706
    attr(,"type of avar")
    [1] "iteration m = 2"

---

                     robust m = 2       full    se diff    t value  Pr(>|z|)
    (Intercept)        95.7277242 99.6682118 24.8071134 -0.1588451 0.8737909
    Education          -4.5592498 -3.7192903  1.7308823 -0.4852782 0.6274790
    Infant.Mortality    0.7232015  0.5669987  0.7546813  0.2069785 0.8360266
                        Pr(>z)    Pr(<z)
    (Intercept)      0.5631045 0.4368955
    Education        0.6862605 0.3137395
    Infant.Mortality 0.4180133 0.5819867
    attr(,"type of avar")
    [1] "fixed point"

# beta_hausman() produces the correct output

         Hausman test   p value
    [1,]    0.3201667 0.8520728
    attr(,"type of avar")
    [1] "iteration m = 1"
    attr(,"coefficients")
    [1] "(Intercept)" "Education"  

---

         Hausman test   p value
    [1,]    0.3201667 0.8520728
    attr(,"type of avar")
    [1] "iteration m = 1"
    attr(,"coefficients")
    [1] "(Intercept)" "Education"  

---

         Hausman test      p value
    [1,]     16.68802 0.0008192085
    attr(,"type of avar")
    [1] "iteration m = 2"
    attr(,"coefficients")
    [1] "(Intercept)"      "Education"        "Infant.Mortality"

---

         Hausman test      p value
    [1,]     16.68802 0.0008192085
    attr(,"type of avar")
    [1] "iteration m = 2"
    attr(,"coefficients")
    [1] "(Intercept)"      "Education"        "Infant.Mortality"

---

         Hausman test     p value
    [1,]      14.8034 0.001992605
    attr(,"type of avar")
    [1] "fixed point"
    attr(,"coefficients")
    [1] "(Intercept)"      "Education"        "Infant.Mortality"

