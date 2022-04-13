# validate_robust2sls() works correctly

    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  5 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  2 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  4 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  3 
    Final selection:  Outliers found:  6     Outliers proportion:  0.1875 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  0 
    Final selection:  Outliers found:  5     Outliers proportion:  0.1562 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  0 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

# print-robust2sls() works correctly

    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  5 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  robustified 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  1 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  4 
    Final selection:  Outliers found:  1     Outliers proportion:  0.0312 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  3 
    Final selection:  Outliers found:  6     Outliers proportion:  0.1875 

---

    Outlier-Robust 2SLS Model 
    Initial estimator:  saturated 
    Reference distribution:  normal 
    Two-stage Least-Squares Model: mpg ~ cyl + disp | cyl + wt 
    Iterations:  0 
    Final selection:  Outliers found:  5     Outliers proportion:  0.1562 

---

    $cons
    $cons$call
    test1 <- outlier_detection(data = data, formula = formula,
                ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
                iterations = 5, convergence_criterion = NULL, shuffle = FALSE,
                shuffle_seed = 42, split = 0.5)
    
    $cons$verbose
    [1] FALSE
    
    $cons$formula
    mpg ~ cyl + disp | cyl + wt
    NULL
    
    $cons$data
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    
    $cons$reference
    [1] "normal"
    
    $cons$sign_level
    [1] 0.05
    
    $cons$psi
    [1] 0.95
    
    $cons$cutoff
    [1] 1.959964
    
    $cons$bias_corr
    [1] 1.317798
    
    $cons$initial
    $cons$initial$estimator
    [1] "robustified"
    
    $cons$initial$split
    NULL
    
    $cons$initial$shuffle
    NULL
    
    $cons$initial$shuffle_seed
    NULL
    
    $cons$initial$user
    NULL
    
    
    $cons$convergence
    $cons$convergence$criterion
    NULL
    
    $cons$convergence$difference
    [1] 0
    
    $cons$convergence$converged
    [1] TRUE
    
    $cons$convergence$iter
    [1] 1
    
    $cons$convergence$max_iter
    NULL
    
    
    $cons$iterations
    $cons$iterations$setting
    [1] 5
    
    $cons$iterations$actual
    [1] 5
    
    
    
    $model
    $model$m0
    
    Call:
    AER::ivreg(formula = formula, data = data, model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       29.48691      0.48088     -0.05362  
    
    
    $model$m1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    $model$m2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    $model$m3
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    $model$m4
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    $model$m5
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    
    $res
    $res$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -2.7926974          -2.7926974          -2.8192740           2.8622433 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              4.6699120          -2.2072776           0.2699120           0.8558914 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -1.0604775          -4.1851714          -5.5851714          -2.1450472 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -1.2450472          -3.3450472           2.3755585           1.7320964 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              4.9596595           5.2096060           3.0487405           6.3020800 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -3.4704496          -0.7822054          -1.8329112          -1.2663064 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              7.3147858           0.1256926           1.0402747           4.0890043 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              1.2873154          -4.8970250          -2.1937767          -3.5221900 
    
    $res$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    $res$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    $res$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    $res$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    $res$m5
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    
    $stdres
    $stdres$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.82397853         -0.82397853         -0.83181988          0.84449787 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.37784611         -0.65125184          0.07963689          0.25252866 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.31289128         -1.23482457         -1.64789114         -0.63289092 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.36734813         -0.98694798          0.70090273          0.51105080 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.46333540          1.53708150          0.89952342          1.85941327 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -1.02394767         -0.23078779         -0.54079596         -0.37362060 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15820963          0.03708529          0.30693051          1.20645069 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.37981927         -1.44485525         -0.64726845         -1.03921353 
    
    $stdres$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    $stdres$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    $stdres$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    $stdres$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    $stdres$m5
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    
    $sel
    $sel$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m5
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    
    $type
    $type$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m5
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    
    attr(,"class")
    [1] "robust2sls"

---

    $cons
    $cons$call
    test2 <- outlier_detection(data = data, formula = formula,
                ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
                iterations = 10, convergence_criterion = 3, shuffle = NULL,
                shuffle_seed = NULL, split = NULL)
    
    $cons$verbose
    [1] FALSE
    
    $cons$formula
    mpg ~ cyl + disp | cyl + wt
    NULL
    
    $cons$data
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    
    $cons$reference
    [1] "normal"
    
    $cons$sign_level
    [1] 0.05
    
    $cons$psi
    [1] 0.95
    
    $cons$cutoff
    [1] 1.959964
    
    $cons$bias_corr
    [1] 1.317798
    
    $cons$initial
    $cons$initial$estimator
    [1] "robustified"
    
    $cons$initial$split
    NULL
    
    $cons$initial$shuffle
    NULL
    
    $cons$initial$shuffle_seed
    NULL
    
    $cons$initial$user
    NULL
    
    
    $cons$convergence
    $cons$convergence$criterion
    [1] 3
    
    $cons$convergence$difference
    [1] 0.8669711
    
    $cons$convergence$converged
    [1] TRUE
    
    $cons$convergence$iter
    [1] 1
    
    $cons$convergence$max_iter
    NULL
    
    
    $cons$iterations
    $cons$iterations$setting
    [1] 10
    
    $cons$iterations$actual
    [1] 1
    
    
    
    $model
    $model$m0
    
    Call:
    AER::ivreg(formula = formula, data = data, model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       29.48691      0.48088     -0.05362  
    
    
    $model$m1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    
    $res
    $res$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -2.7926974          -2.7926974          -2.8192740           2.8622433 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              4.6699120          -2.2072776           0.2699120           0.8558914 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -1.0604775          -4.1851714          -5.5851714          -2.1450472 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -1.2450472          -3.3450472           2.3755585           1.7320964 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              4.9596595           5.2096060           3.0487405           6.3020800 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -3.4704496          -0.7822054          -1.8329112          -1.2663064 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              7.3147858           0.1256926           1.0402747           4.0890043 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              1.2873154          -4.8970250          -2.1937767          -3.5221900 
    
    $res$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    
    $stdres
    $stdres$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.82397853         -0.82397853         -0.83181988          0.84449787 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.37784611         -0.65125184          0.07963689          0.25252866 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.31289128         -1.23482457         -1.64789114         -0.63289092 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.36734813         -0.98694798          0.70090273          0.51105080 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.46333540          1.53708150          0.89952342          1.85941327 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -1.02394767         -0.23078779         -0.54079596         -0.37362060 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15820963          0.03708529          0.30693051          1.20645069 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.37981927         -1.44485525         -0.64726845         -1.03921353 
    
    $stdres$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    
    $sel
    $sel$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    
    $type
    $type$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    
    attr(,"class")
    [1] "robust2sls"

---

    $cons
    $cons$call
    test3 <- outlier_detection(data = data, formula = formula,
                ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
                iterations = "convergence", convergence_criterion = 0.5,
                shuffle = TRUE, shuffle_seed = 42, split = 0.5)
    
    $cons$verbose
    [1] FALSE
    
    $cons$formula
    mpg ~ cyl + disp | cyl + wt
    NULL
    
    $cons$data
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    
    $cons$reference
    [1] "normal"
    
    $cons$sign_level
    [1] 0.05
    
    $cons$psi
    [1] 0.95
    
    $cons$cutoff
    [1] 1.959964
    
    $cons$bias_corr
    [1] 1.317798
    
    $cons$initial
    $cons$initial$estimator
    [1] "saturated"
    
    $cons$initial$split
    [1] 0.5
    
    $cons$initial$shuffle
    [1] TRUE
    
    $cons$initial$shuffle_seed
    [1] 42
    
    $cons$initial$user
    NULL
    
    
    $cons$convergence
    $cons$convergence$criterion
    [1] 0.5
    
    $cons$convergence$difference
    [1] 0
    
    $cons$convergence$converged
    [1] TRUE
    
    $cons$convergence$iter
    [1] 3
    
    $cons$convergence$max_iter
    NULL
    
    
    $cons$iterations
    $cons$iterations$setting
    [1] "convergence"
    
    $cons$iterations$actual
    [1] 4
    
    
    
    $model
    $model$m0
    $model$m0$split1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split2, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       32.49800     -0.21073     -0.04922  
    
    
    $model$m0$split2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split1, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       28.07911      0.43294     -0.04699  
    
    
    
    $model$m1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       29.85888      0.34110     -0.05524  
    
    
    $model$m2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       28.57437      0.77066     -0.06037  
    
    
    $model$m3
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    $model$m4
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       30.38471      0.23405     -0.05194  
    
    
    
    $res
    $res$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.15766438         -2.15766438         -1.93548838          2.84778789 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.60638457         -2.00302767          1.20638457          1.48319533 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.39407170         -3.78464478         -5.00050686         -2.18159698 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             0.06222017         -2.03777983          1.03870645          0.47477352 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.54383293          6.28757538          4.14659215          5.74434208 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -4.24397080         -1.09843283         -0.64982928         -0.28579648 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             8.07510875          1.20167371          0.26587282          5.05828372 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             2.26342163         -4.39697395         -0.99748360         -4.29967450 
    
    $res$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.06684625         -2.06684625         -2.45720358          3.74680634 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.99923132         -1.37615831          1.59923132          1.28063678 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.64528720         -3.44701197         -4.84701197         -0.95209060 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.05209060         -2.15209060          3.88626286          3.22336662 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             6.41853956          5.52422477          3.35850072          6.60439049 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.08878321          0.47909450         -0.59428445          0.04681779 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             8.70888544          0.44079718          1.42226506          4.43018296 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             2.60205914         -4.19546655         -0.96000850         -3.13906599 
    
    $res$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.53864236         -2.53864236         -2.33671992          3.77790763 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.69463315         -1.51440002          1.29463315          1.59971360 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.35648686         -3.87980787         -5.27980787         -1.68877001 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.78877001         -2.88877001          4.15640457          3.43192906 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             6.52446988          5.49435238          3.31323350          6.53551789 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -2.90620711         -0.04103114         -1.18625257         -0.30909645 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             8.60955151          0.41246427          1.60586748          4.48446891 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             2.25127651         -4.74423675         -1.56737144         -2.95187145 
    
    $res$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    $res$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -2.47876650         -2.47876650         -2.91149686          3.01126397 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             5.14094865         -2.00272588          0.74094865          0.69854578 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -1.20789483         -3.88402944         -5.28402944         -1.53232242 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.63232242         -2.73232242          2.65812633          2.03485730 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             5.29607557          5.16668791          3.01087065          6.27195085 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -3.58303392         -0.24049298         -1.26764019         -0.77844221 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             7.71851211          0.08226963          0.92735390          4.01848892 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.77349688         -4.55785280         -1.62345745         -3.63628874 
    
    
    $stdres
    $stdres$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.60521567         -0.60521567         -0.54289624          0.79879237 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             2.04882530         -0.56184073          0.44086723          0.41602997 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.11053543         -1.38307958         -1.40262087         -0.61192866 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             0.02273805         -0.74469648          0.29135273          0.13317195 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             2.02596612          1.76363810          1.16310143          2.09924119 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -1.55093798         -0.30810573         -0.23747687         -0.10444290 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.95100825          0.33706435          0.09716190          1.41882703 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.82715615         -1.60685222         -0.36452541         -1.57129462 
    
    $stdres$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.65673084         -0.65673084         -0.78076508          1.19053039 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.90622801         -0.43726794          0.50814836          0.40691641 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.20503703         -1.09527212         -1.54011564         -0.30252239 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.01655155         -0.68381684          1.23484205          1.02420984 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             2.03946127          1.75529688          1.06714807          2.09851454 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.98144658          0.15223006         -0.18883113          0.01487614 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.76720808          0.14006127          0.45191815          1.40767015 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.82679226         -1.33309010         -0.30503826         -0.99742371 
    
    $stdres$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -0.7271467          -0.7271467          -0.6693098           1.0821111 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              1.6311213          -0.4337716           0.3708235           0.4582081 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -0.1021090          -1.1112985          -1.5123024          -0.4837166 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -0.2259284          -0.8274342           1.1905245           0.9830120 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              1.8688126           1.5737546           0.9490139           1.8719771 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -0.8324288          -0.0117526          -0.3397799          -0.0885349 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              2.4660453           0.1181427           0.4599708           1.2844924 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              0.6448361          -1.3588980          -0.4489443          -0.8455085 
    
    $stdres$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    $stdres$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.69153474         -0.69153474         -0.81225934          0.84009270 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             1.43423941         -0.55872730          0.20671239          0.19488269 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
            -0.33698262         -1.08357979         -1.47415657         -0.42749254 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            -0.17640747         -0.76227264          0.74157316          0.56769144 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.47751724          1.44142023          0.83998297          1.74977026 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.99960704         -0.06709355         -0.35365059         -0.21717247 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.15333686          0.02295186          0.25871636          1.12109176 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.49477621         -1.27156533         -0.45291770         -1.01446425 
    
    
    $sel
    $sel$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                  FALSE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                  FALSE                TRUE                TRUE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                  FALSE                TRUE                TRUE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE                TRUE                TRUE                TRUE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE                TRUE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    
    $type
    $type$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      0                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      0                   1                   1                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      0                   1                   1                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m4
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   1                   1                   1 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   1 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    
    attr(,"class")
    [1] "robust2sls"

---

    $cons
    $cons$call
    test4 <- outlier_detection(data = data, formula = formula,
                ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
                iterations = "convergence", convergence_criterion = 1,
                shuffle = FALSE, shuffle_seed = 42, split = 0.5)
    
    $cons$verbose
    [1] FALSE
    
    $cons$formula
    mpg ~ cyl + disp | cyl + wt
    NULL
    
    $cons$data
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    
    $cons$reference
    [1] "normal"
    
    $cons$sign_level
    [1] 0.05
    
    $cons$psi
    [1] 0.95
    
    $cons$cutoff
    [1] 1.959964
    
    $cons$bias_corr
    [1] 1.317798
    
    $cons$initial
    $cons$initial$estimator
    [1] "saturated"
    
    $cons$initial$split
    [1] 0.5
    
    $cons$initial$shuffle
    [1] FALSE
    
    $cons$initial$shuffle_seed
    NULL
    
    $cons$initial$user
    NULL
    
    
    $cons$convergence
    $cons$convergence$criterion
    [1] 1
    
    $cons$convergence$difference
    [1] 0
    
    $cons$convergence$converged
    [1] TRUE
    
    $cons$convergence$iter
    [1] 2
    
    $cons$convergence$max_iter
    NULL
    
    
    $cons$iterations
    $cons$iterations$setting
    [1] "convergence"
    
    $cons$iterations$actual
    [1] 3
    
    
    
    $model
    $model$m0
    $model$m0$split1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split2, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
      32.169777     0.002182    -0.048434  
    
    
    $model$m0$split2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split1, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       27.34029      0.07197     -0.03831  
    
    
    
    $model$m1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       28.41445     -0.29239     -0.03298  
    
    
    $model$m2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
        29.4102      -0.6228      -0.0291  
    
    
    $model$m3
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = selection,     model = TRUE, y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
        29.4102      -0.6228      -0.0291  
    
    
    
    $res
    $res$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -3.4334792          -3.4334792          -4.1476670           1.7130240 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              3.9488971          -3.1852883          -0.4511029          -0.6732826 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -2.5590414          -4.8653830          -6.2653830          -2.4292209 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -1.5292209          -3.6292209           1.0734722           0.4922677 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              3.6390089           7.7865776           5.6716566           8.9954443 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -1.5275121          -0.2344466          -1.0707448          -1.2086222 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              6.6067284           2.6980697           2.9801493           6.4148126 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              1.3296848          -2.5176136          -1.3856658          -1.5930358 
    
    $res$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.38367033         -0.38367033         -0.88329471          3.24815068 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             4.49666333         -1.14011558          0.09666333          1.99294482 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             0.19837600         -1.93303932         -3.33303932         -0.58006452 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             0.31993548         -1.78006452         -0.10982694         -0.50556013 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             3.13488456          7.75045676          5.65152347          8.99982575 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -1.78426375         -0.08840282         -0.85009154         -1.23311433 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             6.31577395          2.66035009          2.72233181          6.29129212 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             1.29986344         -2.17833681         -1.14902483         -1.85458376 
    
    $res$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
           -0.018002451        -0.018002451        -0.976593895         3.233503758 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
            4.746945308        -1.026697313         0.346945308         1.749460087 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
           -0.022212225        -1.596865235        -2.996865235        -0.003022271 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            0.896977729        -1.203022271        -0.294190453        -0.643354479 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
            3.074705479         7.770863943         5.683572936         9.049726726 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
           -1.924520169         0.324871219        -0.382486811        -0.944024713 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
            6.410825393         2.679593043         2.581299231         6.248054778 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
            1.585072289        -1.754457483        -0.669777818        -1.998332867 
    
    $res$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
           -0.018002451        -0.018002451        -0.976593895         3.233503758 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
            4.746945308        -1.026697313         0.346945308         1.749460087 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
           -0.022212225        -1.596865235        -2.996865235        -0.003022271 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            0.896977729        -1.203022271        -0.294190453        -0.643354479 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
            3.074705479         7.770863943         5.683572936         9.049726726 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
           -1.924520169         0.324871219        -0.382486811        -0.944024713 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
            6.410825393         2.679593043         2.581299231         6.248054778 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
            1.585072289        -1.754457483        -0.669777818        -1.998332867 
    
    
    $stdres
    $stdres$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -0.9557849          -0.9557849          -1.1545948           0.4768581 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              1.0992629          -0.8866954          -0.1255745          -0.1874231 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -0.7123658          -1.3543870          -1.7441079          -0.6762274 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -0.4256925          -1.0102739           0.2988247           0.1370336 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              1.8075950           3.8678056           2.8172666           4.4682826 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -0.7587569          -0.1164560          -0.5318682          -0.6003556 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              3.2817423           1.3402048           1.4803215           3.1864125 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              0.6604907          -1.2505674          -0.6882980          -0.7913043 
    
    $stdres$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
            -0.17648472         -0.17648472         -0.40630720          1.49411854 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
             2.06842253         -0.52444236          0.04446422          0.91673573 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             0.09125108         -0.88917977         -1.53316651         -0.26682418 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             0.14716729         -0.81881282         -0.05051935         -0.23255287 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
             1.44201720          3.56513668          2.59964725          4.13983457 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
            -0.82074442         -0.04066446         -0.39103405         -0.56722091 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
             2.90519618          1.22373584          1.25224684          2.89393477 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
             0.59792487         -1.00201430         -0.52854054         -0.85309096 
    
    $stdres$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
           -0.009689537        -0.009689537        -0.525636319         1.740382590 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
            2.554968725        -0.552603696         0.186737861         0.941619403 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
           -0.011955381        -0.859487622        -1.613015624        -0.001626690 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            0.482784168        -0.647507835        -0.158343389        -0.346275439 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
            1.654911912         4.182545416         3.059093829         4.870873215 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
           -1.035842742         0.174856829        -0.205867516        -0.508106469 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
            3.450526037         1.442248852         1.389343752         3.362917310 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
            0.853140254        -0.944309173        -0.360497386        -1.075571267 
    
    $stdres$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
           -0.009689537        -0.009689537        -0.525636319         1.740382590 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
            2.554968725        -0.552603696         0.186737861         0.941619403 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
           -0.011955381        -0.859487622        -1.613015624        -0.001626690 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
            0.482784168        -0.647507835        -0.158343389        -0.346275439 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
            1.654911912         4.182545416         3.059093829         4.870873215 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
           -1.035842742         0.174856829        -0.205867516        -0.508106469 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
            3.450526037         1.442248852         1.389343752         3.362917310 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
            0.853140254        -0.944309173        -0.360497386        -1.075571267 
    
    
    $sel
    $sel$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE               FALSE               FALSE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE               FALSE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                  FALSE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE               FALSE               FALSE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE               FALSE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                  FALSE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE               FALSE               FALSE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE               FALSE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    $sel$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                  FALSE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE               FALSE               FALSE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE               FALSE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    
    $type
    $type$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   0                   0                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   0 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m1
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      0                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   0                   0                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   0 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m2
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      0                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   0                   0                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   0 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    $type$m3
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      0                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   0                   0                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   0 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    
    attr(,"class")
    [1] "robust2sls"

---

    $cons
    $cons$call
    test5 <- outlier_detection(data = data, formula = formula,
                ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
                iterations = 0, convergence_criterion = NULL,
                shuffle = FALSE, shuffle_seed = 42, split = 0.5)
    
    $cons$verbose
    [1] FALSE
    
    $cons$formula
    mpg ~ cyl + disp | cyl + wt
    NULL
    
    $cons$data
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    
    $cons$reference
    [1] "normal"
    
    $cons$sign_level
    [1] 0.05
    
    $cons$psi
    [1] 0.95
    
    $cons$cutoff
    [1] 1.959964
    
    $cons$bias_corr
    [1] 1.317798
    
    $cons$initial
    $cons$initial$estimator
    [1] "saturated"
    
    $cons$initial$split
    [1] 0.5
    
    $cons$initial$shuffle
    [1] FALSE
    
    $cons$initial$shuffle_seed
    NULL
    
    $cons$initial$user
    NULL
    
    
    $cons$convergence
    $cons$convergence$criterion
    NULL
    
    $cons$convergence$difference
    NULL
    
    $cons$convergence$converged
    NULL
    
    $cons$convergence$iter
    NULL
    
    $cons$convergence$max_iter
    NULL
    
    
    $cons$iterations
    $cons$iterations$setting
    [1] 0
    
    $cons$iterations$actual
    [1] 0
    
    
    
    $model
    $model$m0
    $model$m0$split1
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split2, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
      32.169777     0.002182    -0.048434  
    
    
    $model$m0$split2
    
    Call:
    AER::ivreg(formula = formula, data = data, subset = split1, model = TRUE,     y = TRUE)
    
    Coefficients:
    (Intercept)          cyl         disp  
       27.34029      0.07197     -0.03831  
    
    
    
    
    $res
    $res$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -3.4334792          -3.4334792          -4.1476670           1.7130240 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              3.9488971          -3.1852883          -0.4511029          -0.6732826 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -2.5590414          -4.8653830          -6.2653830          -2.4292209 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -1.5292209          -3.6292209           1.0734722           0.4922677 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              3.6390089           7.7865776           5.6716566           8.9954443 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -1.5275121          -0.2344466          -1.0707448          -1.2086222 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              6.6067284           2.6980697           2.9801493           6.4148126 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              1.3296848          -2.5176136          -1.3856658          -1.5930358 
    
    
    $stdres
    $stdres$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
             -0.9557849          -0.9557849          -1.1545948           0.4768581 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
              1.0992629          -0.8866954          -0.1255745          -0.1874231 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
             -0.7123658          -1.3543870          -1.7441079          -0.6762274 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
             -0.4256925          -1.0102739           0.2988247           0.1370336 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
              1.8075950           3.8678056           2.8172666           4.4682826 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
             -0.7587569          -0.1164560          -0.5318682          -0.6003556 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
              3.2817423           1.3402048           1.4803215           3.1864125 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
              0.6604907          -1.2505674          -0.6882980          -0.7913043 
    
    
    $sel
    $sel$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                   TRUE                TRUE                TRUE                TRUE 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                   TRUE                TRUE                TRUE                TRUE 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                   TRUE                TRUE                TRUE                TRUE 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                   TRUE                TRUE                TRUE                TRUE 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                   TRUE               FALSE               FALSE               FALSE 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                   TRUE                TRUE                TRUE                TRUE 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  FALSE                TRUE                TRUE               FALSE 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                   TRUE                TRUE                TRUE                TRUE 
    
    
    $type
    $type$m0
              Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                      1                   1                   1                   1 
      Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                      1                   1                   1                   1 
               Merc 230            Merc 280           Merc 280C          Merc 450SE 
                      1                   1                   1                   1 
             Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                      1                   1                   1                   1 
      Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                      1                   0                   0                   0 
          Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                      1                   1                   1                   1 
       Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                      0                   1                   1                   0 
         Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                      1                   1                   1                   1 
    
    
    attr(,"class")
    [1] "robust2sls"

