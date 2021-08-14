Estimating knots in BLSGMs w/o(w) TICs in the framework of individual
measurement occasions
================
Jin Liu
2020/12/21

## OS, R version and OpenMx Version

``` r
OpenMx::mxVersion()
```

    ## OpenMx version: 2.17.3 [GIT v2.17.3]
    ## R version: R version 3.6.3 (2020-02-29)
    ## Platform: x86_64-apple-darwin15.6.0 
    ## MacOS: 11.1
    ## Default optimizer: CSOLNP
    ## NPSOL-enabled?: No
    ## OpenMP-enabled?: Yes

## Require package would be used

``` r
library(tidyr)
library(ggplot2)
```

## Data set with positve knot variance

### Read in dataset for analyses (wide-format data)

``` r
dat <- read.csv(file = "Correctly_specified_knot_variance/example_data.csv")
```

### Summarize data

``` r
summary(dat)
```

    ##        id              Y1               Y2               Y3        
    ##  Min.   :  1.0   Min.   : 85.62   Min.   : 78.98   Min.   : 73.05  
    ##  1st Qu.:125.8   1st Qu.: 96.84   1st Qu.: 91.31   1st Qu.: 86.10  
    ##  Median :250.5   Median : 99.95   Median : 94.95   Median : 89.68  
    ##  Mean   :250.5   Mean   :100.14   Mean   : 95.10   Mean   : 90.04  
    ##  3rd Qu.:375.2   3rd Qu.:103.42   3rd Qu.: 98.75   3rd Qu.: 94.24  
    ##  Max.   :500.0   Max.   :114.93   Max.   :110.01   Max.   :104.79  
    ##        Y4               Y5              Y6              Y7       
    ##  Min.   : 66.18   Min.   :59.76   Min.   :55.48   Min.   :52.96  
    ##  1st Qu.: 80.69   1st Qu.:75.04   1st Qu.:71.45   1st Qu.:69.43  
    ##  Median : 84.89   Median :79.78   Median :76.59   Median :74.64  
    ##  Mean   : 85.13   Mean   :80.06   Mean   :76.73   Mean   :75.03  
    ##  3rd Qu.: 89.97   3rd Qu.:84.97   3rd Qu.:82.02   3rd Qu.:80.35  
    ##  Max.   :104.17   Max.   :99.16   Max.   :99.71   Max.   :97.90  
    ##        Y8              Y9             Y10              T1          T2        
    ##  Min.   :45.82   Min.   :40.42   Min.   :38.43   Min.   :0   Min.   :0.7504  
    ##  1st Qu.:67.01   1st Qu.:64.77   1st Qu.:62.87   1st Qu.:0   1st Qu.:0.8691  
    ##  Median :73.03   Median :71.75   Median :69.77   Median :0   Median :1.0158  
    ##  Mean   :73.29   Mean   :71.39   Mean   :69.72   Mean   :0   Mean   :1.0056  
    ##  3rd Qu.:79.15   3rd Qu.:77.86   3rd Qu.:76.72   3rd Qu.:0   3rd Qu.:1.1279  
    ##  Max.   :95.68   Max.   :95.62   Max.   :95.90   Max.   :0   Max.   :1.2493  
    ##        T3              T4              T5              T6       
    ##  Min.   :1.751   Min.   :2.750   Min.   :3.750   Min.   :4.751  
    ##  1st Qu.:1.886   1st Qu.:2.885   1st Qu.:3.891   1st Qu.:4.867  
    ##  Median :2.007   Median :2.998   Median :4.008   Median :4.990  
    ##  Mean   :2.002   Mean   :3.002   Mean   :4.007   Mean   :4.993  
    ##  3rd Qu.:2.117   3rd Qu.:3.119   3rd Qu.:4.137   3rd Qu.:5.123  
    ##  Max.   :2.250   Max.   :3.250   Max.   :4.250   Max.   :5.248  
    ##        T7              T8              T9             T10          x1          
    ##  Min.   :5.751   Min.   :6.751   Min.   :7.750   Min.   :9   Min.   :-2.75128  
    ##  1st Qu.:5.855   1st Qu.:6.869   1st Qu.:7.891   1st Qu.:9   1st Qu.:-0.62326  
    ##  Median :5.999   Median :6.995   Median :8.034   Median :9   Median : 0.03191  
    ##  Mean   :5.990   Mean   :6.996   Mean   :8.015   Mean   :9   Mean   : 0.04213  
    ##  3rd Qu.:6.116   3rd Qu.:7.125   3rd Qu.:8.141   3rd Qu.:9   3rd Qu.: 0.68883  
    ##  Max.   :6.250   Max.   :7.249   Max.   :8.250   Max.   :9   Max.   : 2.61251  
    ##        x2          
    ##  Min.   :-3.42053  
    ##  1st Qu.:-0.59155  
    ##  Median : 0.05391  
    ##  Mean   : 0.03263  
    ##  3rd Qu.: 0.73213  
    ##  Max.   : 2.38615

### Visualize data

``` r
long_dat_T <- gather(dat, var.T, time, T1:T10)
long_dat_Y <- gather(dat, var.Y, measures, Y1:Y10)
long_dat <- data.frame(id = long_dat_T[, 1], time = long_dat_T[, 15],
                       measures = long_dat_Y[, 15])
ggplot(aes(x = time, y = measures), data = long_dat) +
  geom_line(aes(group = id), color = "lightgrey") +
  geom_point(aes(group = id), color = "darkgrey", size = 0.5) +
  geom_smooth(aes(group = 1), size = 1.8, col = "lightblue", se = F) + 
  labs(title = "Nonlinear Pattern with Individually Varying Measurement Time",
       x ="Time", y = "Measurement") + 
  theme(plot.title = element_text(hjust = 0.5))
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](OpenMx_demo_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Bilinear Spline Growth Model with an Unknown Fixed Knot

``` r
source("Correctly_specified_knot_variance/BLSGM_fixed.R")
```

``` r
out
```

    ##      Name    Estimate         SE  true
    ## 1  mueta0 100.1293758 0.23173887 100.0
    ## 2  mueta1  -5.0073839 0.04598678  -5.0
    ## 3  mueta2  -1.7595558 0.04740944  -1.8
    ## 4     mug   4.4901424 0.01715137   4.5
    ## 5   psi00  26.2244590 1.69482944  25.0
    ## 8   psi11   0.9565006 0.06466922   1.0
    ## 10  psi22   1.0235953 0.06898575   1.0

### Bilinear Spline Growth Model with an Unknown Random Knot

``` r
source("Correctly_specified_knot_variance/BLSGM_random.R")
```

``` r
out
```

    ##      Name     Estimate         SE   true
    ## 1  mueta0 100.13152821 0.22990876 100.00
    ## 2  mueta1  -5.00905623 0.04678672  -5.00
    ## 3  mueta2  -1.76048672 0.04849634  -1.80
    ## 4     mug   4.48745957 0.02127772   4.50
    ## 5   psi00  25.83605329 1.67157651  25.00
    ## 9   psi11   0.99605887 0.06933958   1.00
    ## 12  psi22   1.07807802 0.07441031   1.00
    ## 14  psigg   0.07265756 0.01506816   0.09

### Bilinear Spline Growth Model with an Unknown Random Knot (constraint)

``` r
source("Correctly_specified_knot_variance/BLSGM_random_constraint.R")
```

``` r
out
```

    ##      Name     Estimate         SE   true
    ## 1  mueta0 100.13154241 0.22990773 100.00
    ## 2  mueta1  -5.00905242 0.04678688  -5.00
    ## 3  mueta2  -1.76048657 0.04849637  -1.80
    ## 4     mug   4.48745930 0.02127767   4.50
    ## 5   psi00  25.83581106 1.67149742  25.00
    ## 9   psi11   0.99606006 0.06930131   1.00
    ## 12  psi22   1.07808382 0.07441307   1.00
    ## 14  psigg   0.07265915 0.01506832   0.09

### Bilinear Spline Growth Model-TICs with an Unknown Fixed Knot

``` r
source("Correctly_specified_knot_variance/BLSGM_TICs_fixed.R")
```

``` r
out
```

    ##      Name    Estimate         SE        true
    ## 1  mueta0 100.0541569 0.22034641 100.0000000
    ## 2  mueta1  -5.0196912 0.04460078  -5.0000000
    ## 3  mueta2  -1.7733598 0.04566146  -1.8000000
    ## 4     mug   4.4904225 0.01715195   4.5000000
    ## 5   psi00  23.5851828 1.52826942  21.7500000
    ## 8   psi11   0.8909976 0.06067324   0.8700000
    ## 10  psi22   0.9397711 0.06366251   0.8700000
    ## 11 beta10   0.6407125 0.23782062   0.8849477
    ## 12 beta11   0.1314008 0.04729678   0.1769895
    ## 13 beta12   0.1412056 0.04838782   0.1769895
    ## 14 beta20   1.4744461 0.24121348   1.3274219
    ## 15 beta21   0.2113132 0.04799005   0.2654843
    ## 16 beta22   0.2450010 0.04915344   0.2654843

### Bilinear Spline Growth Model-TICs with an Unknown Random Knot

``` r
source("Correctly_specified_knot_variance/BLSGM_TICs_random.R")
```

``` r
out
```

    ##      Name     Estimate         SE         true
    ## 1  mueta0 100.05945443 0.21919651 100.00000000
    ## 2  mueta1  -5.02406765 0.04482645  -5.00000000
    ## 3  mueta2  -1.77713891 0.04614343  -1.80000000
    ## 4     mug   4.48208231 0.02081616   4.50000000
    ## 5   psi00  23.36745390 1.51547703  21.75000000
    ## 9   psi11   0.90359747 0.06349922   0.87000000
    ## 12  psi22   0.96392794 0.06715955   0.87000000
    ## 14  psigg   0.06219080 0.01440643   0.07830000
    ## 15 beta10   0.59682657 0.23637030   0.88494767
    ## 16 beta11   0.16863210 0.04833003   0.17698953
    ## 17 beta12   0.17883651 0.04974100   0.17698953
    ## 18 beta1r   0.07641234 0.02246033   0.05309687
    ## 19 beta20   1.43909321 0.24011431   1.32742186
    ## 20 beta21   0.24121636 0.04910219   0.26548430
    ## 21 beta22   0.27576950 0.05053695   0.26548430
    ## 22 beta2r   0.06190508 0.02283239   0.07964533

### Bilinear Spline Growth Model-TICs with an Unknown Random Knot (constraint)

``` r
source("Correctly_specified_knot_variance/BLSGM_TICs_random_constraint.R")
```

``` r
out
```

    ##      Name     Estimate         SE         true
    ## 1  mueta0 100.05946898 0.21920233 100.00000000
    ## 2  mueta1  -5.02406809 0.04483111  -5.00000000
    ## 3  mueta2  -1.77713725 0.04614556  -1.80000000
    ## 4     mug   4.48208230 0.02081849   4.50000000
    ## 5   psi00  23.36773803 1.51582739  21.75000000
    ## 9   psi11   0.90360761 0.06366756   0.87000000
    ## 12  psi22   0.96393084 0.06712591   0.87000000
    ## 14  psigg   0.06218965 0.01440830   0.07830000
    ## 15 beta10   0.59683789 0.23839910   0.88494767
    ## 16 beta11   0.16863250 0.04870209   0.17698953
    ## 17 beta12   0.17883671 0.04975750   0.17698953
    ## 18 beta1r   0.07641177 0.02246821   0.05309687
    ## 19 beta20   1.43910047 0.24087398   1.32742186
    ## 20 beta21   0.24121729 0.04925118   0.26548430
    ## 21 beta22   0.27576927 0.05055516   0.26548430
    ## 22 beta2r   0.06190559 0.02283803   0.07964533

## Data set with zero knot variance

### Read in dataset for analyses (wide-format data)

``` r
dat <- read.csv(file = "Over_specified_knot_variance/example_data.csv")
```

### Summarize data

``` r
summary(dat)
```

    ##        id              Y1               Y2               Y3        
    ##  Min.   :  1.0   Min.   : 84.07   Min.   : 79.49   Min.   : 73.38  
    ##  1st Qu.:125.8   1st Qu.: 96.50   1st Qu.: 91.13   1st Qu.: 85.51  
    ##  Median :250.5   Median : 99.87   Median : 94.94   Median : 89.82  
    ##  Mean   :250.5   Mean   :100.08   Mean   : 95.00   Mean   : 90.00  
    ##  3rd Qu.:375.2   3rd Qu.:103.96   3rd Qu.: 99.02   3rd Qu.: 94.57  
    ##  Max.   :500.0   Max.   :116.37   Max.   :110.41   Max.   :108.08  
    ##        Y4               Y5               Y6               Y7        
    ##  Min.   : 66.89   Min.   : 60.93   Min.   : 53.51   Min.   : 48.21  
    ##  1st Qu.: 80.06   1st Qu.: 74.67   1st Qu.: 70.48   1st Qu.: 68.83  
    ##  Median : 85.08   Median : 80.11   Median : 76.62   Median : 74.73  
    ##  Mean   : 85.06   Mean   : 80.11   Mean   : 76.56   Mean   : 74.70  
    ##  3rd Qu.: 90.24   3rd Qu.: 85.59   3rd Qu.: 82.57   3rd Qu.: 81.17  
    ##  Max.   :107.00   Max.   :102.17   Max.   :100.65   Max.   :102.19  
    ##        Y8               Y9              Y10               T1   
    ##  Min.   : 46.08   Min.   : 42.24   Min.   : 36.85   Min.   :0  
    ##  1st Qu.: 66.00   1st Qu.: 63.97   1st Qu.: 62.08   1st Qu.:0  
    ##  Median : 72.41   Median : 70.88   Median : 69.08   Median :0  
    ##  Mean   : 72.70   Mean   : 70.71   Mean   : 68.91   Mean   :0  
    ##  3rd Qu.: 79.35   3rd Qu.: 77.39   3rd Qu.: 76.29   3rd Qu.:0  
    ##  Max.   :102.54   Max.   :104.81   Max.   :104.24   Max.   :0  
    ##        T2               T3              T4              T5       
    ##  Min.   :0.7506   Min.   :1.751   Min.   :2.750   Min.   :3.751  
    ##  1st Qu.:0.8880   1st Qu.:1.866   1st Qu.:2.885   1st Qu.:3.862  
    ##  Median :1.0146   Median :1.985   Median :3.005   Median :3.992  
    ##  Mean   :1.0104   Mean   :1.994   Mean   :3.003   Mean   :3.995  
    ##  3rd Qu.:1.1358   3rd Qu.:2.116   3rd Qu.:3.126   3rd Qu.:4.123  
    ##  Max.   :1.2496   Max.   :2.248   Max.   :3.250   Max.   :4.249  
    ##        T6              T7              T8              T9             T10   
    ##  Min.   :4.750   Min.   :5.750   Min.   :6.752   Min.   :7.753   Min.   :9  
    ##  1st Qu.:4.876   1st Qu.:5.868   1st Qu.:6.880   1st Qu.:7.883   1st Qu.:9  
    ##  Median :5.009   Median :5.996   Median :7.002   Median :8.001   Median :9  
    ##  Mean   :5.006   Mean   :6.000   Mean   :7.001   Mean   :8.001   Mean   :9  
    ##  3rd Qu.:5.132   3rd Qu.:6.126   3rd Qu.:7.121   3rd Qu.:8.121   3rd Qu.:9  
    ##  Max.   :5.249   Max.   :6.248   Max.   :7.249   Max.   :8.249   Max.   :9  
    ##        x1                 x2          
    ##  Min.   :-2.70006   Min.   :-2.92776  
    ##  1st Qu.:-0.73904   1st Qu.:-0.63003  
    ##  Median :-0.01010   Median : 0.08960  
    ##  Mean   :-0.01079   Mean   : 0.03983  
    ##  3rd Qu.: 0.70552   3rd Qu.: 0.68301  
    ##  Max.   : 2.54820   Max.   : 2.55657

### Visualize data

``` r
long_dat_T <- gather(dat, var.T, time, T1:T10)
long_dat_Y <- gather(dat, var.Y, measures, Y1:Y10)
long_dat <- data.frame(id = long_dat_T[, 1], time = long_dat_T[, 15],
                       measures = long_dat_Y[, 15])
ggplot(aes(x = time, y = measures), data = long_dat) +
  geom_line(aes(group = id), color = "lightgrey") +
  geom_point(aes(group = id), color = "darkgrey", size = 0.5) +
  geom_smooth(aes(group = 1), size = 1.8, col = "lightblue", se = F) + 
  labs(title = "Nonlinear Pattern with Individually Varying Measurement Time",
       x ="Time", y = "Measurement") + 
  theme(plot.title = element_text(hjust = 0.5))
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](OpenMx_demo_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Bilinear Spline Growth Model with an Unknown Fixed Knot

``` r
source("Over_specified_knot_variance/BLSGM_fixed.R")
```

``` r
out
```

    ##      Name    Estimate         SE  true
    ## 1  mueta0 100.0480185 0.24357830 100.0
    ## 2  mueta1  -4.9997148 0.04979124  -5.0
    ## 3  mueta2  -1.9359487 0.04618407  -1.8
    ## 4     mug   4.4958098 0.01788532   4.5
    ## 5   psi00  29.0536403 1.87336083  25.0
    ## 8   psi11   1.1413082 0.07638797   1.0
    ## 10  psi22   0.9680092 0.06536893   1.0

### Bilinear Spline Growth Model with an Unknown Random Knot

``` r
source("Over_specified_knot_variance/BLSGM_random.R")
```

``` r
out
```

    ##      Name     Estimate         SE  true
    ## 1  mueta0 100.04408857 0.24447999 100.0
    ## 2  mueta1  -4.99642793 0.04988635  -5.0
    ## 3  mueta2  -1.93282664 0.04575792  -1.8
    ## 4     mug   4.50271163 0.01822717   4.5
    ## 5   psi00  29.25812852 1.89054869  25.0
    ## 9   psi11   1.13987208 0.07880148   1.0
    ## 12  psi22   0.94263061 0.06624157   1.0
    ## 14  psigg  -0.01752068 0.01156987   0.0

### Bilinear Spline Growth Model with an Unknown Random Knot (constraint)

``` r
source("Over_specified_knot_variance/BLSGM_random_constraint.R")
```

    ## Warning: In model 'Estimate a random knot' Optimizer returned a non-zero status
    ## code 6. The model does not satisfy the first-order optimality conditions to the
    ## required accuracy, and no improved point for the merit function could be found
    ## during the final linesearch (Mx status RED)

``` r
out
```

    ##      Name      Estimate         SE  true
    ## 1  mueta0  1.000427e+02 0.24469602 100.0
    ## 2  mueta1 -4.996057e+00 0.04997544  -5.0
    ## 3  mueta2 -1.932488e+00 0.04579116  -1.8
    ## 4     mug  4.503600e+00 0.01915661   4.5
    ## 5   psi00  2.931256e+01 1.89592186  25.0
    ## 9   psi11  1.144879e+00 0.07913741   1.0
    ## 12  psi22  9.454561e-01 0.06641090   1.0
    ## 14  psigg  1.949309e-12 0.00000000   0.0

### Bilinear Spline Growth Model-TICs with an Unknown Fixed Knot

``` r
source("Over_specified_knot_variance/BLSGM_TICs_fixed.R")
```

``` r
out
```

    ##      Name   Estimate         SE        true
    ## 1  mueta0 99.9951941 0.22645302 100.0000000
    ## 2  mueta1 -5.0057739 0.04670011  -5.0000000
    ## 3  mueta2 -1.9401057 0.04388957  -1.8000000
    ## 4     mug  4.4958588 0.01788531   4.5000000
    ## 5   psi00 24.9695427 1.61494972  21.7500000
    ## 8   psi11  0.9896484 0.06682805   0.8700000
    ## 10  psi22  0.8624600 0.05870974   0.8700000
    ## 11 beta10  0.8349253 0.22968784   0.8849477
    ## 12 beta11  0.2432972 0.04667453   0.1769895
    ## 13 beta12  0.2181988 0.04376491   0.1769895
    ## 14 beta20  1.5519275 0.24494580   1.3274219
    ## 15 beta21  0.2185224 0.04976080   0.2654843
    ## 16 beta22  0.1640152 0.04664124   0.2654843

### Bilinear Spline Growth Model-TICs with an Unknown Random Knot

``` r
source("Over_specified_knot_variance/BLSGM_TICs_random.R")
```

``` r
out
```

    ##      Name     Estimate         SE        true
    ## 1  mueta0 99.991642287 0.22757868 100.0000000
    ## 2  mueta1 -5.002777346 0.04668382  -5.0000000
    ## 3  mueta2 -1.937377804 0.04333508  -1.8000000
    ## 4     mug  4.502052703 0.01822580   4.5000000
    ## 5   psi00 25.209895546 1.63357378  21.7500000
    ## 9   psi11  0.982852523 0.06869932   0.8700000
    ## 12  psi22  0.832481428 0.05933374   0.8700000
    ## 14  psigg -0.017730737 0.01154151   0.0000000
    ## 15 beta10  0.833668544 0.23008845   0.8849477
    ## 16 beta11  0.244334575 0.04723416   0.1769895
    ## 17 beta12  0.219286173 0.04398423   0.1769895
    ## 18 beta1r  0.002380352 0.01852795   0.0000000
    ## 19 beta20  1.543069435 0.24548440   1.3274219
    ## 20 beta21  0.225950923 0.05036150   0.2654843
    ## 21 beta22  0.171500107 0.04686577   0.2654843
    ## 22 beta2r  0.016194788 0.01967234   0.0000000

### Bilinear Spline Growth Model-TICs with an Unknown Random Knot (constraint)

``` r
source("Over_specified_knot_variance/BLSGM_TICs_random_constraint.R")
```

``` r
out
```

    ##      Name      Estimate         SE        true
    ## 1  mueta0  9.999013e+01 0.22787944 100.0000000
    ## 2  mueta1 -5.002452e+00 0.04677235  -5.0000000
    ## 3  mueta2 -1.937064e+00 0.04336464  -1.8000000
    ## 4     mug  4.502921e+00 0.01917190   4.5000000
    ## 5   psi00  2.527625e+01 1.64178870  21.7500000
    ## 9   psi11  9.873893e-01 0.06928246   0.8700000
    ## 12  psi22  8.351031e-01 0.05943045   0.8700000
    ## 14  psigg  2.084042e-11 0.00000000   0.0000000
    ## 15 beta10  8.336234e-01 0.23229164   0.8849477
    ## 16 beta11  2.443712e-01 0.04766424   0.1769895
    ## 17 beta12  2.193038e-01 0.04405170   0.1769895
    ## 18 beta1r  2.451581e-03 0.01936364   0.0000000
    ## 19 beta20  1.543180e+00 0.24724786   1.3274219
    ## 20 beta21  2.259130e-01 0.05073398   0.2654843
    ## 21 beta22  1.714821e-01 0.04693680   0.2654843
    ## 22 beta2r  1.614409e-02 0.02056657   0.0000000
