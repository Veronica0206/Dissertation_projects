Estimating knots in BLSGMMs in the framework of individual measurement occasions
================
Jin Liu
2019/06/12

Require package would be used
-----------------------------

``` r
library(tidyr)
library(ggplot2)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

Read in dataset for analyses (wide-format data)
-----------------------------------------------

``` r
dat <- read.csv(file = "example_data.csv")
```

Summarize data
--------------

``` r
table(dat$class)
```

    ## 
    ##   1   2 
    ## 243 257

``` r
summary(dat[dat$class == 1, ])
```

    ##        id              Y1               Y2               Y3        
    ##  Min.   :  2.0   Min.   : 85.64   Min.   : 79.42   Min.   : 72.58  
    ##  1st Qu.:126.0   1st Qu.: 95.08   1st Qu.: 90.45   1st Qu.: 84.64  
    ##  Median :252.0   Median : 98.29   Median : 93.51   Median : 88.02  
    ##  Mean   :251.6   Mean   : 98.32   Mean   : 93.35   Mean   : 88.27  
    ##  3rd Qu.:374.5   3rd Qu.:101.69   3rd Qu.: 96.67   3rd Qu.: 91.99  
    ##  Max.   :498.0   Max.   :112.66   Max.   :108.22   Max.   :105.13  
    ##        Y4              Y5              Y6              Y7       
    ##  Min.   :65.04   Min.   :61.58   Min.   :57.77   Min.   :53.36  
    ##  1st Qu.:79.08   1st Qu.:74.71   1st Qu.:72.68   1st Qu.:69.17  
    ##  Median :83.55   Median :79.08   Median :76.76   Median :74.46  
    ##  Mean   :83.29   Mean   :79.47   Mean   :76.90   Mean   :74.46  
    ##  3rd Qu.:87.87   3rd Qu.:84.17   3rd Qu.:81.94   3rd Qu.:79.78  
    ##  Max.   :99.62   Max.   :96.94   Max.   :95.57   Max.   :97.45  
    ##        Y8              Y9             Y10               T1   
    ##  Min.   :49.75   Min.   :45.83   Min.   : 40.01   Min.   :0  
    ##  1st Qu.:65.80   1st Qu.:62.91   1st Qu.: 59.62   1st Qu.:0  
    ##  Median :71.83   Median :69.26   Median : 66.74   Median :0  
    ##  Mean   :71.72   Mean   :69.17   Mean   : 66.58   Mean   :0  
    ##  3rd Qu.:77.99   3rd Qu.:75.84   3rd Qu.: 73.88   3rd Qu.:0  
    ##  Max.   :98.45   Max.   :96.63   Max.   :100.38   Max.   :0  
    ##        T2               T3              T4              T5       
    ##  Min.   :0.7525   Min.   :1.761   Min.   :2.753   Min.   :3.752  
    ##  1st Qu.:0.8625   1st Qu.:1.902   1st Qu.:2.889   1st Qu.:3.883  
    ##  Median :0.9832   Median :2.008   Median :3.030   Median :4.002  
    ##  Mean   :0.9918   Mean   :2.009   Mean   :3.014   Mean   :4.003  
    ##  3rd Qu.:1.1239   3rd Qu.:2.116   3rd Qu.:3.130   3rd Qu.:4.125  
    ##  Max.   :1.2453   Max.   :2.249   Max.   :3.247   Max.   :4.250  
    ##        T6              T7              T8              T9       
    ##  Min.   :4.750   Min.   :5.751   Min.   :6.753   Min.   :7.751  
    ##  1st Qu.:4.864   1st Qu.:5.871   1st Qu.:6.884   1st Qu.:7.863  
    ##  Median :5.006   Median :5.993   Median :6.992   Median :7.997  
    ##  Mean   :5.000   Mean   :5.989   Mean   :6.999   Mean   :7.990  
    ##  3rd Qu.:5.141   3rd Qu.:6.111   3rd Qu.:7.112   3rd Qu.:8.109  
    ##  Max.   :5.247   Max.   :6.249   Max.   :7.249   Max.   :8.245  
    ##       T10          x1                x2              class  
    ##  Min.   :9   Min.   :-2.5795   Min.   :-2.6091   Min.   :1  
    ##  1st Qu.:9   1st Qu.:-0.9063   1st Qu.:-0.8708   1st Qu.:1  
    ##  Median :9   Median :-0.1603   Median :-0.3009   Median :1  
    ##  Mean   :9   Mean   :-0.1799   Mean   :-0.2075   Mean   :1  
    ##  3rd Qu.:9   3rd Qu.: 0.5079   3rd Qu.: 0.4898   3rd Qu.:1  
    ##  Max.   :9   Max.   : 2.6054   Max.   : 2.9942   Max.   :1

``` r
summary(dat[dat$class == 2, ])
```

    ##        id              Y1               Y2               Y3        
    ##  Min.   :  1.0   Min.   : 89.54   Min.   : 82.34   Min.   : 75.55  
    ##  1st Qu.:126.0   1st Qu.: 98.36   1st Qu.: 93.33   1st Qu.: 87.82  
    ##  Median :250.0   Median :102.03   Median : 97.22   Median : 91.74  
    ##  Mean   :249.5   Mean   :102.00   Mean   : 96.94   Mean   : 91.73  
    ##  3rd Qu.:375.0   3rd Qu.:105.45   3rd Qu.:101.27   3rd Qu.: 95.65  
    ##  Max.   :500.0   Max.   :113.92   Max.   :109.53   Max.   :107.25  
    ##        Y4               Y5               Y6              Y7       
    ##  Min.   : 67.94   Min.   : 59.97   Min.   :49.66   Min.   :45.75  
    ##  1st Qu.: 82.30   1st Qu.: 76.78   1st Qu.:70.91   1st Qu.:66.36  
    ##  Median : 86.84   Median : 81.56   Median :76.43   Median :72.35  
    ##  Mean   : 86.55   Mean   : 81.56   Mean   :76.33   Mean   :72.38  
    ##  3rd Qu.: 90.97   3rd Qu.: 86.32   3rd Qu.:81.65   3rd Qu.:78.59  
    ##  Max.   :102.05   Max.   :101.39   Max.   :98.51   Max.   :96.48  
    ##        Y8              Y9             Y10              T1   
    ##  Min.   :42.06   Min.   :41.02   Min.   :32.06   Min.   :0  
    ##  1st Qu.:63.65   1st Qu.:59.95   1st Qu.:57.54   1st Qu.:0  
    ##  Median :70.35   Median :66.89   Median :64.25   Median :0  
    ##  Mean   :69.63   Mean   :66.97   Mean   :64.30   Mean   :0  
    ##  3rd Qu.:75.74   3rd Qu.:73.27   3rd Qu.:71.03   3rd Qu.:0  
    ##  Max.   :95.77   Max.   :94.88   Max.   :94.03   Max.   :0  
    ##        T2               T3              T4              T5       
    ##  Min.   :0.7542   Min.   :1.750   Min.   :2.750   Min.   :3.752  
    ##  1st Qu.:0.8743   1st Qu.:1.878   1st Qu.:2.871   1st Qu.:3.859  
    ##  Median :0.9878   Median :2.007   Median :2.979   Median :4.002  
    ##  Mean   :1.0011   Mean   :1.999   Mean   :2.991   Mean   :3.994  
    ##  3rd Qu.:1.1486   3rd Qu.:2.107   3rd Qu.:3.119   3rd Qu.:4.128  
    ##  Max.   :1.2489   Max.   :2.248   Max.   :3.248   Max.   :4.249  
    ##        T6              T7              T8              T9       
    ##  Min.   :4.750   Min.   :5.752   Min.   :6.750   Min.   :7.754  
    ##  1st Qu.:4.866   1st Qu.:5.891   1st Qu.:6.877   1st Qu.:7.874  
    ##  Median :5.019   Median :6.009   Median :6.985   Median :7.992  
    ##  Mean   :5.002   Mean   :6.004   Mean   :6.999   Mean   :7.997  
    ##  3rd Qu.:5.130   3rd Qu.:6.123   3rd Qu.:7.134   3rd Qu.:8.115  
    ##  Max.   :5.237   Max.   :6.248   Max.   :7.246   Max.   :8.247  
    ##       T10          x1                x2              class  
    ##  Min.   :9   Min.   :-2.8460   Min.   :-2.3684   Min.   :2  
    ##  1st Qu.:9   1st Qu.:-0.2748   1st Qu.:-0.2324   1st Qu.:2  
    ##  Median :9   Median : 0.3706   Median : 0.4325   Median :2  
    ##  Mean   :9   Mean   : 0.3293   Mean   : 0.3868   Mean   :2  
    ##  3rd Qu.:9   3rd Qu.: 0.9533   3rd Qu.: 0.9860   3rd Qu.:2  
    ##  Max.   :9   Max.   : 2.6135   Max.   : 2.6051   Max.   :2

Visualize data
--------------

``` r
long_dat_T <- gather(dat, var.T, time, T1:T10)
long_dat_Y <- gather(dat, var.Y, measures, Y1:Y10)
long_dat <- data.frame(id = long_dat_T[, "id"], time = long_dat_T[, "time"],
                       measures = long_dat_Y[, "measures"], class = long_dat_Y[, "class"])
ggplot(aes(x = time, y = measures), data = long_dat) +
  geom_line(aes(group = id), color = "lightgrey", data = long_dat) +
  geom_point(aes(group = id), color = "darkgrey", size = 0.5) +
  geom_smooth(aes(group = 1), size = 1.8, col = "lightblue", se = F, 
              data = long_dat[long_dat$class == 1, ] ) + 
  geom_smooth(aes(group = 1), size = 1.8, col = "pink", se = F, 
              data = long_dat[long_dat$class == 2, ] ) + 
  labs(title = "Nonlinear Pattern with Individually Varying Measurement Time",
       x ="Time", y = "Measurement") + 
  theme(plot.title = element_text(hjust = 0.5))
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](demo_files/figure-markdown_github/unnamed-chunk-4-1.png)

Bilinear Spline Growth Mixture Model with Unknown Fixed Knots
-------------------------------------------------------------

``` r
rm(list = ls())
dat <- read.csv(file = "example_data.csv")
c <- 2
source("BLSGMM_fixed_2steps.R")
```

``` r
Step1.out
```

    ##           Name    Estimate         SE  True
    ## 1     c1mueta0  98.5298897 0.37549660  98.0
    ## 2     c1mueta1  -4.9330067 0.07414751  -5.0
    ## 3     c1mueta2  -2.6195374 0.08570465  -2.6
    ## 4        c1mug   3.5524337 0.03550190   3.5
    ## 5      c1psi00  23.9527856 2.33228892  25.0
    ## 6      c1psi01   1.4882395 0.35567211   1.5
    ## 7      c1psi02   1.2302153 0.41443397   1.5
    ## 8      c1psi11   0.9309671 0.09809742   1.0
    ## 9      c1psi12   0.2217709 0.07633837   0.3
    ## 10     c1psi22   0.9850576 0.11519429   1.0
    ## 11 c1residuals   1.0969288 0.03972610   1.0
    ## 12    c2mueta0 102.0479504 0.35694119 102.0
    ## 13    c2mueta1  -5.2071074 0.07934535  -5.0
    ## 14    c2mueta2  -2.6801852 0.09140002  -2.6
    ## 15       c2mug   5.5448983 0.03359501   5.5
    ## 16     c2psi00  22.6433117 2.31813348  25.0
    ## 17     c2psi01   1.3480190 0.34844757   1.5
    ## 18     c2psi02   1.7894528 0.39085234   1.5
    ## 19     c2psi11   0.8127126 0.09724047   1.0
    ## 20     c2psi12   0.1976068 0.08075218   0.3
    ## 21     c2psi22   1.0203151 0.11980641   1.0
    ## 22 c2residuals   1.0237298 0.03880763   1.0
    ## 23          p2   0.9112863 0.13373008   1.0

``` r
Step2.out
```

    ##     Name   Estimate        SE      True
    ## 1 beta20 -0.1605988 0.1113516 0.0000000
    ## 2 beta21  0.3101442 0.1185165 0.4054651
    ## 3 beta22  0.5694390 0.1272626 0.5306283

``` r
rm(list = ls())
dat <- read.csv(file = "example_data.csv")
c <- 2
source("BLSGMM_fixed_1step.R")
```

``` r
Step.out
```

    ##           Name    Estimate         SE        True
    ## 1     c1mueta0  98.4543061 0.35512844  98.0000000
    ## 2     c1mueta1  -4.9878731 0.07651651  -5.0000000
    ## 3     c1mueta2  -2.6193415 0.07922923  -2.6000000
    ## 4        c1mug   3.5473262 0.03527097   3.5000000
    ## 5      c1psi00  23.5125000 2.28918224  25.0000000
    ## 6      c1psi01   1.3779712 0.35681337   1.5000000
    ## 7      c1psi02   1.2731682 0.38392352   1.5000000
    ## 8      c1psi11   0.9245455 0.10100635   1.0000000
    ## 9      c1psi12   0.2544574 0.07972720   0.3000000
    ## 10     c1psi22   1.0159983 0.11242763   1.0000000
    ## 11 c1residuals   1.0924074 0.03954656   1.0000000
    ## 12    c2mueta0 101.9995182 0.35312035 102.0000000
    ## 13    c2mueta1  -5.1380033 0.07688425  -5.0000000
    ## 14    c2mueta2  -2.6662633 0.08109164  -2.6000000
    ## 15       c2mug   5.5481050 0.03313440   5.5000000
    ## 16     c2psi00  23.0050990 2.34819729  25.0000000
    ## 17     c2psi01   1.2334089 0.36273441   1.5000000
    ## 18     c2psi02   1.6789699 0.37850178   1.5000000
    ## 19     c2psi11   0.8876693 0.10187653   1.0000000
    ## 20     c2psi12   0.1991599 0.07671623   0.3000000
    ## 21     c2psi22   1.0063398 0.11236735   1.0000000
    ## 22 c2residuals   1.0201321 0.03761555   1.0000000
    ## 23      beta20  -0.1113030 0.13288006   0.0000000
    ## 24      beta21   0.3271014 0.12238885   0.4054651
    ## 25      beta22   0.6042520 0.13414994   0.5306283
