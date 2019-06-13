demo
================
Jin Liu
2019/06/12

<style>
body {
text-align: justify}
</style>
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
    ##        Y8              Y9             Y10              T1   
    ##  Min.   :45.82   Min.   :40.42   Min.   :38.43   Min.   :0  
    ##  1st Qu.:67.01   1st Qu.:64.77   1st Qu.:62.87   1st Qu.:0  
    ##  Median :73.03   Median :71.75   Median :69.77   Median :0  
    ##  Mean   :73.29   Mean   :71.39   Mean   :69.72   Mean   :0  
    ##  3rd Qu.:79.15   3rd Qu.:77.86   3rd Qu.:76.72   3rd Qu.:0  
    ##  Max.   :95.68   Max.   :95.62   Max.   :95.90   Max.   :0  
    ##        T2               T3              T4              T5       
    ##  Min.   :0.7504   Min.   :1.751   Min.   :2.750   Min.   :3.750  
    ##  1st Qu.:0.8691   1st Qu.:1.886   1st Qu.:2.885   1st Qu.:3.891  
    ##  Median :1.0158   Median :2.007   Median :2.998   Median :4.008  
    ##  Mean   :1.0056   Mean   :2.002   Mean   :3.002   Mean   :4.007  
    ##  3rd Qu.:1.1279   3rd Qu.:2.117   3rd Qu.:3.119   3rd Qu.:4.137  
    ##  Max.   :1.2493   Max.   :2.250   Max.   :3.250   Max.   :4.250  
    ##        T6              T7              T8              T9       
    ##  Min.   :4.751   Min.   :5.751   Min.   :6.751   Min.   :7.750  
    ##  1st Qu.:4.867   1st Qu.:5.855   1st Qu.:6.869   1st Qu.:7.891  
    ##  Median :4.990   Median :5.999   Median :6.995   Median :8.034  
    ##  Mean   :4.993   Mean   :5.990   Mean   :6.996   Mean   :8.015  
    ##  3rd Qu.:5.123   3rd Qu.:6.116   3rd Qu.:7.125   3rd Qu.:8.141  
    ##  Max.   :5.248   Max.   :6.250   Max.   :7.249   Max.   :8.250  
    ##       T10          x1                 x2          
    ##  Min.   :9   Min.   :-2.75128   Min.   :-3.42053  
    ##  1st Qu.:9   1st Qu.:-0.62326   1st Qu.:-0.59155  
    ##  Median :9   Median : 0.03191   Median : 0.05391  
    ##  Mean   :9   Mean   : 0.04213   Mean   : 0.03263  
    ##  3rd Qu.:9   3rd Qu.: 0.68883   3rd Qu.: 0.73213  
    ##  Max.   :9   Max.   : 2.61251   Max.   : 2.38615

Visualize data
--------------

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

![](demo_files/figure-markdown_github/unnamed-chunk-4-1.png)

Bilinear Spline Growth Model with an Unknown Fixed Knot
-------------------------------------------------------

``` r
rm(list = ls()[-1])
source("BLSGM_fixed.R")
```

``` r
out
```

    ##         Name    Estimate         SE  true
    ## 1     mueta0 100.1293827 0.23173897 100.0
    ## 2     mueta1  -5.0073851 0.04598774  -5.0
    ## 3     mueta2  -1.7595540 0.04740976  -1.8
    ## 4        mug   4.4901432 0.01715136   4.5
    ## 5      psi00  26.2244210 1.69502446  25.0
    ## 6      psi01   1.2195359 0.23869384   1.5
    ## 7      psi02   0.8745696 0.24504954   1.5
    ## 8      psi11   0.9565007 0.06475618   1.0
    ## 9      psi12   0.2561529 0.04816112   0.3
    ## 10     psi22   1.0235970 0.06898309   1.0
    ## 11 residuals   1.0568444 0.02526310   1.0

Bilinear Spline Growth Model with an Unknown Random Knot
--------------------------------------------------------

``` r
rm(list = ls()[-1])
source("BLSGM_random.R")
```

``` r
out
```

    ##         Name     Estimate         SE   true
    ## 1     mueta0 100.13153533 0.22990755 100.00
    ## 2     mueta1  -5.00905474 0.04678729  -5.00
    ## 3     mueta2  -1.76048618 0.04849637  -1.80
    ## 4        mug   4.48745940 0.02127799   4.50
    ## 5      psi00  25.83575348 1.67157077  25.00
    ## 6      psi01   1.36659474 0.24624254   1.50
    ## 7      psi02   1.02784667 0.25349463   1.50
    ## 8      psi0g   0.34334686 0.11013893   0.45
    ## 9      psi11   0.99605575 0.06934669   1.00
    ## 10     psi12   0.29650637 0.05243930   0.30
    ## 11     psi1g   0.05323307 0.02354645   0.09
    ## 12     psi22   1.07807755 0.07439966   1.00
    ## 13     psi2g   0.06969977 0.02460273   0.09
    ## 14     psigg   0.07265716 0.01506808   0.09
    ## 15 residuals   0.98992356 0.02555639   1.00

Bilinear Spline Growth Model-TICs with an Unknown Fixed Knot
------------------------------------------------------------

``` r
rm(list = ls()[-1])
source("BLSGM_TICs_fixed.R")
```

``` r
out
```

    ##         Name     Estimate         SE        true
    ## 1     mueta0 100.05416619 0.22034694 100.0000000
    ## 2     mueta1  -5.01969082 0.04460091  -5.0000000
    ## 3     mueta2  -1.77336066 0.04566140  -1.8000000
    ## 4        mug   4.49042259 0.01715200   4.5000000
    ## 5      psi00  23.58536856 1.52821271  25.0000000
    ## 6      psi01   0.80708207 0.21693135   1.5000000
    ## 7      psi02   0.40611997 0.22125445   1.5000000
    ## 8      psi11   0.89100406 0.06064655   1.0000000
    ## 9      psi12   0.18204495 0.04430965   0.3000000
    ## 10     psi22   0.93977197 0.06366559   1.0000000
    ## 11    beta10   0.64071711 0.23697135   0.8849477
    ## 12    beta11   0.13140164 0.04716486   0.1769895
    ## 13    beta12   0.14120636 0.04838238   0.1769895
    ## 14    beta20   1.47446101 0.24083242   1.3274219
    ## 15    beta21   0.21131583 0.04792107   0.2654843
    ## 16    beta22   0.24500200 0.04914802   0.2654843
    ## 17      mux1   0.04213338 0.04246855   0.0000000
    ## 18      mux2   0.03262543 0.04180676   0.0000000
    ## 19     phi11   0.90180628 0.05703419   1.0000000
    ## 20     phi12   0.19521274 0.04065251   0.3000000
    ## 21     phi22   0.87396097 0.05527203   1.0000000
    ## 22 residuals   1.05683774 0.02526279   1.0000000

Bilinear Spline Growth Model-TICs with an Unknown Random Knot
-------------------------------------------------------------

``` r
rm(list = ls()[-4])
source("BLSGM_TICs_random.R")
```

``` r
out
```

    ##         Name     Estimate         SE         true
    ## 1     mueta0 100.05945156 0.21919646 100.00000000
    ## 2     mueta1  -5.02406979 0.04482608  -5.00000000
    ## 3     mueta2  -1.77714033 0.04614374  -1.80000000
    ## 4        mug   4.48208201 0.02081646   4.50000000
    ## 5      psi00  23.36759700 1.51546680  25.00000000
    ## 6      psi01   0.89795856 0.22133148   1.50000000
    ## 7      psi02   0.50148031 0.22667886   1.50000000
    ## 8      psi0g   0.19533535 0.10188876   0.45000000
    ## 9      psi11   0.90360034 0.06346723   1.00000000
    ## 10     psi12   0.19354233 0.04693913   0.30000000
    ## 11     psi1g   0.02158694 0.02185262   0.09000000
    ## 12     psi22   0.96392928 0.06716451   1.00000000
    ## 13     psi2g   0.03767628 0.02262242   0.09000000
    ## 14     psigg   0.06219095 0.01440591   0.09000000
    ## 15    beta10   0.59682628 0.23583292   0.88494767
    ## 16    beta11   0.16863112 0.04824278   0.17698953
    ## 17    beta12   0.17883609 0.04973838   0.17698953
    ## 18    beta1r   0.07641230 0.02245813   0.05309687
    ## 19    beta20   1.43908856 0.23974581   1.32742186
    ## 20    beta21   0.24121565 0.04903718   0.26548430
    ## 21    beta22   0.27576916 0.05053466   0.26548430
    ## 22    beta2r   0.06190573 0.02282994   0.07964533
    ## 23      mux1   0.04213254 0.04246807   0.00000000
    ## 24      mux2   0.03262502 0.04180726   0.00000000
    ## 25     phi11   0.90180598 0.05703181   1.00000000
    ## 26     phi12   0.19521276 0.04064914   0.30000000
    ## 27     phi22   0.87395964 0.05527008   1.00000000
    ## 28 residuals   0.98998284 0.02555779   1.00000000
