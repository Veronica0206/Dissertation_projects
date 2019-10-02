Estimating knots in BLSGMs w/o(w) TICs in the framework of individual measurement occasions
================
Jin Liu
2019/10/01

OS, R version and OpenMx Version
--------------------------------

``` r
OpenMx::mxVersion()
```

    ## OpenMx version: 2.14.11 [GIT v2.14.11]
    ## R version: R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin15.6.0 
    ## MacOS: 10.14.6
    ## Default optimizer: CSOLNP
    ## NPSOL-enabled?: No
    ## OpenMP-enabled?: Yes

Require package would be used
-----------------------------

``` r
library(tidyr)
library(ggplot2)
```

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

![](OpenMx_demo_files/figure-markdown_github/unnamed-chunk-5-1.png)

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
    ## 1     mueta0 100.1293783 0.23173943 100.0
    ## 2     mueta1  -5.0073868 0.04598809  -5.0
    ## 3     mueta2  -1.7595540 0.04740966  -1.8
    ## 4        mug   4.4901431 0.01715136   4.5
    ## 5      psi00  26.2245070 1.69502327  25.0
    ## 6      psi01   1.2195451 0.23868732   1.5
    ## 7      psi02   0.8745718 0.24503571   1.5
    ## 8      psi11   0.9565017 0.06475191   1.0
    ## 9      psi12   0.2561526 0.04816344   0.3
    ## 10     psi22   1.0235974 0.06897952   1.0
    ## 11 residuals   1.0568450 0.02526333   1.0

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
    ## 1     mueta0 100.13152897 0.22990760 100.00
    ## 2     mueta1  -5.00905555 0.04678703  -5.00
    ## 3     mueta2  -1.76048638 0.04849598  -1.80
    ## 4        mug   4.48745940 0.02127768   4.50
    ## 5      psi00  25.83579780 1.67151606  25.00
    ## 6      psi01   1.36658773 0.24615669   1.50
    ## 7      psi02   1.02783858 0.25347677   1.50
    ## 8      psi0g   0.34334974 0.11013175   0.45
    ## 9      psi11   0.99605304 0.06932231   1.00
    ## 10     psi12   0.29650403 0.05243111   0.30
    ## 11     psi1g   0.05323383 0.02354332   0.09
    ## 12     psi22   1.07807632 0.07440090   1.00
    ## 13     psi2g   0.06970013 0.02460213   0.09
    ## 14     psigg   0.07265732 0.01506838   0.09
    ## 15 residuals   0.98992352 0.02555630   1.00

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
    ## 1     mueta0 100.05415636 0.22034631 100.0000000
    ## 2     mueta1  -5.01969026 0.04460044  -5.0000000
    ## 3     mueta2  -1.77336052 0.04566130  -1.8000000
    ## 4        mug   4.49042225 0.01715197   4.5000000
    ## 5      psi00  23.58521387 1.52823555  25.0000000
    ## 6      psi01   0.80705057 0.21700649   1.5000000
    ## 7      psi02   0.40611080 0.22125588   1.5000000
    ## 8      psi11   0.89099851 0.06067465   1.0000000
    ## 9      psi12   0.18204323 0.04430549   0.3000000
    ## 10     psi22   0.93977156 0.06366439   1.0000000
    ## 11    beta10   0.64072123 0.23726138   0.8849477
    ## 12    beta11   0.13140155 0.04719773   0.1769895
    ## 13    beta12   0.14120608 0.04838126   0.1769895
    ## 14    beta20   1.47445653 0.24094476   1.3274219
    ## 15    beta21   0.21131499 0.04794201   0.2654843
    ## 16    beta22   0.24500168 0.04915125   0.2654843
    ## 17      mux1   0.04213279 0.04246787   0.0000000
    ## 18      mux2   0.03262544 0.04180753   0.0000000
    ## 19     phi11   0.90180663 0.05703095   1.0000000
    ## 20     phi12   0.19521275 0.04065077   0.3000000
    ## 21     phi22   0.87396188 0.05527713   1.0000000
    ## 22 residuals   1.05683768 0.02526277   1.0000000

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
    ## 1     mueta0 100.05945615 0.21919697 100.00000000
    ## 2     mueta1  -5.02407114 0.04482675  -5.00000000
    ## 3     mueta2  -1.77713926 0.04614334  -1.80000000
    ## 4        mug   4.48208187 0.02081620   4.50000000
    ## 5      psi00  23.36739614 1.51551825  25.00000000
    ## 6      psi01   0.89791630 0.22144354   1.50000000
    ## 7      psi02   0.50146726 0.22667398   1.50000000
    ## 8      psi0g   0.19533462 0.10193103   0.45000000
    ## 9      psi11   0.90359253 0.06350732   1.00000000
    ## 10     psi12   0.19354018 0.04694024   0.30000000
    ## 11     psi1g   0.02158816 0.02185669   0.09000000
    ## 12     psi22   0.96392869 0.06715814   1.00000000
    ## 13     psi2g   0.03767487 0.02262305   0.09000000
    ## 14     psigg   0.06219025 0.01440717   0.09000000
    ## 15    beta10   0.59682393 0.23688006   0.88494767
    ## 16    beta11   0.16863211 0.04840472   0.17698953
    ## 17    beta12   0.17883656 0.04974327   0.17698953
    ## 18    beta1r   0.07641176 0.02246462   0.05309687
    ## 19    beta20   1.43908408 0.24028491   1.32742186
    ## 20    beta21   0.24121461 0.04912540   0.26548430
    ## 21    beta22   0.27576809 0.05053743   0.26548430
    ## 22    beta2r   0.06190592 0.02283459   0.07964533
    ## 23      mux1   0.04213212 0.04247055   0.00000000
    ## 24      mux2   0.03262513 0.04181105   0.00000000
    ## 25     phi11   0.90180671 0.05704066   1.00000000
    ## 26     phi12   0.19521147 0.04065322   0.30000000
    ## 27     phi22   0.87396069 0.05527870   1.00000000
    ## 28 residuals   0.98998310 0.02555936   1.00000000
