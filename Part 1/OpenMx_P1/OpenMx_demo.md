Estimating knots in BLSGMs w/o(w) TICs in the framework of individual
measurement occasions
================
Jin Liu
2021/11/21

## Require package would be used

``` r
library(OpenMx)
```

    ## OpenMx may run faster if it is compiled to take advantage of multiple cores.

``` r
library(tidyr)
library(ggplot2)
```

## OS, R version and OpenMx Version

``` r
mxOption(model = NULL, key = "Default optimizer", "CSOLNP", reset = FALSE)
mxVersion()
```

    ## OpenMx version: 2.19.6 [GIT v2.19.6]
    ## R version: R version 4.1.0 (2021-05-18)
    ## Platform: x86_64-apple-darwin17.0 
    ## MacOS: 12.0.1
    ## Default optimizer: CSOLNP
    ## NPSOL-enabled?: No
    ## OpenMP-enabled?: No

## “True” values of parameters

``` r
### Population values of growth factor means
# meanY0 <- c(100, -5, -1.8, 4.5)
### Population values of growth factor var-cov matrix
# psiY0 <- matrix(c(25, 1.5, 1.5, 0.45,
#                   1.5, 1.0, 0.3, 0.09,
#                   1.5, 0.3, 1.0, 0.09,
#                   0.45, 0.09, 0.09, 0.09), nrow = 4)
### Population values of TIC means
# meanX0 <- c(0, 0)
### Population values of growth factor var-cov matrix
# phi0 <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
### Population values path coefficients
# betaXtoY <- matrix(c(0.88494767, 1.32742186, 
#                      0.17698953, 0.26548430, 
#                      0.17698953, 0.26548430, 
#                      0.05309687, 0.07964533), byrow = T, nrow = 4)
```

## Define Parameter lists

``` r
### Bilinear spline with a fixed knot
paraFixed <- c("mueta0", "mueta1", "mueta2", "mug",
               paste0("psi", c("00", "01", "02", "11", "12", "22")),
               "residuals")
### Bilinear spline with a random knot
paraRandom <- c("mueta0", "mueta1", "mueta2", "mug",
                paste0("psi", c("00", "01", "02", "0g", "11", "12", "1g", "22", "2g", "gg")),
                "residuals")
### Bilinear spline with a fixed knot and two baseline covariates
paraFixedTIC <- c("mueta0", "mueta1", "mueta2", "mug",
                  paste0("psi", c("00", "01", "02", "11", "12", "22")),
                  paste0("beta1", 0:2), paste0("beta2", 0:2),
                  paste0("mux", 1:2), paste0("phi", c("11", "12", "22")),
                  "residuals")
### Bilinear spline with a random knot and two baseline covariates
paraRandomTIC <- c("mueta0", "mueta1", "mueta2", "mug",
                   paste0("psi", c("00", "01", "02", "0g", "11", "12", "1g", "22",
                                   "2g", "gg")), paste0("beta1", c(0:2, "r")),
                   paste0("beta2", c(0:2, "r")), paste0("mux", 1:2),
                   paste0("phi", c("11", "12", "22")), "residuals")
```

## Read in dataset for analyses (wide-format data)

``` r
load("uni_dat.RData")
```

## Summarize data

``` r
summary(uni_dat)
```

    ##        id              Y1               Y2               Y3        
    ##  Min.   :  1.0   Min.   : 86.41   Min.   : 81.03   Min.   : 74.06  
    ##  1st Qu.:125.8   1st Qu.: 97.02   1st Qu.: 91.78   1st Qu.: 86.43  
    ##  Median :250.5   Median :100.28   Median : 95.37   Median : 90.39  
    ##  Mean   :250.5   Mean   :100.27   Mean   : 95.29   Mean   : 90.30  
    ##  3rd Qu.:375.2   3rd Qu.:103.70   3rd Qu.: 98.59   3rd Qu.: 94.50  
    ##  Max.   :500.0   Max.   :113.81   Max.   :108.53   Max.   :105.54  
    ##        Y4               Y5              Y6              Y7       
    ##  Min.   : 66.72   Min.   :59.31   Min.   :55.57   Min.   :53.99  
    ##  1st Qu.: 81.11   1st Qu.:76.05   1st Qu.:72.31   1st Qu.:69.87  
    ##  Median : 85.38   Median :80.59   Median :77.20   Median :75.06  
    ##  Mean   : 85.38   Mean   :80.38   Mean   :77.08   Mean   :75.11  
    ##  3rd Qu.: 89.84   3rd Qu.:84.80   3rd Qu.:81.83   3rd Qu.:80.62  
    ##  Max.   :101.61   Max.   :98.03   Max.   :97.12   Max.   :96.83  
    ##        Y8              Y9             Y10              T1          T2        
    ##  Min.   :49.43   Min.   :43.53   Min.   :37.69   Min.   :0   Min.   :0.7531  
    ##  1st Qu.:67.83   1st Qu.:65.26   1st Qu.:63.65   1st Qu.:0   1st Qu.:0.8616  
    ##  Median :73.54   Median :71.37   Median :70.08   Median :0   Median :0.9928  
    ##  Mean   :73.46   Mean   :71.53   Mean   :69.84   Mean   :0   Mean   :0.9955  
    ##  3rd Qu.:79.26   3rd Qu.:78.28   3rd Qu.:77.30   3rd Qu.:0   3rd Qu.:1.1293  
    ##  Max.   :94.67   Max.   :97.30   Max.   :98.59   Max.   :0   Max.   :1.2499  
    ##        T3              T4              T5              T6       
    ##  Min.   :1.750   Min.   :2.751   Min.   :3.751   Min.   :4.751  
    ##  1st Qu.:1.870   1st Qu.:2.882   1st Qu.:3.862   1st Qu.:4.888  
    ##  Median :1.988   Median :3.007   Median :3.998   Median :4.992  
    ##  Mean   :1.997   Mean   :3.004   Mean   :3.997   Mean   :5.003  
    ##  3rd Qu.:2.129   3rd Qu.:3.133   3rd Qu.:4.124   3rd Qu.:5.139  
    ##  Max.   :2.250   Max.   :3.250   Max.   :4.246   Max.   :5.249  
    ##        T7              T8              T9             T10          x1          
    ##  Min.   :5.750   Min.   :6.751   Min.   :7.751   Min.   :9   Min.   :-3.34488  
    ##  1st Qu.:5.882   1st Qu.:6.877   1st Qu.:7.862   1st Qu.:9   1st Qu.:-0.71199  
    ##  Median :5.998   Median :6.993   Median :7.996   Median :9   Median :-0.05942  
    ##  Mean   :6.001   Mean   :6.997   Mean   :7.994   Mean   :9   Mean   :-0.03872  
    ##  3rd Qu.:6.127   3rd Qu.:7.122   3rd Qu.:8.115   3rd Qu.:9   3rd Qu.: 0.59897  
    ##  Max.   :6.248   Max.   :7.248   Max.   :8.250   Max.   :9   Max.   : 3.79265  
    ##        x2          
    ##  Min.   :-2.77197  
    ##  1st Qu.:-0.67655  
    ##  Median :-0.07555  
    ##  Mean   :-0.07230  
    ##  3rd Qu.: 0.59526  
    ##  Max.   : 2.55387

## Visualize data

``` r
long_dat_T <- gather(uni_dat, var.T, time, T1:T10)
long_dat_Y <- gather(uni_dat, var.Y, measures, Y1:Y10)
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

![](OpenMx_demo_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Bilinear Spline Growth Model with an Unknown Fixed Knot

``` r
source("BLSGM_fixed.R")
BLSGM_F <- getBLSGM_Fixed(dat = uni_dat, T_records = 1:10, traj_var = "Y", t_var = "T", paraNames = paraFixed)
BLSGM_F[[2]]
```

    ##         Name    Estimate         SE
    ## 1     mueta0 100.2494271 0.21430725
    ## 2     mueta1  -4.9656989 0.04493228
    ## 3     mueta2  -1.8033159 0.04704869
    ## 4        mug   4.4983581 0.01740289
    ## 5      psi00  22.3545740 1.44940260
    ## 6      psi01   0.8934618 0.21367079
    ## 7      psi02   1.5624909 0.23298573
    ## 8      psi11   0.9111324 0.06163578
    ## 9      psi12   0.2278701 0.04651298
    ## 10     psi22   1.0083183 0.06794141
    ## 11 residuals   1.0290761 0.02459786

## Bilinear Spline Growth Model with an Unknown Random Knot

``` r
source("BLSGM_random.R")
BLSGM_R <- getBLSGM_Random(dat = uni_dat, T_records = 1:10, traj_var = "Y", t_var = "T", paraNames = paraRandom)
BLSGM_R[[2]]
```

    ##         Name     Estimate         SE
    ## 1     mueta0 100.25274767 0.21240569
    ## 2     mueta1  -4.96831819 0.04497018
    ## 3     mueta2  -1.80587238 0.04753055
    ## 4        mug   4.49298098 0.02187107
    ## 5      psi00  21.98510371 1.42637754
    ## 6      psi01   1.05316889 0.21724735
    ## 7      psi02   1.72605594 0.23834102
    ## 8      psi0g   0.33860725 0.10464688
    ## 9      psi11   0.91569168 0.06410831
    ## 10     psi12   0.23629136 0.04892317
    ## 11     psi1g   0.01828650 0.02297654
    ## 12     psi22   1.03376033 0.07142006
    ## 13     psi2g   0.04315645 0.02456474
    ## 14     psigg   0.08114637 0.01601601
    ## 15 residuals   0.95887901 0.02477001

## Bilinear Spline Growth Model-TICs with an Unknown Fixed Knot

``` r
source("BLSGM_TIC_fixed.R")
BLSGM_TIC_F <- getBLSGM_TIC_Fixed(dat = uni_dat, T_records = 1:10, traj_var = "Y", t_var = "T", x_var = c("x1", "x2"), 
                                  paraNames = paraFixedTIC)
BLSGM_TIC_F[[2]]
```

    ##         Name     Estimate          SE
    ## 1     mueta0 100.35799902  0.20488935
    ## 2     mueta1  -4.94278981  0.04316244
    ## 3     mueta2  -1.77363594  0.04395957
    ## 4        mug   4.49858017  0.01740299
    ## 5      psi00  20.24897567  1.31610408
    ## 6      psi01   0.48335260  0.19317348
    ## 7      psi02   1.00942485  0.20178704
    ## 8      psi11   0.82749755  0.05651686
    ## 9      psi12   0.11773985  0.04076784
    ## 10     psi22   0.86162087  0.05865252
    ## 11    beta10   0.83457946  0.20512044
    ## 12    beta11   0.11268282  0.04244966
    ## 13    beta12   0.18689096  0.04325028
    ## 14    beta20   1.05688683  0.22169032
    ## 15    beta21   0.25482390  0.04586730
    ## 16    beta22   0.30925470  0.04678761
    ## 17      mux1  -0.03872397 -0.03872397
    ## 18      mux2  -0.07229983 -0.07229983
    ## 19     phi11   1.04198070  1.04198070
    ## 20     phi12   0.21780407  0.21780407
    ## 21     phi22   0.89100161  0.89100161
    ## 22 residuals   1.02906830  0.02459788

## Bilinear Spline Growth Model-TICs with an Unknown Random Knot

``` r
source("BLSGM_TIC_random.R")
BLSGM_TIC_R <- getBLSGM_TIC_Random(dat = uni_dat, T_records = 1:10, traj_var = "Y", t_var = "T", x_var = c("x1", "x2"), 
                                   paraNames = paraRandomTIC)
BLSGM_TIC_R[[2]]
```

    ##         Name      Estimate         SE
    ## 1     mueta0  1.003587e+02 0.20341428
    ## 2     mueta1 -4.943272e+00 0.04278002
    ## 3     mueta2 -1.774156e+00 0.04393636
    ## 4        mug  4.497576e+00 0.02169406
    ## 5      psi00  1.998610e+01 1.30054712
    ## 6      psi01  6.095690e-01 0.19420777
    ## 7      psi02  1.144073e+00 0.20507587
    ## 8      psi0g  2.464561e-01 0.09838749
    ## 9      psi11  8.138977e-01 0.05750016
    ## 10     psi12  1.052355e-01 0.04206442
    ## 11     psi1g -3.960549e-04 0.02155188
    ## 12     psi22  8.634310e-01 0.06069847
    ## 13     psi2g  1.589400e-02 0.02229507
    ## 14     psigg  7.651118e-02 0.01566019
    ## 15    beta10  8.102848e-01 0.20411050
    ## 16    beta11  1.332513e-01 0.04293179
    ## 17    beta12  2.069461e-01 0.04403526
    ## 18    beta1r  4.250684e-02 0.02174132
    ## 19    beta20  1.033103e+00 0.22088826
    ## 20    beta21  2.745389e-01 0.04646616
    ## 21    beta22  3.286330e-01 0.04762450
    ## 22    beta2r  4.073141e-02 0.02351875
    ## 23      mux1 -3.872407e-02 0.04564868
    ## 24      mux2 -7.229984e-02 0.04221119
    ## 25     phi11  1.041981e+00 0.06589505
    ## 26     phi12  2.178050e-01 0.04417851
    ## 27     phi22  8.910012e-01 0.05635721
    ## 28 residuals  9.589326e-01 0.02477157
