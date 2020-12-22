<div align = "justify">
  
# Dissertation Projects Demo

## Part I: Bilinear spline growth models (BLSGMs) w(w/o) time-invariant covariates (TICs) in the framework of individual measurement occasions
**Description:** <br>
In this part, we developed four models in unstructured time framework:
- BLSGMs for estimating fixed knots 
- BLSGMs for estimating random knots
- BLSGMs-TICs for estimating fixed knots 
- BLSGMs-TICs for estimating random knots

**Example data:**
[Data](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/example_data.csv)

**Demo:** 

Frequentist Framework:

- [*R* package: *OpenMx*](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)

Bayesian Framework:

- [*R* package: *rjags*](https://github.com/Veronica0206/Dissertation_projects/blob/master/rjags_demo1.md)
(For OS, R version, and rjags version, see the demo)

**Source Code:** <br>
***R package: OpenMx*** <br>
- [BLSGMs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/BLSGM_fixed.R)
- [BLSGMs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/BLSGM_random.R)
- [BLSGMs-TICs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/BLSGM_TICs_fixed.R) 
- [BLSGMs-TICs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P1/BLSGM_TICs_random.R)

***MPlus 8*** <br>
- [BLSGMs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/MPlus8_P1/BLSGM_Unknown%20Fixed%20Knot.inp)
- [BLSGMs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/MPlus8_P1/BLSGM_Unknown%20Random%20Knot.inp)
- [BLSGMs-TICs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/MPlus8_P1/BLSGM_TIC_Unknown%20Fixed%20Knot.inp) 
- [BLSGMs-TICs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/MPlus8_P1/BLSGM_TIC_Unknown%20Random%20Knot.inp)

## Part II: Bilinear spline growth mixture models (BLSGMMs) in the framework of individual measurement occasions
In this part, we developed two models in unstructured time framework:
- Two-step BLSGMMs for estimating fixed knots
(1) First step: multivariate Gaussian mixture models for clustering trajectories with considering uncertainty;
(2) Second step: investigate predictors for clusters
- One-step BLSGMMs for estimating fixed knots (Mixture of experts models for clustering and estimating coefficients simultaneously)

**Example data:**
[Data](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P2/example_data.csv)

**Demo:** 

Frequentist Framework:

- [*R* package: *OpenMx*](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P2/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)

**Source Code:** <br>
***R package: OpenMx*** <br>
- [Two-step BLSGMMs for estimating fixed knots: Step 1 & Step 2](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P2/BLSGMM_fixed_2steps.R)
- [One-step BLSGMMs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/OpenMx_P2/BLSGMM_fixed_1step.R)

</div>
