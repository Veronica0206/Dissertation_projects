<div align = "justify">
  
# Dissertation Projects Demo

## Part I: Bilinear spline growth models (BLSGMs) w(w/o) time-invariant covariates (TICs) in the framework of individual measurement occasions
**Manuscript Title:** <br>
Obtaining interpretable parameters from reparameterized longitudinal models: transformation matrices between growth factors in two parameter-spaces

**Description:** <br>
In this part, we developed four models in unstructured time framework:
- BLSGMs for estimating fixed knots 
- BLSGMs for estimating random knots
- BLSGMs-TICs for estimating fixed knots 
- BLSGMs-TICs for estimating random knots

**Example data:**
- [Data](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/example_data.csv)

**Demo:** 
- [*R* package: *OpenMx*](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)

**Source Code:** <br>
***R package: OpenMx*** <br>
**The models developed in this project are now part of *R* package *NonLinearCurve* (dependency: *OpenMx*), where we provide functions capable of 'calculating' starting values from the input and generate the estimates described in the manuscript.**
- [BLSGMs for estimating fixed knots](https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGM_fixed.R)
- [BLSGMs for estimating random knots](https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGM_random.R)
- [BLSGMs-TICs for estimating fixed knots](https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGM_TIC_fixed.R)
- [BLSGMs-TICs for estimating random knots](https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGM_TIC_random.R)

***MPlus 8*** <br>
- [BLSGMs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_Unknown%20Fixed%20Knot.inp)
- [BLSGMs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_Unknown%20Random%20Knot.inp)
- [BLSGMs-TICs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_TIC_Unknown%20Fixed%20Knot.inp)
- [BLSGMs-TICs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_TIC_Unknown%20Random%20Knot.inp)

## Part II: Bilinear spline growth mixture models (BLSGMMs) in the framework of individual measurement occasions
**Manuscript Title:** <br>
Two-step growth mixture model to examine heterogeneity in nonlinear trajectories (**accepted for publication in *Journal of Behavioral Data Science***)

**Description:** <br> 
In this part, we developed two models in unstructured time framework:
- Two-step BLSGMMs for estimating fixed knots
(1) First step: multivariate Gaussian mixture models for clustering trajectories with considering uncertainty;
(2) Second step: investigate predictors for clusters
- One-step BLSGMMs for estimating fixed knots (Mixture of experts models for clustering and estimating coefficients simultaneously)

**Example data:**
- [Data](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%202/example_data.csv)

**Demo:** 
- [*R* package: *OpenMx*](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%202/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)
  
**Source Code:** <br>
***R package: OpenMx*** <br>
**The Two-step model developed in this project is now part of *R* package *NonLinearCurve* (dependency: *OpenMx*), where we provide functions capable of 'calculating' starting values from the input and generate the estimates described in the manuscript.**
(https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGMM_2steps.R)  

 
</div>
