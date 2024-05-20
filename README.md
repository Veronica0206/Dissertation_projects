<div align = "justify">
  
# Dissertation Projects Demo

## Part I: Bilinear spline growth models (BLSGMs) w(w/o) time-invariant covariates (TICs) in the framework of individual measurement occasions
**Manuscript Title:** <br>
Obtaining interpretable parameters from reparameterized longitudinal models: transformation matrices between growth factors in two parameter-spaces (**accepted for publication in *Journal of Educational and Behavioral Statistics***)

**Description:** <br>
In this part, we developed four models in unstructured time framework:
- BLSGMs for estimating fixed knots (not in the manuscript)
- BLSGMs for estimating random knots (not in the manuscript)
- BLSGMs-TICs for estimating fixed knots (in the manuscript)
- BLSGMs-TICs for estimating random knots (in the manuscript)

**Example data:**
- [Data](https://github.com/Veronica0206/Dissertation_projects/tree/master/Part1/Data/BLS_dat.RData)

**Source Code:** <br>
***R package: nlpsem*** <br>
**The models developed in this project are now part of *R* package *nlpsem* (dependency: *OpenMx*), where we provide functions capable of 'calculating' starting values from the input and generate the estimates described in the manuscript.**
- [*R* package: *nlpsem*](https://github.com/Veronica0206/Dissertation_projects/tree/master/Part1/OpenMx/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)

***MPlus 8*** <br>
- [BLSGMs for estimating fixed knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_Unknown%20Fixed%20Knot.inp) (not in the manuscript, provide for the cases that the TICs that are not the primary interest)
- [BLSGMs for estimating random knots](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%201/MPlus8_P1/BLSGM_Unknown%20Random%20Knot.inp) (not in the manuscript, provide for the cases that the TICs that are not the primary interest)
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
- [Data](https://github.com/Veronica0206/NonLinearCurve/blob/main/data/BLSGM_uni_sub_dat.RData)

**Demo:** 
- [*R* package: *OpenMx*](https://github.com/Veronica0206/Dissertation_projects/blob/master/Part%202/OpenMx_P2/OpenMx_demo.md)
(For OS, R version, and OpenMx version, see the demo)
  
**Source Code:** <br>
***R package: OpenMx*** <br>
**The Two-step model developed in this project is now part of *R* package *NonLinearCurve* (dependency: *OpenMx*), where we provide functions capable of 'calculating' starting values from the input and generate the estimates described in the manuscript.**
(https://github.com/Veronica0206/NonLinearCurve/blob/main/R/BLSGMM_2steps.R)  

 
</div>
