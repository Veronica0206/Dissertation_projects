## Required libraries
#####################
Sys.setenv(OMP_NUM_THREADS = parallel::detectCores() - 1)
library(OpenMx)

## Set the number of repeated measures
#######################################
p <- 10

## Original parameter setting
##############################
c1_mean0 <- c(98.0, -5.0, -2.6, 3.5)
c2_mean0 <- c(102.0, -5.0, -2.6, 5.5)

c1_psi0 <- c2_psi0 <- matrix(c(25, 1.5, 1.5,
                               1.5, 1.0, 0.3,
                               1.5, 0.3, 1.0), nrow = 3)
sd1 <- sd2 <- 1
sd <- c(sd1, sd2)

c1_true <- c(c1_mean0, c1_psi0[row(c1_psi0) >= col(c1_psi0)], sd1)
c2_true <- c(c2_mean0, c2_psi0[row(c2_psi0) >= col(c2_psi0)], sd2)

## Transformed matrices obtained by multivariate Delta method
##############################################################
### For mean vector
####################
c1_func0 <- matrix(c(1, c1_mean0[4], 0,
                     0, 0.5, 0.5,
                     0, -0.5, 0.5), nrow = 3, byrow = T)
c2_func0 <- matrix(c(1, c2_mean0[4], 0,
                     0, 0.5, 0.5,
                     0, -0.5, 0.5), nrow = 3, byrow = T)

### For var-cov matrix
#######################
c1_grad0 <- matrix(c(1, c1_mean0[4], 0,
                     0, 0.5, 0.5,
                     0, -0.5, 0.5), nrow = 3, byrow = T)
c2_grad0 <- matrix(c(1, c2_mean0[4], 0,
                     0, 0.5, 0.5,
                     0, -0.5, 0.5), nrow = 3, byrow = T)

c1_mean0.s <- c1_func0 %*% c1_mean0[1:3]
c1_psi0.s <- c1_grad0 %*% c1_psi0 %*% t(c1_grad0)
c1_true.s <- c(c1_mean0.s[1:3], c1_mean0[4], c1_psi0.s[row(c1_psi0.s) >= col(c1_psi0.s)])
c1_init <- c(c1_true.s, sd1) * runif(length(c1_true.s) + 1, 0.9, 1.1)

c2_mean0.s <- c2_func0 %*% c2_mean0[1:3]
c2_psi0.s <- c2_grad0 %*% c2_psi0 %*% t(c2_grad0)
c2_true.s <- c(c2_mean0.s[1:3], c2_mean0[4], c2_psi0.s[row(c2_psi0.s) >= col(c2_psi0.s)])
c2_init <- c(c2_true.s, sd2) * runif(length(c2_true.s) + 1, 0.9, 1.1)

init <- list(c1_init, c2_init)

Beta <- matrix(c(0, 0, 0, 0, log(1.5), log(1.7)), byrow = T, nrow = 2)
beta <- Beta * runif(dim(Beta)[2], 0.9, 1.1)
## Main function to estimate fixed knots in mixture models 
###########################################################
### Step 1:
############
#### Define manifested variables
manifests <- paste0("Y", 1:p)
#### Define latent variables
latents <- c("eta0s", "eta1s", "eta2s")
outDef <- list(); outLoads1 <- list(); outLoads2 <- list()
class.list <- list()
for (k in 1:c){
  for(i in 1:p){
    outDef[[i]] <- mxMatrix("Full", 1, 1, free = F, labels = paste0("data.T", i), 
                            name = paste0("t", i))
    outLoads1[[i]] <- mxAlgebraFromString(paste0("t", i, " -c", k, "mug"), name = paste0("c", k, "L1", i))
    outLoads2[[i]] <- mxAlgebraFromString(paste0("abs(t", i, " -c", k, "mug)"), 
                                          name = paste0("c", k, "L2", i))
  }
  ### Create a mxModel object
  class.list[[k]] <- mxModel(name = paste0("Class", k), type = "RAM", 
                             manifestVars = manifests, latentVars = latents,
                             #### Define factor loadings from latent variables to manifests
                             mxPath(from = "eta0s", to = manifests, arrows = 1, free = F, values = 1),
                             mxPath(from = "eta1s", to = manifests, arrows = 1, free = F, values = 0,
                                    labels = paste0("c", k, "L1", 1:p, "[1,1]")),
                             mxPath(from = "eta2s", to = manifests, arrows = 1, free = F, values = 0, 
                                    labels = paste0("c", k, "L2", 1:p, "[1,1]")),
                             #### Define the variances of residuals
                             mxPath(from = manifests, to = manifests, arrows = 2, free = T, values = init[[k]][11],
                                    labels = paste0("c", k, "residuals")),
                             #### Define means of latent variables
                             mxPath(from = "one", to = latents[1:3], arrows = 1, free = T, values = init[[k]][1:3],
                                    labels = paste0("c", k, c("mueta0s", "mueta1s", "mueta2s"))),
                             #### Define var-cov matrix of latent variables
                             mxPath(from = latents, to = latents, arrows = 2,
                                    connect = "unique.pairs", free = T,
                                    values = init[[k]][c(5:10)],
                                    labels = paste0("c", k, c("psi0s0s", "psi0s1s", "psi0s2s", 
                                                              "psi1s1s", "psi1s2s", "psi2s2s"))),
                             #### Add additional parameter and constraints
                             mxMatrix("Full", 1, 1, free = T, values = init[[k]][4], 
                                      labels = paste0("c", k, "muknot"), name = paste0("c", k, "mug")),
                             outDef, outLoads1, outLoads2, 
                             mxAlgebraFromString(paste0("rbind(c", k, "mueta0s, c", k, "mueta1s, c", k, "mueta2s)"), 
                                                 name = paste0("c", k, "mean_s")),
                             mxAlgebraFromString(paste0("rbind(cbind(c", k, "psi0s0s, c", k, "psi0s1s, c", k, "psi0s2s),",
                                                        "cbind(c", k, "psi0s1s, c", k, "psi1s1s, c", k, "psi1s2s),",
                                                        "cbind(c", k, "psi0s2s, c", k, "psi1s2s, c", k, "psi2s2s))"), 
                                                 name = paste0("c", k, "psi_s")),
                             mxAlgebraFromString(paste0("rbind(cbind(", "1,", "-c", k, "muknot,", "c", k, "muknot),",
                                                        "cbind(0, 1, -1), cbind(0, 1, 1))"),
                                                 name = paste0("c", k, "func")),
                             mxAlgebraFromString(paste0("rbind(cbind(", "1,", "-c", k, "muknot,", "c", k, "muknot),",
                                                        "cbind(0, 1, -1), cbind(0, 1, 1))"),
                                                 name = paste0("c", k, "grad")),
                             mxAlgebraFromString(paste0("c", k, "func %*% c", k, "mean_s"), name = paste0("c", k, "mean")),
                             mxAlgebraFromString(paste0("c", k, "grad %*% c", k, "psi_s %*% t(c", k, "grad)"), 
                                                 name = paste0("c", k, "psi")),
                             mxFitFunctionML(vector = T))
}
### Make the class proportion matrix, fixing one parameter at a non-zero constant (one)
classBeta <- mxMatrix(type = "Full", nrow = c, ncol = dim(beta)[2],
                      free = rep(c(F, rep(T, c - 1)), 3), values = beta,
                      labels = paste0("beta", rep(1:c), rep(0:2, each = c)), 
                      name = "classbeta")
classPV <- mxMatrix(nrow = 3, ncol = 1, labels = c(NA, "data.x1", "data.x2"), 
                    values = 1, name = "weightsV")
classP <- mxAlgebra(classbeta %*% weightsV, name = "weights")
algebraObjective <- mxExpectationMixture(paste0("Class", 1:c), 
                                         weights = "weights", scale = "softmax")
objective <- mxFitFunctionML()
GMM_mx <- mxModel("Growth Mixture Model, random knots", 
                  mxData(observed = dat, type = "raw"), 
                  class.list, classBeta, classPV, classP, algebraObjective, objective)
Step_model <- mxTryHard(GMM_mx, extraTries = 9, loc = 1, scale = 0.05, 
                        initialGradientIterations = 20, OKstatuscodes = 0)

### Extract estimates of the mean vector and the var-cov matrix of growth factor in the original setting 
#########################################################################################################
paraFixed <- c("mueta0", "mueta1", "mueta2", "mug",
               paste0("psi", c("00", "01", "02", "11", "12", "22")),
               "residuals")

model.para <- summary(Step_model)$parameters[, c(1, 5, 6)]
### Estimates of parameters in each latent class
model.est <- model.se <- est <- list()
for (k in 1:c){
  model.est[[k]] <- c(mxEvalByName(paste0("c", k, "mean"), model = Step_model@submodels[[k]]),
                      model.para[model.para$name == paste0("c", k, "muknot"), 2],
                      mxEvalByName(paste0("c", k, "psi"), model = Step_model@submodels[[k]])[
                        row(mxEvalByName(paste0("c", k, "psi"), model = Step_model@submodels[[k]])) >= 
                          col(mxEvalByName(paste0("c", k, "psi"), model = Step_model@submodels[[k]]))],
                      model.para[model.para$name == paste0("c", k, "residuals"), 2])
  model.se[[k]] <- c(mxSE(paste0("Class", k, ".c", k, "mean"), Step_model, forceName = T), 
                     model.para[model.para$name == paste0("c", k, "muknot"), 3],
                     mxSE(paste0("Class", k, ".c", k, "psi"), Step_model, forceName = T)[
                       row(mxSE(paste0("Class", k, ".c", k, "psi"), Step_model, forceName = T)) >=
                         col(mxSE(paste0("Class", k, ".c", k, "psi"), Step_model, forceName = T))],
                     model.para[model.para$name == paste0("c", k, "residuals"), 3])
  est[[k]] <- data.frame(Name = paste0("c", k, paraFixed),
                         Estimate = model.est[[k]], SE = model.se[[k]])
}
est.beta <- data.frame(Name = paste0("beta", rep(2:c, 3), rep(0:2, each = c - 1)), 
                       Estimate = c(mxEval(classbeta, Step_model)[-1, ]),
                       SE = c(mxSE(classbeta, Step_model)[-1, ]))
Step.out0 <- rbind(do.call(rbind.data.frame, est), est.beta)
Step.out0$True <- c(c1_true, c2_true, Beta[-1, ])
Step.out <- Step.out0[c(1:5, 8, 10, 12:16, 19, 21, 23:25), ]
