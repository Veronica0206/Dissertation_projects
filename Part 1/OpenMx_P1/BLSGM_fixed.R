## Required libraries
#####################
Sys.setenv(OMP_NUM_THREADS = parallel::detectCores() - 1)
library(OpenMx)

## Set the number of repeated measures
#######################################
p <- 10

## Original parameter setting
##############################
mean0 <- c(100, -5, -1.8, 4.5)
psi0 <- matrix(c(25, 1.5, 1.5,
                 1.5, 1.0, 0.3,
                 1.5, 0.3, 1.0), nrow = 3)
sd <- 1
true <- c(mean0, psi0[row(psi0) >= col(psi0)], sd)

## Transformed matrices obtained by multivariate Delta method
##############################################################
### For mean vector
####################
func0 <- matrix(c(1, mean0[4], 0,
                  0, 0.5, 0.5,
                  0, -0.5, 0.5), nrow = 3, byrow = T)
### For var-cov matrix
#######################
grad0 <- matrix(c(1, mean0[4], 0,
                  0, 0.5, 0.5,
                  0, -0.5, 0.5), nrow = 3, byrow = T)

mean0.s <- func0 %*% mean0[1:3]
psi0.s <- grad0 %*% psi0 %*% t(grad0)
true.s <- c(mean0.s[1:3], mean0[4], psi0.s[row(psi0.s) >= col(psi0.s)])

init <- c(true.s, sd) * runif(length(true.s) + 1, 0.9, 1.1)

## Main function to estimate a fixed knot 
##########################################
### Define manifested variables
manifests <- paste0("Y", 1:p)
### Define latent variables
latents <- c("eta0s", "eta1s", "eta2s")
outDef <- list(); outLoads1 <- list(); outLoads2 <- list()
for(i in 1:p){
  outDef[[i]] <- mxMatrix("Full", 1, 1, free = F, labels = paste0("data.T", i), name = paste0("t", i))
  outLoads1[[i]] <- mxAlgebraFromString(paste0("t", i, " - mug"), name = paste0("L1", i))
  outLoads2[[i]] <- mxAlgebraFromString(paste0("abs(t", i, " - mug)"), name = paste0("L2", i))
}  
### Create a mxModel object
model_mx <- mxModel("Estimate a fixed knot", type = "RAM",
                    manifestVars = manifests, latentVars = latents,
                    mxData(observed = dat[, 2:(2 * p + 1)], type = "raw"),
                    #### Define factor loadings from latent variables to manifests
                    mxPath(from = "eta0s", to = manifests, arrows = 1, free = F, values = 1),
                    mxPath(from = "eta1s", to = manifests, arrows = 1, free = F, values = 0,
                           labels = paste0("L1", 1:p, "[1,1]")),
                    mxPath(from = "eta2s", to = manifests, arrows = 1, free = F, values = 0,
                           labels = paste0("L2", 1:p, "[1,1]")),
                    #### Define the variances of residuals
                    mxPath(from = manifests, to = manifests, arrows = 2, free = T, values = init[11],
                           labels = "residuals"),
                    #### Define means of latent variables
                    mxPath(from = "one", to = latents, arrows = 1, free = T, values = init[1:3],
                           labels = c("mueta0s", "mueta1s", "mueta2s")),
                    #### Define var-cov matrix of latent variables
                    mxPath(from = latents, to = latents, arrows = 2, connect = "unique.pairs", 
                           free = T, values = init[c(5:10)],
                           labels = c("psi0s0s", "psi0s1s", "psi0s2s",
                                      "psi1s1s", "psi1s2s", "psi2s2s")),
                    #### Add additional parameter and constraints
                    mxMatrix("Full", 1, 1, free = T, values = init[4], 
                             labels = "muknot", name = "mug"),
                    outDef, outLoads1, outLoads2,
                    mxAlgebra(rbind(mueta0s, mueta1s, mueta2s), name = "mean_s"),
                    mxAlgebra(rbind(cbind(psi0s0s, psi0s1s, psi0s2s),
                                    cbind(psi0s1s, psi1s1s, psi1s2s),
                                    cbind(psi0s2s, psi1s2s, psi2s2s)), name = "psi_s"),
                    mxAlgebra(rbind(cbind(1, -mug, mug),
                                    cbind(0, 1, -1),
                                    cbind(0, 1, 1)), name = "func"),
                    mxAlgebra(rbind(cbind(1, -mug, mug), 
                                    cbind(0, 1, -1),  
                                    cbind(0, 1, 1)), name = "grad"),
                    mxAlgebra(func %*% mean_s, name = "mean"),
                    mxAlgebra(grad %*% psi_s %*% t(grad), name = "psi"))
model <- mxTryHard(model_mx, extraTries = 9,
                   initialGradientIterations = 20, OKstatuscodes = 0)

paraFixed <- c("mueta0", "mueta1", "mueta2", "mug",
               paste0("psi", c("00", "01", "02", "11", "12", "22")),
               "residuals")

model.para <- summary(model)$parameters[, c(1, 5, 6)]
model.est <- c(model$mean$result, model.para[model.para$name == "muknot", 2],
               model$psi$result[row(model$psi$result) >= col(model$psi$result)], 
               model.para[1, 2])
mean.se <- mxSE(mean, model)
psi.se <- mxSE(psi, model)
model.se <- c(mean.se, model.para[model.para$name == "muknot", 3], 
              psi.se[row(psi.se) >= col(psi.se)], model.para[1, 3])
out0 <- data.frame(Name = paraFixed, Estimate = model.est, SE = model.se)
out0$true <- true
out <- out0[c(1:5, 8, 10), ]

