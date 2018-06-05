## ---- message=FALSE------------------------------------------------------
library(remixsiar)
data(gulls_small)

gulls_small_in <- simmr_load(mixtures=gulls_small$mixes,
                       source_names=gulls_small$s_names,
                       source_means=gulls_small$source_means,
                       source_sds=gulls_small$source_sds)  

prior_gulls <- default_prior(gulls_small_in)  
mcmccontrol <- list(iter = 50000, burn = 1000, thin = 10, n.chain = 4)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
data(gulls_out)

## ----eval = FALSE--------------------------------------------------------
#  gulls_out <- simmr_mcmc(gulls_small_in, prior.control = prior_gulls, mcmc.control = mcmccontrol)

## ---- eval=FALSE---------------------------------------------------------
#  K <- 80
#  gulls_clone <- simmr_clone(gulls_small_in, K = K, mcmc.control = mcmccontrol, prior.control = prior_gulls)

## ------------------------------------------------------------------------
data(gulls_clone)

## ------------------------------------------------------------------------
rout <- remix_shrink(gulls_out, gulls_clone, prior_gulls, stat = mean)
rout

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 5---------------
pc <- remix_pca(gulls_out)
plot(pc)

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 4---------------
remix_pcaplot(pc, axes = c(1,2))

