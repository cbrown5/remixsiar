## ---- message=FALSE------------------------------------------------------
library(remixsiar)
data(gulls_small)

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 7---------------
gulls_small_in <- simmr_load(mixtures=gulls_small$mixes,
                       source_names=gulls_small$s_names,
                       source_means=gulls_small$source_means,
                       source_sds=gulls_small$source_sds)  

plot(gulls_small_in, title = '')

## ------------------------------------------------------------------------
prior_gulls <- default_prior(gulls_small_in)  
prior_gulls 

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 5---------------
gpd <- drawpriors(gulls_small_in, prior_gulls)
plot(density(gpd[,'Krill']), main = 'Krill')

## ------------------------------------------------------------------------
prior_gulls <- default_prior(gulls_small_in)  
mcmccontrol <- list(iter = 50000, burn = 1000, thin = 10, n.chain = 4)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
data(gulls_out)

## ----eval = FALSE--------------------------------------------------------
#  gulls_out <- simmr_mcmc(gulls_small_in, prior.control = prior_gulls, mcmc.control = mcmccontrol)

## ------------------------------------------------------------------------
summary(gulls_out)  

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 7---------------
xgullssmall <- plot_dists(gulls_small_in, gulls_out, prior_gulls, plotdist = T)  
xgullssmall  

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

## ----------------------------------------------------------------------------------------------------
sims_gulls <- simdata(gulls_small_in, gulls_out, fast = T)
sims_gulls

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 5, fig.height = 6.5-------------------------
remix_2DPI(sims_gulls, gulls_small_in, ylims = c(6, 18), prob = 0.05)

## ---- dev='png', dev.args=list(pointsize=8), fig.width = 4-------------------------------------------
xresid <- remix_PI(sims_gulls, gulls_small_in, gulls_out)

## ---- warning=FALSE, message=FALSE-------------------------------------------------------------------
library(BEST)
apply(xresid$resid[[1]], 2, BESTmcmc)  

