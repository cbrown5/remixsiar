## ---- message=FALSE------------------------------------------------------
library(remixsiar)

## ------------------------------------------------------------------------
tracer_means <- c(-17, 12.75)
tracer_sds <- c(0.35, 1.1)
smeans <- cbind(cmean = c(-19.2, -15.72, -17.18), nmean = c(9.54, 11.24, 15.34))
ssds <- cbind(c(0.41, 1.02, 1.61), c(1.15, 1.64, 2.15))
snames <- c("Krill", "Mussels", "Shag_prey")

## ----message = FALSE-----------------------------------------------------
mod1 <- simmr_fitsims(n = 5, tracer_means = tracer_means,tracer_sds = tracer_sds,
      snames = snames, smeans = smeans, ssds = ssds, seed = 42)

## ----message = FALSE-----------------------------------------------------
plot(mod1$dat_in)
plot(mod1$dat_out, type = "boxplot")

## ---- eval = FALSE-------------------------------------------------------
#  calckl(mod1)
#  calchell(mod1)

## ------------------------------------------------------------------------
nvals <- c(5, 10, 20, 40, 80)
seeds <- seq(10, 60, by = 10)

## ------------------------------------------------------------------------
mcmc.control <-  list(iter = 10000, burn = 2000, thin = 10, n.chain = 2)
prior.control <- NULL

## ------------------------------------------------------------------------
dfin <- expand.grid(n = nvals, seed = seeds)
df <- c(as.list(dfin),
list(tracer_means = list(tracer_means), tracer_sds = list(tracer_sds),
snames = list(snames), smeans = list(smeans), ssds = list(ssds),
mcmc.control = list(mcmc.control), prior.control = list(NULL)))

## ----message = FALSE-----------------------------------------------------
library(purrr)
system.time(gulls_multirun <- pmap(df, simmr_fitsims))

## ------------------------------------------------------------------------
rout <- map(1:length(gulls_multirun), ~calchell(gulls_multirun[[.x]]))
routkl <- map(1:length(gulls_multirun), ~calckl(gulls_multirun[[.x]]))
rdf <- do.call(rbind, rout) %>% data.frame() %>%
  setNames(paste0('hd', snames)) %>%
  cbind(dfin)
rdf2 <- do.call(rbind, rout) %>% data.frame() %>%
  setNames(paste0('kl', snames)) %>%
  cbind(rdf)

## ------------------------------------------------------------------------
library(ggplot2)

ggplot(rdf, aes(x = n, y = hdKrill)) + geom_point() + 
    stat_smooth() + ylab("Hellinger distance")

ggplot(rdf2, aes(x = n, y = klMussels)) + geom_point() + 
  stat_smooth() + ylab("Kullback-Leibler divergence")


