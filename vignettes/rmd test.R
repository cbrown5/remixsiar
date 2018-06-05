
library(remixsiar)
library(purrr)
devtools::load_all("~/Code/isotope mixing models/remixsiar")


setwd("~/Code/isotope mixing models/remixsiar/data-raw")

sdat <- read.csv("gull_sources.csv")
cdat <- read.csv("gull_data.csv")
infprior <- load("sims/informative-prior.rda")


tracer_means <- c(-17, 12.75)
tracer_sds <- c(0.35, 1.1)
smeans <- cbind(cmean = c(-19.2, -15.72, -17.18), nmean = c(9.54, 11.24, 15.34))
ssds <- cbind(c(0.41, 1.02, 1.61), c(1.15, 1.64, 2.15))
snames <- c("Krill", "Mussels", "Shag_prey")

simmr_fitsims()

nvals <- c(5, 10, 20, 40, 80)
seeds <- seq(10, 60, by = 10)
mcmc.control <-  list(iter = 10000, burn = 2000, thin = 10, n.chain = 2)
# you should use more iterations and chains
prior.control <- NULL

dfin <- expand.grid(n = nvals, seed = seeds)
df <- c(as.list(dfin),
list(tracer_means = list(tracer_means), tracer_sds = list(tracer_sds),
snames = list(snames), smeans = list(smeans), ssds = list(ssds),
mcmc.control = list(mcmc.control), prior.control = list(NULL)))

#takes about 85 minutes
system.time(gulls_multirun <- pmap(df, simmr_fitsims))

rout <- map(1:length(gulls_multirun), ~calchell(gulls_multirun[[.x]]))
routkl <- map(1:length(gulls_multirun), ~calckl(gulls_multirun[[.x]]))
rdf <- do.call(rbind, rout) %>% data.frame() %>%
  setNames(paste0('hd', as.character(sdat$source))) %>%
  cbind(dfin)
rdf2 <- do.call(rbind, rout) %>% data.frame() %>%
  setNames(paste0('kl', as.character(sdat$source))) %>%
  cbind(rdf)

library(ggplot2)

ggplot(rdf, aes(x = n, y = hdKrill)) + geom_point() + 
    stat_smooth() + ylab("Hellinger distance")

ggplot(rdf2, aes(x = n, y = klMussels)) + geom_point() + 
  stat_smooth() + ylab("Kullback-Leibler divergence")




