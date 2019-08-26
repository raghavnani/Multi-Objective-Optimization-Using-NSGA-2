### Adaptive Sampling Script (copied) ###

data("sim.data")
library(PrevMap)
library(sf)
library(sampling)
library(geosample) 
library(viridisLite)

# NOT RUN {
#example using toy datasets
#1. sampling locations with associated prediction variance and exceedance probabilities
set.seed(1234)
xy.all <- expand.grid(x = seq(0,1, l = 10),y = seq(0,1, l = 10))
xy.all$predvar <- runif(100, min=0, max = 2.5)
xy.all$exceedprob <- runif(100, min = 0, max = 1)
obj1 <- sf::st_as_sf(xy.all, coords = c('x', 'y'))

#2. initial sample design
set.seed(1234)
xy.sample <- discrete.inhibit.sample(obj = obj1, size = 70,
                                     delta = 0.01, k = 0)
init.design <- xy.sample$sample.locs

#3. adaptive sampling designs
#a. using prediction variance criterion
adapt.design.pv <- adaptive.sample(obj1 = obj1, obj2 = init.design,
                                   pred.var.col = 1, criterion = "predvar",
                                   delta = 0.1, batch.size = 10,
                                   poly = NULL, plotit = TRUE)


#b. using exceedance probability criterion
adapt.design.ep <- adaptive.sample(obj1 = obj1, obj2 = init.design,
                                   excd.prob.col = 2, criterion = "exceedprob",
                                   delta = 0.1, batch.size = 10,
                                   poly = NULL, plotit = TRUE)



# }
# NOT RUN {

#1. Generate inhibitory design without close pairs using discrete.inhibit.sample().
set.seed(1234)
xy.sample <- discrete.inhibit.sample(obj = sim.data, size = 100, delta = 0.075,
                                     k = 0, plotit = TRUE)
names(xy.sample)
init.design <- xy.sample$sample.locs

#2. Data analysis
knots <- as.matrix(expand.grid(seq(-0.2, 1.2, length = 15),
                               seq(-0.2, 1.2, length = 15)))
lr.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 1000, thin = 6)

par0.lr <- c(0.001, 1, 0.4)
fit.MCML.lr <- binomial.logistic.MCML(y ~ 1,
                                      units.m = ~units.m, coords = ~st_coordinates(init.design),
                                      data = init.design, par0 = par0.lr, fixed.rel.nugget = 0,
                                      start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
                                      low.rank = TRUE, knots = knots, kappa = 1.5,
                                      method = "nlminb", messages = TRUE,
                                      plot.correlogram = FALSE)

summary(fit.MCML.lr, log.cov.pars = FALSE)

# Note: parameter estimation above can and should be repeated several times with updated starting
# values for the covariance function.

#3. Plug-in prediction using estimated parameters
pred.MCML.lr <- spatial.pred.binomial.MCML(object = fit.MCML.lr,
                                           control.mcmc = lr.mcmc,
                                           grid.pred = st_coordinates(sim.data),
                                           type = "joint", messages = TRUE,
                                           scale.predictions = "prevalence",
                                           standard.errors = TRUE,  thresholds = 0.45,
                                           scale.thresholds = "prevalence")


#4. Visualisation of analysis from initial sample
plot(pred.MCML.lr, type = "prevalence", summary = "predictions",
     zlim = c(0, 1), main = "Prevalence - predictions")
contour(pred.MCML.lr, "prevalence", "predictions",
        zlim = c(0, 1), levels = seq(0.1,0.9, 0.1), add = TRUE)

plot(pred.MCML.lr,  summary = "exceedance.prob",
     zlim = c(0, 1), main = "Prevalence - exceedance probability")
contour(pred.MCML.lr, summary = "exceedance.prob",
        zlim = c(0, 1), levels = seq(0.1,0.3, 0.1), add = TRUE)

plot(pred.MCML.lr, type = "prevalence",  summary = "standard.errors",
     main = "Prevalence - standard errors")

#5. Adaptive sampling
#create data frame of ingredients to adaptive sampling from spatial predictions above
obj1 <- as.data.frame(cbind(pred.MCML.lr$grid,
                            c(pred.MCML.lr$prevalence$standard.errors)^2,
                            pred.MCML.lr$exceedance.prob))
colnames(obj1) <- c("x", "y", "pred.var", "exceed.prob")
obj1 <- sf::st_as_sf(obj1, coords = c('x', 'y'))


#adaptive sampling using prediction variance criterion.
adapt.design.pv <- adaptive.sample(obj1 = obj1, obj2 = init.design,
                                   pred.var.col = 1, excd.prob.col = 2,
                                   criterion = "predvar", delta = 0.08,
                                   batch.size = 10, poly = NULL, plotit = TRUE)

#adaptive sampling using exceedance probability criterion.
adapt.design.ep <- adaptive.sample(obj1 = obj1, obj2 = init.design,
                                   pred.var.col = 1, excd.prob.col = 2,
                                   criterion = "exceedprob", delta = 0.08,
                                   batch.size = 10, poly = NULL, plotit = TRUE)
# }
# NOT RUN {


# }

