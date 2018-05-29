#============================================================================
# File: 01_rstan_analysis.R
# Date: 29/05/2018
# Author: Jason Grafmiller
#
# Fit Bayesian generalized linear mixed models using stan on genitives data.
#============================================================================

# Load libraries
library(plyr)
library(tidyverse)
library(reshape2)

library(rstan)
library(brms)
library(shinystan)

# Setup -------------------------------------------------------------------

# Load data
source("munge/01_load_data.R")
source("munge/02_split_data_by_genre.R")

# Set the rstan options.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Fit stan models ---------------------------------------------------------

# Fit regression models using brms and stan.

# formula
# We'll consider only the most important predictors
fmla <- Type ~ (1|Speaker_ID) +
  (0 + c.PossessorAnim.Binary|Speaker_ID) +
  c.PossessorAnim.Binary +
  z.PossessorWC +
  z.PossessumWC +
  c.Final.Sibilant +
  c.Possessor.Givenness +
  c.Possessor.Expression.Type +
  c.PersistenceBinary +
  c.ProtoSemanticRelation

# Standardize predictors according to Gelman (2008).
corpus_genre_dlist <- llply(corpus_genre_dlist,
  .fun = function(d) {
    d <- mutate(d,
      c.PossessorAnim.Binary = c.(PossessorAnim.Binary),
      c.Final.Sibilant = c.(Final.Sibilant),
      c.Possessor.Givenness = c.(Possessor.Givenness),
      c.PersistenceBinary = c.(PersistenceBinary),
      c.ProtoSemanticRelation = c.(ProtoSemanticRelation),
      c.Possessor.Expression.Type = c.(Possessor.Expression.Type),
      z.PossessorWC = z.(PossessorWC, factor = 2),
      z.PossessumWC = z.(PossessumWC, factor = 2))
    return (d)
    })

# Create custom fitting function
fit.stan.func <- function(x){
  m <- brm(fmla, data = x, family = "bernoulli",
    prior = c(set_prior("cauchy(0, 10)", class = "b")),
    chains = 4, iter = 3000, warmup = 600, seed = 43214,
    refresh = 0)
  return(m)
}

# Fit models
t <- proc.time()
written_stan_models <- vector("list")
for(i in 1:10) {
  written_stan_models[[i]] <- corpus_genre_dlist[[i]] %>%
    fit.stan.func(.)
}
t2 <- proc.time() - t

# Save to disk
saveRDS(written_stan_models, 'data/written_stan_models.RDS')


# Inspect models ----------------------------------------------------------

# shinystan!
# Check each model...
launch_shinystan(written_stan_models[[1]])


# Fixed effects -----------------------------------------------------------

# create a dataframe for inspecting factors
coef_stan <- plyr::ldply(d, .progress = "text",
  .id = "Name",
  .fun = function(d){
    posterior_samples(d, pars = '^b') %>%
      mutate(chain = rep(1:4, each = 2400),
             iteration = rep(1:2400, 4)) %>%
      reshape2::melt(id.vars = c("chain", "iteration"))
  }) %>% group_by(Name, variable) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    low95 = quantile(value, .025),
    hi95 = quantile(value, .975),
    low50 = quantile(value, .25),
    hi50 = quantile(value, .75)) %>%
  mutate(., Corpus = gsub("\\..*$", "", Name),
    Genre = gsub("^.*\\.", "", Name),
    Genre = fct_recode(Genre, `Adventure Fiction` = "Western Fiction") %>%
     factor(levels = levels(gens$Genre)),
    variable = gsub("^b_([cz]\\.|)", "", variable) %>%  factor,
    sig = ifelse(low95 > 0 | hi95 < 0, "sig", "ns"))


# Visualize coefficient effects
ggplot(coef_stan, aes(x = variable, y = mean, color = Genre)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = low95, ymax = hi95),
                  position = position_dodge(width = .4)) +
  geom_linerange(aes(ymin = low50, ymax = hi50),
                 position = position_dodge(width = .4), size = 1.5) +
  coord_flip() +
  facet_grid(Corpus ~ Genre)



# Predictor importance ----------------------------------------------------

# define function for calculating WAIC or LOO information criterion for
# estimating predictor importance
permute.varimp.brms <- function(fit, data = NULL, verbose = FALSE, ranef = TRUE,
                                ic = c("WAIC", "LOO")){
  require(rstan, quietly = T)
  require(brms, quietly = T)

  if (class(fit)[1] != "brmsfit"){
    stop("fit is not a supported class 'brmsfit'")
  }
  fmla <- fit$formula
  data <- fit$data
  ic = ic[1]
  resp <- fit$formula$resp
  y <- as.numeric(data[,resp]) - 1
  if (ranef){
    random <- names(ranef(fit))
    vars <- names(data)[-1]
  } else {
    # only fixed effs
    vars <- names(data)[-which(names(data) %in% c(random, resp))]
  }
  # remove interaction terms
  vars <- vars[grep(":", vars, invert = T)]
  # get information criteria from full model
  full.probs <- as.data.frame(predict(fit))
  full.C <- somers2(full.probs[, 1], y)[[1]]
  full.acc <- mean(round(full.probs[, 1]) == y)
  if (ic == "LOO") {
    full.ic <- LOO(fit)$estimates[3, 1]
  } else full.ic <- WAIC(fit)$estimates[3, 1]
  varimp_mat <- matrix(nrow = length(vars), ncol = 3)
  if (verbose) {cat("variables run:\n")}
  # loop through (fixed effects) predictors
  for (i in seq(1, length(vars))){
    # find main effect and any interactions
    d <- data
    d[, vars[i]] <- sample(data[, vars[i]]) # reshuffle values
    new_fit <- update(fit, newdata = d, refresh = 0)
    new.probs <- as.data.frame(predict(new_fit))
    new.C <- somers2(new.probs[,1], y)[[1]]
    C.diff <- full.C - new.C
    new.acc <- mean(round(new.probs[,1]) == y)
    Acc.diff <- full.acc - new.acc
    if (ic == "LOO") {
      new.ic <- LOO(new_fit)$estimates[3, 1]
    } else new.ic <- WAIC(new_fit)$estimates[3, 1]
    # The difference in AICc here is the same as the likelihood ratio
    IC.diff <- new.ic - full.ic
    varimp_mat[i, ] <- c(C.diff, Acc.diff, IC.diff)
    if (verbose) {cat(vars[i], "... ", sep = "")}
  }
  rownames(varimp_mat) <- vars
  colnames(varimp_mat) <- c("C", "accuracy", "IC")
  return(as.data.frame(varimp_mat))
}

t <- proc.time()
varimp_stan <- plyr::llply(written_stan_models,
  .fun = function(d){
    vmp <- permute.varimp.brms(d) %>%
      mutate(pred = rownames(.))
    return(vmp)
  })
t2 <- proc.time() - t

# For visualization
varimp_df <- ldply(varimp_stan, .id = "Name") %>%
  mutate(Genre = gsub(".*\\.", "", Name),
         Corpus = gsub("\\..*", "", Name),
         pred = gsub("[cz]\\.","", pred))

# Create plots
plot_list <- vector("list")
for (i in 1:10){
  x <- varimp_stan[[i]]
  if (i > 6) {
    col <- pal_jco("default")(4)[1]
  } else col <- pal_jco("default")(4)[4]
  nam <- gsub(".*\\.", "", names(varimp_stan)[[i]])
  p <- mutate(x,
      pred = gsub("[cz]\\.","", pred),
      pred = gsub("Possessor", "Possr", pred),
      pred = gsub("Possessum", "Possm", pred),
      pred = gsub("Speaker_ID", "Author", pred),
      pred = gsub("PersistenceBinary", "Persistence", pred),
      pred = gsub("Anim.Binary", ".Animacy", pred),
      pred = gsub("Corpus", "Time", pred),
      pred = gsub("ProtoSemanticRelation", "Semantic.Relation", pred),
      pred = gsub("Expression.Type", "Type", pred),
      pred = gsub("LogTFreq", "Log.Freq", pred)
    ) %>%
    ggplot(aes(x = fct_reorder(pred, IC), y = IC)) + coord_flip() +
    # geom_ribbon(x = seq(.5, 12.5, length.out = 12), ymin = m, ymax = abs(m), alpha = .3) +
    geom_hline(yintercept = 0, col = "red") +
    geom_segment(aes(xend = pred, yend = 0), color = pal_jco("default")(4)[4]) +
    geom_point(color = pal_jco("default")(4)[4]) +
    # scale_color_manual(guide = "none", values = c("gray", qlvlblue)) +
    geom_hline(yintercept = c(m, abs(m)), linetype = 2) +
    labs(x = "", y = "", title = nam) +
    theme_minimal()
  plot_list[[i]] <- p
}

# Plot Brown varimp graphs
do.call("grid.arrange", c(plot_list[1:5], ncol = 3))
# Plot Frown varimp graphs
do.call("grid.arrange", c(plot_list[6:10], ncol = 3))


# Random slopes -----------------------------------------------------------

ranef_slopes <- plyr::ldply(written_stan_models, .progress = "text",
  .id = "Name",
  .fun = function(d){
   df <- ranef(d)$Speaker_ID[,,2] %>%
     as.data.frame %>%
     mutate(Text = rownames(.))
   return(df)
  }) %>%
  mutate(Corpus = gsub("\\..*$", "", Name),
         Genre = gsub("^.*\\.", "", Name),
         Genre = fct_recode(Genre, `Adventure Fiction` = "Western Fiction") %>%
           factor(levels = levels(gens$Genre)))

head(ranef_slopes)

# Plot Brown author slopes
subset(ranef_slopes, Corpus == "Brown") %>%
  ggplot(aes(Genre, Estimate, color = Genre, fill = Genre)) +
  geom_point(position = position_jitter(width = .15),
             alpha = .9) +
  geom_boxplot(outlier.size = 0, alpha = .3, notch = F) +
  labs(x = "", y = "") +
  scale_color_jco(guide = "none") +
  scale_fill_jco(guide = "none") +
  theme(axis.text.x = element_text(size = rel(1.25)))
# 1080px by 500px

# Plot Brown author slopes
subset(ranef_slopes, Corpus == "Frown" & Estimate > -3) %>%
  ggplot(aes(Genre, Estimate, color = Genre, fill = Genre)) +
  geom_point(position = position_jitter(width = .15),
             alpha = .9) +
  geom_boxplot(outlier.size = 0, alpha = .3, notch = F) +
  labs(x = "", y = "") +
  scale_color_jco(guide = "none") +
  scale_fill_jco(guide = "none") +
  theme(axis.text.x = element_text(size = rel(1.25)))

