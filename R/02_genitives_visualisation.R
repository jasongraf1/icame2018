#============================================================================
# File: 02_genitives_visualisation.R
# Date: 29/05/2018
# Author: Jason Grafmiller
#
# Fit Bayesian generalized linear mixed models using stan on genitives data.
#============================================================================

# Load libraries
library(plyr)
library(tidyverse)
library(reshape2)

library(ggsci) # for colors
library(ggpubr)
theme_set(theme_minimal())
theme_update(text = element_text(family = "Open Sans"))

devtools::install_github("jasongraf1/JGggplot")
library(JGggplot)


# Setup -------------------------------------------------------------------

# Load data (if not already loaded)
source("munge/01_load_data.R")
source("munge/02_split_data_by_genre.R")



# Plot 1 ------------------------------------------------------------------

# Bar plot of distribution of genitive variants by genre and time

ggBar.plot(gens, "Time", "Type", facet = "Genre",
           opp.cols = T, facet.cols = 5, size = 5) +
  # scale_fill_manual(name = "", values = brewer.pal(3, "PuBu")[-2]) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        strip.text = element_text(size =rel(1.25)),
        axis.text.x = element_text(size = rel(1.75)),
        legend.text = element_text(size = rel(1.5))) +
  scale_fill_manual(name = "", values = c("#EFC000FF", "#0073C2FF"))


# Plot 2 ------------------------------------------------------------------

# Bar plot of by-author variation in genitives across genres

t <- table(gens$Speaker_ID)
dt <- data.frame(N = as.vector(t),
  s.gen = as.vector(table(gens$Speaker_ID, gens$Type)[, 2]))
dt$Author <- names(t)
dt$Genre <- rep(c("Press", "Non-fiction", "Learned",
  "General\nFiction", "Adventure\nFiction"), c(32, 33, 24, 57, 31))
dt2 <- split(dt, dt$Genre, drop = TRUE)
dt <- plyr::ldply(dt2, function(x) return(x[order(x$N, decreasing = T), ]))
dt$Author <- factor(dt$Author, levels = dt$Author)
dt$Genre <- factor(dt$Genre)

# the plot
ggplot(dt, aes(Author, N)) +
  geom_bar(aes(fill = Genre), stat = 'identity', alpha = .4) +
  geom_bar(aes(y = s.gen, fill = Genre), stat = 'identity') +
  theme(axis.text.x = element_blank(), legend.title = element_blank(),
        legend.text = element_text(size = rel(1.25)), legend.position = "top") +
  scale_fill_jco()


# Plot 3 ------------------------------------------------------------------

# Variable importance rankings of by-Genre/Time models. See file
# '01_rstan_analysis.R' for how to get varimp_df object.

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


# Plots 4 & 5 -------------------------------------------------------------

# Plots of by-author adjustments to the slope coefficient for Possessor Animacy.

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



