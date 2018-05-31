#============================================================================
# File: 03_MDS_analysis.R
# Date: 05/31/2018
# Author: Jason Grafmiller
#
# Multidimensional scaling plots for genitives registers
#============================================================================

# Load libraries
library(plyr)
library(tidyverse)
library(reshape2)

library(rstan)
library(brms)

library(ggsci) # for colors
library(plotly) # for 3d visualisation

# Setup -------------------------------------------------------------------

# Load data
source("munge/01_load_data.R")
source("munge/02_split_data_by_genre.R")

# Run '01_rstan_analysis.R' before to generate models.
mod_list <- c(written_stan_models, swbd_model)


# Get posterior samples for coefficients ----------------------------------

post_samp <- vector("list")
for(i in 1:11){
  mod <- mod_list[[i]]
  post_samp[[i]] <- posterior_samples(mod, pars = '^b')
}
names(post_samp) <- names(mod_list)

# create dataframe with mean posterior estimate
mean_df <- ldply(post_samp,
  .fun = function(x) apply(x, 2, mean), .id = "Genre")
rownames(mean_df) <- names(mod_list)


# Create MDS --------------------------------------------------------------

# make distance matrices
# written only
genre_dist_wr <- dist(mean_df[-11, -1])
genre_mds_wr <- cmdscale(genre_dist_wr, eig = T, k = 3)

# written and spoken
genre_dist_sp <- dist(mean_df[, -1])
genre_mds_sp <- cmdscale(genre_dist_sp, eig = T, k = 3)

# For ggplot
genre_mds_wr2 <- genre_mds_wr[[1]] %>%
  as.data.frame() %>%
  dplyr::rename(x = "V1", y = "V2", z = "V3") %>%
  mutate(name = rownames(.) %>% factor,
    genre = gsub("^.*\\.", "", name) %>% factor,
    corpus = gsub("\\..*$", "", name) %>% factor
    )

genre_mds_sp2 <- genre_mds_sp[[1]] %>%
  as.data.frame() %>%
  dplyr::rename(x = "V1", y = "V2", z = "V3") %>%
  mutate(name = rownames(.) %>% factor,
         genre = gsub("^.*\\.", "", name) %>% factor,
         corpus = gsub("\\..*$", "", name) %>% factor
  )

# Scree plots
data.frame(Eigenvalue = genre_mds_wr$eig,
  Dim = factor(1:length(genre_mds_wr$eig))) %>%
  ggplot(aes(Dim, Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = .7) +
  geom_line(aes(group = 1)) +
  geom_point() +
  ggtitle("SCree plot of genitives MDS (written only)")

data.frame(Eigenvalue = genre_mds_sp$eig,
           Dim = factor(1:length(genre_mds_sp$eig))) %>%
  ggplot(aes(Dim, Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = .7) +
  geom_line(aes(group = 1)) +
  geom_point() +
  ggtitle("SCree plot of genitives MDS (written only)")


# MDS plots
ggplot(genre_mds_wr2, aes(x, y, color = corpus)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  coord_flip() +
  geom_text(aes_string(label = "genre"), nudge_x = .4, nudge_y = .5) +
  scale_color_manual(name = "", values = pal_jco("default")(4)[c(1,4)]) +
  theme_bw() +
  theme(legend.position = "top")

ggplot(genre_mds_sp2, aes(x, y, color = corpus)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  # coord_flip() +
  geom_text(aes_string(label = "genre"), nudge_x = .5, nudge_y = .3) +
  scale_color_manual(name = "", values = pal_jco("default")(4)[c(1,4, 3)]) +
  theme_bw() +
  theme(legend.position = "top")

# 3D plots
plot_ly(data = genre_mds_wr2) %>%
  add_trace(x = ~x, y = ~y, z = ~z,
            type = "scatter3d", inherit = F,
            color = ~ corpus,
            mode = "markers") %>%
  add_text(x = ~x, y = ~y, z = ~z,
           text = ~ genre,
           type = "scatter3d",
           mode = "markers")

plot_ly(data = genre_mds_sp2) %>%
  add_trace(x = ~x, y = ~y, z = ~z,
            type = "scatter3d", inherit = F,
            color = ~ corpus,
            mode = "markers") %>%
  add_text(x = ~x, y = ~y, z = ~z,
           text = ~ genre,
           type = "scatter3d",
           mode = "markers")
