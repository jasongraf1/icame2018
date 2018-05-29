#============================================================================
# File: 02_split_data_by_genre.R
# Date: 07/07/2016
# Author: Jason Grafmiller
#
#============================================================================

# source("munge/01_load_data.R")
genres <- levels(gens$Genre)
corpora <- levels(gens$Corpus)

genre_dlist <- plyr::dlply(gens, plyr::.(Genre), droplevels)

corpus_dlist <- plyr::dlply(gens, plyr::.(Corpus), droplevels)

corpus_genre_dlist <- plyr::dlply(gens, plyr::.(Corpus, Genre), droplevels)
