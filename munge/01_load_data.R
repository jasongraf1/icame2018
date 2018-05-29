#============================================================================
# File: 01_load_data.R
# Date: 07/07/2016
# Author: Jason Grafmiller
#
# Load in data and subset it.
#============================================================================

# written dataset
genitives <- read.delim("data/written_2016.txt", strip.white = T)

# spoken dataset
spoken <- read.delim("data/spoken_2014.txt")

# Fix name sin spoken dataset to match those of the written one.
spoken <- mutate(spoken,
 Corpus = "SWBD",
 Genre = "Spoken Conv.",
 GenreBinary = "Non-Press",
 Modality = "spoken",
 PersistenceBinary = ifelse(Strict.Persistence == "s", "Y", "N"),
 Time = "1990s"
) %>%
  rename(
    SemanticRelation = "Semantic.Relation",
    Possessor.Head = "P.or.Head",
    Possessum.Head = "P.um.Head",
    Possessor.Expression.Type = "P.or.Part.of.Speech",
    PossessorAnim.Binary = "Possessor.Animacy.Binary",
    PossessumAnim.Binary = "Possessum.Animacy.Binary",
    ProtoSemanticRelation = "protoSem",
    TypeTokenRatio = "TypeTokenR",
    PossessorFreq = "Por.Count",
    PossessorTextFreq = "Por.Freq",
    PossessorLogTFreq = "Por.Log.Freq"
  )

# Fix word counts -------------------------------------------------------

genitives <- genitives %>%
  mutate(PossessorWC = str_count(Possessor, "\\S+"),
    PossessumWC = str_count(Possessum, "\\S+"),
    PossessorLogTFreq = log10(PossessorTextFreq),
    TypeTokenRatio = as.numeric(as.character(TypeTokenRatio))
    )

# fix TTR so that is is on a 100 scale
genitives[genitives$Corpus == "Frown", "TypeTokenRatio"] <- genitives[genitives$Corpus == "Frown", "TypeTokenRatio"] * 100


# Separate out the relevant info ----------------------------------------

keep <- c(names(genitives)[c(2:4, 6:8, 10:15, 19:21, 32:35, 41:43, 45, 47, 49:51, 54)],
          "Time")

gens <- subset(genitives,
  !is.na(PossessorFreq) &
  !is.na(Resp) &
  Comments %in% c('', '0', '1') &
  !is.na(TypeTokenRatio) &
  TypeTokenRatio < 100 &
  PossessorWC <= 10 &
  PossessumWC <= 10) %>%
  mutate(
    Time = fct_recode(Corpus, `1960s` = "Brown", `1990s` = "Frown"),
    Genre = fct_recode(Genre, `Adventure Fiction` = "Western Fiction")
    )

gens[gens$PossessorLogTFreq == "-Inf", "PossessorLogTFreq"] <- log10(1/2000)

gens <- droplevels(gens[, keep])


# Convert factors ---------------------------------------------------------

gens[c(1, 4:5, 9:10, 13, 16:19, 26:27)] <- lapply(gens[c(1, 4:5, 9:10, 13, 16:19, 26:27)], as.factor)

gens$PossessorAnim.Binary <- relevel(gens$PossessorAnim.Binary, ref = "Inanim")
gens$Genre <- relevel(gens$Genre, ref = "Press")

gens <- mutate(gens,
  Genre.sum = Genre,
  Genre.sum2 = factor(Genre, levels = rev(levels(Genre))))

contrasts(gens$Genre.sum) <- contr.sum(5)
contrasts(gens$Genre.sum2) <- contr.sum(5)





