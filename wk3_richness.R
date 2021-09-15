# Intro to biodiversity concepts and metrics
# Michael Roswell 20210913

# The following exercise is a really quick primer on common concepts and
# measures of biodiversity. Please work with a partner to add code, graphs, and
# comments below

library(dplyr)
library(ggplot2)
library(vegan) # this one can be a bit of a pain to install
library(iNEXT) # this one is kind of optional, but lightweight


?BCI
data("BCI")
head(BCI)
# please add a sentence or two about what "BCI" is:

# How many individual trees are in each 1 Ha plot?

stemcount <- rowSums(BCI)
summary(stemcount)

# please add a sentence or two about the number of trees in the BCI plots:

# often the simplest way to think about diversity is "how many different kinds
# of [blah] can be found in [bleh]). Here we might look at how many kinds of
# tree live in each plot.

raw_rich <- function(x){sum(x>0)} # this counts cases >0

BCI_rich<-apply(BCI, 1, raw_rich)
BCI_rich
summary(BCI_rich)

# one reason I am suspicious of richness is that it can reflect abundance
# strongly. Does it here? Show your work. Hints: you might make a graph or use
# the function `cor()`





# as you probably just read, although the maximum number of tree species in any
# one plot was 109, across all 50 plots there were 225 species detected! 

# Ecologists have long thought about diversity at multiple simultaneous scales.
# e.g. here we might be interested in tree diversity on the whole island, and
# also the tree diversity in any given 1-ha plot, and perhaps even comparing how
# different plots tend to be from each other.

# The diversity of the whole thing (e.g. the whole island) is commonly called
# "gamma diversity"

# quickly write some definitions for alpha and beta diversity here. If you feel
# uncertain about this, that's fine, we'll chat about it later!




##########################################################################
######################### Checkpoint 1 ##################################
###########################################################################
### let Michael/Dan know you are here before moving on ####################
###########################################################################

# Diversity concepts are pretty direct in situations where complete censuses are
# possible, and we won't spend that much time talking about those situations.
# Discuss with your partner: what are some key sampling concerns for the
# taxa/systems you work? Can you census organisms as one might trees > 1cm dbh
# on a small island?


# while one of us could likely census the trees in any given hectare plot
# comprehensively in a single day, we could not census the whole island (where
# by census, I mean record the species identity of every single tree; the whole
# island is ~1560 Ha). But one might want to know, "How many tree species
# lived on the whole island in the year 2000?"

# I think that number is what we should call the tree "richness" of BCI. Here
# are some ways we might estimate that number, without exhaustively censusing
# each Ha.

# raw richness, again, but now at the whole island scale

BCI_gamma <- raw_rich(colSums(BCI))
BCI_gamma # over the 50 plots, observers recorded 225 species

# but did they see all of them?
BCI_accum<-data.frame()
for(i in 1:nrow(BCI)){
  BCI_accum[i,"plots"]<-i
  BCI_accum[i, "species"] <- raw_rich(colSums(BCI[1:i,]))
}

BCI_accum %>% ggplot(aes(plots, species)) +
  geom_point() +
  theme_classic()

# in one to three sentences, what does this plot show


# now we'll make a similar, but more abstract plot
reps<-50

BCI_raref<-data.frame()

for(i in 2:nrow(BCI)-1){
  for(j in 1:reps){
    BCI_raref[(i-1)*100+j #reason I don't use for loops anymore!
              ,"plots"] <- i 
    BCI_raref[(i-1)*100+j, "species"] <- raw_rich(
      colSums(BCI[
        sample(1:nrow(BCI), i, replace =T) # random set of rows (plots) 
        , ]
      )
    )}
}

# besides more complicated, what's different this time? Try to summarize what
# the code chunk above is doing in a few sentences.

BCI_raref %>% ggplot(aes(plots, species)) +
  geom_point(alpha = 0.3) +
  # geom_smooth(alpha = 0.4, color ="blue") + # I thought maybe a fit line 
  # # would be helpful, but I don't think we need it and it's a bit wobbly
  theme_classic()

# If you were to guess, how many tree species would there be at 1560 plots/ the
# whole island?

# can you make a good guess based on this curve?

# I think the answer might be around 480 species of tree. 

iNEXT::ChaoSpecies(t((BCI>0)), datatype = "incidence_raw") # I think this is using the Chao2 estimator. 

# as you may have heard, I have some skepticism about richness estimators

##########################################################################
######################### Checkpoint 2 ##################################
###########################################################################
### let Michael or Dan know you are here. We will do the ################
### next part together ##################################################
###########################################################################

