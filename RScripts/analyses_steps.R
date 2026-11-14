pacman::p_load(tidyverse, waldo)
load("data/simdata.Rdata")
# pairwise analyses ####
## from wide #####
### t-test for trait1 ####
t_w_1 <- t.test(widedata$trait1_bl,
  widedata$trait1_fu,
  paired = TRUE)
t_w_na_1 <- t.test(widedata_na$trait1_bl,
  widedata_na$trait1_fu,
  paired = TRUE)

### wilcoxon test for trait2 ####
w_w_2 <- wilcox.test(widedata$trait2_bl,
  widedata$trait2_fu,
  paired = TRUE)
w_w_na_2 <- wilcox.test(widedata_na$trait2_bl,
  widedata_na$trait2_fu,
  paired = TRUE)



## from long ####
### t-test for trait1 ####
t_l_1 <- t.test(longdata$trait1[longdata$Visit == "bl"],
  longdata$trait1[longdata$Visit == "fu"],
  paired = TRUE)
t_l_na_1 <- t.test(longdata_na$trait1[longdata$Visit == "bl"],
  longdata_na$trait1[longdata$Visit == "fu"],
  paired = TRUE)
t_l_na_dropped_1 <- t.test(longdata_na |>
  filter(Visit == "bl") |>
  pull(trait1) |>
  na.omit(),
longdata_na |>
  filter(Visit == "fu") |>
  pull(trait1) |>
  na.omit(),
paired = TRUE)

### wilcoxon test for trait2 ####



# comparison results ####
compare(t_w_1, t_l_1)
compare(t_w_na_1, t_l_na_1)
compare(t_l_na_1, t_l_na_dropped_1)


# extract names parts from wide what vs. when ####
# create more example names in different ways like whatever.1 whatever.2 etc
# create a function that would do this with a pattern input using str_glue notation



# notes ####
# pairwise long needs ID for matching, probably best to then pivot_wider?!
