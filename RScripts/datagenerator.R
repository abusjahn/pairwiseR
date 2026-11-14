pacman::p_load(tidyverse)

# definitions #####
n_samples <- 50
set.seed(581210)
# some dummy data #####
## wide #####

widedata <- tibble(
  ID = paste("M", 1:n_samples),
  trait1_bl = rnorm(n_samples, 100, 10),
  trait2_bl = rlnorm(n_samples, 2, 1),
  trait3_bl = runif(n_samples, min = 18, max = 90),
  trait1_fu = trait1_bl - rnorm(n_samples, 25, 5),
  trait2_fu = trait2_bl - rnorm(n_samples, 1, .2),
  trait3_fu = trait3_bl + rnorm(n_samples, 1, .1),
)

widedata_na <- widedata |>
  mutate(
    across(contains("trait"), \(x) {
      x[sample.int(n(), size = 0.2 * n())] <- NA   # 20% missing
      x
    })
  )

## long #####

longdata <-
  widedata |>
  pivot_longer(cols = -ID,
    names_to = c(".value", "Visit"),
    names_sep = "_")

longdata_na <-
  widedata_na |>
  pivot_longer(cols = -ID,
    names_to = c(".value", "Visit"),
    names_sep = "_")

# saving #####
save(list = ls(pattern = "data"),
  file = "Data/simdata.Rdata")
