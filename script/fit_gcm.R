# fit gcm on nonword data, use latinfrench/germanyiddish as two categories

# -- head -- #

setwd('~/Github/xxx/')

library(tidyverse)
library(glmmTMB)
library(performance)
library(brms)

# -- fun -- #

# Hungarian orthography to IPA-like transcription for distance lookup
transcribeIPA = function(dat) {
  dat |> stringr::str_replace_all(c(
    'x' = 'ks', 'cs' = 'č', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ',
    'ny' = 'ṉ', 'sz' = 'ß', 's' = 'š', 'ß' = 's', 'ck' = 'kk',
    'codec' = 'kodek', 'ch' = 'h', 'ly' = 'j'
  ))
}

source('script/gcm.R')

# -- read -- #

trials_nonwords = read_tsv('dat/filtered_data_nonword.tsv')   # nonword trial data
racz_rebrus     = read_tsv('dat/racz_rebrus.tsv')   # real word phonological predictions
ph_dist         = read_tsv('dat/word_distances.tsv.gz')        # pre-computed pairwise phonological distances

# -- setup gcm -- #

training = racz_rebrus |>
  mutate(
    transcribed = transcribeIPA(stem),
    gcm_category = case_when(                                   # latin/french = front, german/yiddish = back
      language %in% c('yi','de') ~ 'back',
      language %in% c('fr','la') ~ 'front'
    )
  ) |>
  filter(!is.na(gcm_category)) |>                              # drop words with no category
  select(stem, transcribed, gcm_category, log_odds_back)

test_1 = trials_nonwords |>
  mutate(
    transcribed = transcribeIPA(target),               # convert Hungarian orthography to phonological form
    accept = as.double(accept)
         ) |>
  rename(stem = target)

test = test_1 |>
  select(id, stem, transcribed, accept)

training |> count(gcm_category)

# -- fit gcm -- #

set.seed(1337)                                                  # reproducibility

param_grid = tidyr::crossing(                                   # hyperparameter search grid
  var_s = seq(0.1, 5, by = 0.25),                              # sensitivity: controls decay of similarity with distance
  var_p = c(1, 2)                                         # distance metric: 1 = city-block, 2 = Euclidean
)

results = param_grid |>                                         # evaluate each parameter combination
  mutate(
    rmse = purrr::map2_dbl(var_s, var_p, \(s, p) {
      dat = GCM(test, training, ph_dist, var_s = s, var_p = p) |>   # GCM predictions for this s and p
        dplyr::left_join(test, by = 'transcribed')                    # join to trial-level data
      fit = glmmTMB(                                                   # binomial GLMM with by-participant random slopes
        accept ~ p_back + (1 | id) + (1 | transcribed),
        data = dat,
        family = binomial
      )
      sqrt(mean((as.numeric(dat$accept) - fitted(fit))^2))            # RMSE: fitted probability vs. observed binary
    })
  )

best_params = results |>                                        # select best parameter pair
  dplyr::slice_min(rmse, n = 1)

best_preds = GCM(                                               # GCM predictions with best parameters
  test, training, ph_dist,
  var_s = best_params$var_s,
  var_p = best_params$var_p
) |>
  dplyr::left_join(test, by = 'transcribed') |>                    # join to trial-level data
  dplyr::left_join(test_1 |> distinct(transcribed,stem)) |> # ugh
  mutate(c_p_back = scale(p_back)[,1])

best_fit = brm(                                             # refit GLMM with best parameters
  accept ~ c_p_back + (1 | id) + (1 | transcribed),
  data = best_preds,
  family = bernoulli,
  cores = 4,
  chains = 4
)

# -- check -- #

summary(best_fit)
broom.mixed::tidy(best_fit) |> 
  filter(effect == 'fixed') |> 
  select(term,estimate,conf.low,conf.high) |> 
  knitr::kable('latex', digits = 2)
sjPlot::plot_model(best_fit, 'pred')
hypothesis(best_fit, 'c_p_back > 0')
best_preds |> 
  summarise(
    mean = mean(accept),
    .by = c(stem,p_back)
  ) |> 
  ggplot(aes(p_back,mean)) +
  geom_point() +
  geom_smooth()
