# get per nonword counts

# -- head -- #

setwd('~/Github/Racz2027vh1/')

library(tidyverse)

# -- read -- #

d = read_tsv('dat/filtered_data_nonword_class_gcm_preds.tsv')

# -- aggr -- #

aggr = d |> 
  count(word,word_class,accept) |> 
  pivot_wider(names_from = accept, values_from = n, values_fill = 0) |> 
  mutate(
    p_pretest = `TRUE` / (`TRUE` + `FALSE`),
    lo_pretest = qlogis(p_pretest)
         ) |> 
  select(word,word_class,p_pretest,lo_pretest)

# -- write -- #

write_tsv(aggr, '~/Github/Racz2027mnszmtsz/dat/nonword_pretest.tsv')
