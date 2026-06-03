# grab data from Racz Rebrus 2026, print into file in local

# -- head -- #

setwd('~/Github/xxx/')

library(tidyverse)

# -- read -- #

l = read_tsv('https://raw.githubusercontent.com/petyaracz/RaczRebrus2024/refs/heads/main/dat/stemlanguage.tsv')
d = read_tsv('https://raw.githubusercontent.com/petyaracz/RaczRebrus2024/refs/heads/main/dat/dat_wide_stems.tsv')

# -- setup -- #

d2 = d |> 
  filter(stem_varies == T) |> 
  left_join(l)

# -- check -- #

drop = c('komplett','korrekt')

d2 = d2 |> 
  filter(!stem %in% drop)

# -- write -- #

write_tsv(d2, 'dat/racz_rebrus.tsv')
