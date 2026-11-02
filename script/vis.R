# make plots

# -- head -- #

setwd('~/Github/Racz2027vh1/')

library(tidyverse)
library(ggthemes)

# -- read -- #

d = read_tsv('dat/filtered_data_nonword_class_gcm_preds.tsv')

# -- setup -- #

nonword_means = d |> 
  summarise(
    p = mean(accept),
    lo = qlogis(p),
    .by = c(word,class,word_class,p_back,c_p_back)
  )

# -- viz -- #

nonword_means |> 
  ggplot(aes(p,p_back,label = word,colour = class)) +
  geom_label()

nonword_means |> 
  mutate(
    `word type` = ifelse(class == 'bizalmas',
                         'Yiddish/German',
                         'French/Latin'
    )
  ) |> 
  ggplot(aes(`word type`,p)) +
  ggrain::geom_rain() + 
  theme_bw() +
  coord_flip() +
  ylab('mean accept "back"')
ggsave('viz/distributions.pdf', width = 4, height = 3)


