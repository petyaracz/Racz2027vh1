# grab words from exp, grab category, print into file in local

# -- head -- #

setwd('~/Github/xxx/')

library(tidyverse)

# -- read -- #

words = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1U0HUTrINAZLPFPIHse-4yVAa_xlmwozxRqKCYYLbz9E/edit?usp=sharing', 'Sheet2')
trials_nonwords = read_tsv('dat/filtered_data_nonword.tsv')

# -- keep -- #

words = words |> 
  mutate(word = str_extract(s0, '(?<=egy ).*(?=\\.$)')) |> 
  select(class,word)

words = words |> 
  filter(word %in% trials_nonwords$target)

# -- write -- #

write_tsv(words, 'dat/nonword_classes.tsv')
