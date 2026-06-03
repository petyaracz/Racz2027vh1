##
# GCM (Nosofsky 1988)
##

# returns p(back) for each unique test transcription given training exemplars and pre-computed phonological distances
GCM = function(test, training, ph_dist, var_s, var_p) {
  tidyr::crossing(                                                                    # all test x training pairs
    test_w  = unique(test$transcribed),                                               # unique test transcriptions
    train_w = training$transcribed                                                    # training transcriptions
  ) |>
    dplyr::left_join(ph_dist, join_by(test_w == word1, train_w == word2)) |>         # look up phonological distance
    dplyr::left_join(                                                                  # attach category label
      dplyr::select(training, transcribed, gcm_category),
      join_by(train_w == transcribed)
    ) |>
    dplyr::mutate(pairwise_sim = exp(-phon_dist / var_s)^var_p) |>                   # pairwise similarity
    dplyr::group_by(test_w, gcm_category) |>                                          # group by test word and category
    dplyr::summarise(category_sim = sum(pairwise_sim), .groups = 'drop_last') |>     # sum similarity per category
    dplyr::mutate(total_sim = sum(category_sim)) |>                                   # total similarity per test word
    dplyr::ungroup() |>
    dplyr::mutate(p_back = category_sim / total_sim) |>                               # normalise to probability
    dplyr::filter(gcm_category == 'back') |>                                          # keep back category only
    dplyr::select(transcribed = test_w, p_back)                                       # return clean output
}
