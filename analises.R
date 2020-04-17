

fs::dir_create("data-raw") # Guardar o csv aqui
fs::dir_create("data")

tidy_tweets("data-raw/mandetta_demit.csv", "data")

tweets <- readr::read_rds("data/tweets_tidy.rds")


qtd_tw_tempo(tweets, "Mandetta")

tw_hashtags(tweets)

tw_urls(tweets)

tw_sent <- tw_sent_base(tweets)


# 10 tweets mais positivos e negativos
tw_pos_neg(tweets, tw_sent, "positivo") %>% dplyr::glimpse()
tw_pos_neg(tweets, tw_sent, "negativo") %>% dplyr::glimpse()


# Juntando informações de sentimento aos dados dos tweets
tw_fim <- tweets %>%
  dplyr::inner_join(
  tw_sent %>% dplyr::select(id, sentiment = tw_sentiment_op),
  by = "id")


sent_hora(tw_fim, "Mandetta")

pos_neg_hora(tw_fim, "Mandetta")

prop_pos(tw_fim, "Mandetta")

