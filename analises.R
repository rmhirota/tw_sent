source("R/1-import_tidy.R")
source("R/2-explore.R")
source("R/3-sent.R")

fs::dir_create("data-raw") # Guardar o csv aqui
fs::dir_create("data")

tidy_tweets("data-raw/mandetta_demit.csv", "data")

tweets <- readr::read_rds("data/tweets_tidy.rds")


qtd_tw_tempo(tweets, "Mandetta")

tw_hashtags(tweets)

tw_urls(tweets)

tw_sent <- tw_sent_base(tweets)
readr::write_rds(tw_sent, "data/tw_sent.rds")


# 10 tweets mais positivos e negativos
tw_pos_neg(tweets, tw_sent, "positivo") %>%
  with(status_url) %>% paste(sep = "\n") %>% clipr::write_clip()
tw_pos_neg(tweets, tw_sent, "negativo") %>%
  with(status_url) %>% paste(sep = "\n") %>% clipr::write_clip()



sent_hora(tw_fim, "Mandetta")

pos_neg_hora(tw_fim, "Mandetta")

prop_pos(tw_fim, "Mandetta")

