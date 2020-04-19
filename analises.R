library(tidyverse)

# fs::dir_create("data-raw") # Guardar o csv aqui
# fs::dir_create("data")

# tidy_tweets("data-raw/apenas_mandetta.csv", "data")
tweets <- readr::read_rds("data/mandetta_demit_tidy.rds")

# tw_sent <- tw_sent_base(tweets)
# readr::write_rds(tw_sent, "data/tw_sent_bolsonaro.rds")
tw_sent <- readr::read_rds("data/tw_sent.rds")

agrupa_tw_sent(tweets, tw_sent, "data/tw_fim")
tw_fim <- readr::read_rds("data/tw_fim.rds")


qtd_tw_tempo(tweets, "Mandetta")
tw_hashtags(tweets)
tw_urls(tweets)

# 10 tweets mais positivos e negativos
tw_pos_neg(tweets, tw_sent, "positivo") %>%
  with(status_url) %>% paste(sep = "\n") %>% clipr::write_clip()
tw_pos_neg(tweets, tw_sent, "negativo") %>%
  with(status_url) %>% paste(sep = "\n") %>% clipr::write_clip()


sent_hora(tw_fim, "Mandetta")
pos_neg_hora(tw_fim, "Mandetta")
prop_pos(tw_fim, "Mandetta")


# Gráfico de quantidade de twets positivos e negativos, por minuto nos dias 16 e 17 de abril
pos_neg_min <- tw_fim %>%
  dplyr::filter(created_at > "2020-04-16 15:30:00") %>%
  dplyr::mutate(datahora = lubridate::floor_date(created_at, "minute")) %>%
  dplyr::mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
  dplyr::group_by(datahora, cat) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(!is.na(cat)) %>%
  ggplot2::ggplot(ggplot2::aes(x = datahora, y = n, color = cat)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_datetime(date_breaks = "1 hour", labels = scales::label_date_short()) +
  ggplot2::labs(title = "Quantidade de tweets positivos e negativos por minuto",
                subtitle = sprintf("Tweets com alguma referência a %s", "Mandetta")) +
  ggplot2::theme_minimal()


plotly::ggplotly(pos_neg_min)


pos_neg_min

tw_fim %>%
  dplyr::filter(created_at >= "2020-04-16 12:00:00") %>%
  dplyr::mutate(datahora = lubridate::floor_date(created_at, "minute")) %>%
  dplyr::mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
  dplyr::group_by(datahora, cat) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(!is.na(cat)) %>%
  clipr::write_clip()




