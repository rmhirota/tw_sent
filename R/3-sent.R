
#' Calcula pontuação de sentimento de tweets
#'
#' @param tw base limpa de tweets criada usando tidy_tweets
#'
#' @return df com tweets e valores de sentimento
#'
#' @export
tw_sent_base <- function(tw) {
  # Léxicos
  op30 <- lexiconPT::oplexicon_v3.0 %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(polarity = mean(polarity))
  sent <- lexiconPT::sentiLex_lem_PT02 %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(polarity = mean(polarity))

  # O léxico não contempla acentos, portanto, vamos tirar caracteres especiais
  tw <- tw %>%
    dplyr::mutate(text = iconv(text, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
           text = stringr::str_to_lower(stringr::str_replace_all(text, "[//^~']+", "")))

  # Separando palavras
  tweets_unnested <- tw %>% tidytext::unnest_tokens(term, text)

  # Correspondência com léxicos
  tweets_unnested <- tweets_unnested %>%
    dplyr::inner_join(op30, by = "term") %>%
    dplyr::inner_join(sent %>% dplyr::select(term, lex_polarity = polarity), by = "term") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      tw_sentiment_op = sum(polarity),
      tw_sentiment_lex = sum(lex_polarity),
      n_words = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      most_neg = min(tw_sentiment_lex, tw_sentiment_op),
      most_pos = max(tw_sentiment_lex, tw_sentiment_op)
    )

  cat(sprintf("%s dos tweets categorizados", scales::percent(nrow(tweets_unnested)/nrow(tw))))

  tweets_unnested <- tweets_unnested %>% filter(abs(tw_sentiment_op - tw_sentiment_lex) <= 5)

  return(tweets_unnested)
}

#' Lista tweets mais positivos ou mais negativos
#'
#' @param tweets base limpa de tweets criada usando tidy_tweets
#' @param tw_sent base com valores de sentimento obtida por tw_sent_base
#' @param sent "positivo" ou "negativo"
#'
#' @return df com 10 tweets mais positivos/negativos
#'
#' @export
tw_pos_neg <- function(tweets, tw_sent, sent = "positivo") {
  most_pos <- tw_sent %>%
    dplyr::arrange(desc(most_pos)) %>%
    head(10) %>%
    dplyr::pull(id)
  most_neg <- tw_sent %>%
    dplyr::arrange(most_neg) %>%
    head(10) %>%
    dplyr::pull(id)
  if (sent == "positivo") {sent <- most_pos} else {sent <- most_neg}
  tweets %>%
    dplyr::filter(id %in% sent)
}



#' Juntan informações de sentimento aos dados dos tweets
#'
#' @param tweets base limpa de tweets criada usando tidy_tweets
#' @param tw_sent base com valores de sentimento gerada por tw_sent_base
#' @param path_arq path e nome do arquivo rds que será salvo
#'
#' @return
#'
#' @export
agrupa_tw_sent <- function(tweets, tw_sent, path_arq = "data/tw_fim") {
  tweets %>%
  dplyr::inner_join(
    tw_sent %>% dplyr::select(id, sentiment = tw_sentiment_op),
    by = "id") %>%
  readr::write_rds(sprintf("%s.rds", path_arq))
}


#' Cria gráfico com média de sentimento por hora
#'
#' @param tweets_fim base agrupada de tweets criada usando agrupa_tw_sent
#' @param tema string indicando palavrs(s)-chave utilizada(s) na coleta dos tweets
#'
#' @return gráfico ggplot com a média de sentimento por hora
#'
#' @export
sent_hora <- function(tweets_fim, tema) {
  tweets_fim %>%
    dplyr::mutate(datahora = lubridate::floor_date(created_at, "hour")) %>%
    dplyr::group_by(datahora) %>%
    dplyr::summarise(media = mean(sentiment)) %>%
    ggplot2::ggplot(ggplot2::aes(x = datahora, y = media)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
    ggplot2::geom_hline(yintercept = 0, color= "red") +
    ggplot2::scale_x_datetime(date_breaks = "1 day", labels = scales::label_date_short()) +
    ggplot2::labs(title = sprintf("Média do sentimento por hora de tweets com referência a %s", tema)) +
    ggplot2::theme_minimal()
}


#' Cria gráfico com total de tweets positivos e negativos por hora
#'
#' @param tweets_fim base agrupada de tweets criada usando agrupa_tw_sent
#' @param tema string indicando palavrs(s)-chave utilizada(s) na coleta dos tweets
#'
#' @return gráfico ggplot com a quantidade de tweets positivos e tweets negativos por hora
#'
#' @export
pos_neg_hora <- function(tweets_fim, tema) {
  tweets_fim %>%
    dplyr::mutate(datahora = lubridate::floor_date(created_at, "hour")) %>%
    dplyr::mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
    dplyr::group_by(datahora, cat) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(!is.na(cat)) %>%
    ggplot2::ggplot(ggplot2::aes(x = datahora, y = n, color = cat)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(date_breaks = "1 day", labels = scales::label_date_short()) +
    ggplot2::labs(title = "Quantidade de tweets positivos e negativos por hora",
                  subtitle = sprintf("Tweets com alguma referência a %s", tema)) +
    ggplot2::theme_minimal()
}


#
#' Cria gráfico com proporção de tweets positivos sobre quantidade de tweets negativos
#'
#' @param tweets_fim base agrupada de tweets criada usando agrupa_tw_sent
#' @param tema string indicando palavrs(s)-chave utilizada(s) na coleta dos tweets
#'
#' @return gráfico ggplot com a proporção de tweets positivos sobre quantidade de tweets negativos por hora
#'
#' @export
prop_pos <- function(tweets_fim, tema) {
  tweets_fim %>%
    dplyr::mutate(datahora = lubridate::floor_date(created_at, "hour")) %>%
    dplyr::mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
    dplyr::group_by(datahora, cat) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    tidyr::spread(cat, n) %>%
    dplyr::mutate(p = positivo/negativo) %>%
    ggplot2::ggplot(ggplot2::aes(x = datahora, y = p)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
    ggplot2::geom_smooth() +
    ggplot2::scale_x_datetime(date_breaks = "1 day", labels = scales::label_date_short()) +
    ggplot2::labs(title = "Proporção de tweets positivos e tweets negativos",
                  subtitle = sprintf("Com alguma referência a %s", tema)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}



