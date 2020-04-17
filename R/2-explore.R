library(magrittr)


# Quantidade de tweets por tempo

qtd_tw_tempo <- function(tw, tema) {
  data_ini <- format(min(tw$created_at), "%d/%m/%Y")
  data_fim <- format(max(tw$created_at), "%d/%m/%Y")
  tw %>%
    dplyr::mutate(dia_hora = lubridate::floor_date(created_at, "hours")) %>%
    dplyr::count(dia_hora) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = dia_hora, y = n)) +
    ggplot2::labs(title = sprintf("Quantidade de tuítes por hora com referência a %s", tema),
         subtitle = sprintf("De %s a %s", data_ini, data_fim),
         x = "", y = "") +
    ggplot2::theme_minimal()
}

# Hashtags mais compartilhadas

tw_hashtags <- function(tw) {
  tw %>%
    dplyr::filter(!is.na(hashtags)) %>%
    dplyr::mutate(hash_sep = stringr::str_split(hashtags, ", ")) %>%
    tidyr::unnest(hash_sep) %>%
    dplyr::count(hash_sep, sort = T) %>% head(50)
}


# urls mais compartilhadas
tw_urls <- function(tw) {
  tw %>%
  dplyr::filter(!is.na(urls_url)) %>%
  dplyr::mutate(url_sep = stringr::str_split(urls_url, ", ")) %>%
  tidyr::unnest(url_sep) %>%
  dplyr::count(url_sep, sort = T) %>% head(50)
}





