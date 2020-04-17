# twsent

`twsent` é um pacote de utilizado no [Núcleo](https://nucleo.jor.br/)/[Volt Data Lab](https://www.voltdata.info/) para análise de sentimento de tweets coletados com `Rtweet`.
O processo de coleta está descrito [aqui](https://gist.github.com/voltdatalab/a342c1179284deafa5c508dad33373f5).

## Instação
O pacote pode ser instalado diretamente por este repositório.
```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rmhirota/twsent")

library(twsent)
```

## Funções
`twsent` tem 10 funções, das quais 4 são de organização e exploração da base e 6 estão relacionadas com a análise de sentimentos em si.

* `tidy_tweets` organiza o arquivo .csv coletado e salva a nova base em um objeto .rds
* `qtd_tweet_tempo` cria um gráfico `ggplot` com o número total de tweets por hora
* `tw_hashtags` retorna uma tabela com as 50 hashtags mais compartilhadas
* `tw_urls` retorna uma tabela com as 50 URLs mais comparilhadas
* `tw_sent_base` e `agrupa_tw_sent` são funções auxiliares para criar a base final com valor de sentimento por tweet
* `tw_pos_neg` retorna os dados dos 10 tweets mais positivos/negativos
* `sent_hora` cria um gráfico `ggplot` com a média do valor de sentimento por hora
* `pos_neg_hora` cria um gráfico `ggplot` com a quantidade total de tweets positivos e tweets negativos por hora
* `prop_pos` cria um gráfico `ggplot` com a proporção entre tweets positivos e tweets negativos por hora
