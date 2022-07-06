# Problema de Negocio - Acessar uma URL com supostas mentiras ditas pelo 
# ex-presidente do USA Trump e coletar os dados e persisti-los num arquivo CSV.

# Pacotes R para Web Scraping
# RCurl
# httr
# XML
# rvest

# Desativar warnings
options(warn = -1)

# Pacote xml2 para processar os dados
library(xml2)

# Pacote rvest - util para quem nao conhece HTML e CSS
library(rvest)

# Demais pacotes para manipulacao de dados
library(stringr)
library(dplyr)
library(lubridate)
library(readr)


# Leitura da web page - Retorna um documento xml
webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
webpage 
class(webpage)


# Extraindo os registros
# Cada elemento na web page acima tem o seguinte formato em html:
# <span class="short-desc"><strong> DATE </strong> LIE <span class="short-truth"><a href="URL"> EXPLANATION </a></span></span>
?html_nodes
results <- webpage %>% html_nodes(".short-desc") #extrari as tag que apresenta a class informada ".short-desc"
results
length(results)

# Construindo o dataset
records <- vector("list", length = length(results))
records #criacao de lista vazia do mesmo tamanho dos nos 

dateF <- function(tags_html, x){
  res <- str_c(tags_html[x] %>%
                  html_nodes("strong") %>% # retorna o no com marcao "strong"
                  html_text(trim = TRUE), # retira o texto do no e remove os espacos antes e depois do texto
                ', 2017') #concatena o texto com removido do no com a string informada
  
  return(res)
}

lieF <- function(tags_html, x){
  res<- str_sub(xml_contents(tags_html[x])[2] %>% # retorna o 2 conteudo dos filhos da marcacao sinalozada(i)
            html_text(trim = TRUE),  # retira o texto do no e remove os espacos antes e depois do texto
          2, -2)
  
  return(res)
}

explanationF <- function(tags_html, x){
  res<- str_sub(tags_html[x] %>%
            html_nodes(".short-truth") %>% # retorna o no com marcao ".short-truth"
            html_text(trim = TRUE), # retira o texto do no e remove os espacos antes e depois do texto
          2, -2) # retorna a substring do vetor que comeca na posicao 2 e termina na posicao -2
  
  return(res)
}

urlF <- function(tags_html, x){
  res <- tags_html[x] %>% 
    html_nodes("a") %>% # retorna o no com marcao "a"
    html_attr("href") # retira do no o atributo "href"
  
  return(res)
}


?seq_along
for (i in seq_along(results)) {
  
  date <- dateF(results, i)
  
  lie <- lieF(results, i)
  
  explanation <- explanationF(results, i)
  
  url <- urlF(results, i)
  
  records[[i]] <- data_frame(date = date, lie = lie, explanation = explanation, url = url)
}

class(records)
length(records)

# Dataset final
df <- bind_rows(records) # combinar os 180 itens do records em linhas de um unico data.frame

class(df)
str(df)
View(df)

# Transformando o campo data para o formato Date em R
?mdy
df$date <- mdy(df$date)


# Exportando para CSV
write_csv(df, "mentiras_trump.csv")


# Lendo os dados
df <- read_csv("mentiras_trump.csv")
View(df)
