install.packages("stringi")
install.packages("knitr")
install.packages("janitor")
install.packages("kableExtra")
install.packages("shinydashboard")
install.packages("shiny")
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringi)
library(knitr)
library(janitor)
library(kableExtra)
library(shinydashboard)
library(shiny)

## Inserir Dados##
df_pop <- read.csv("C:\\Users\\marce\\OneDrive\\Documentos\\Work_Space_R\\tabela136.csv")
df_Anatel <- read_delim("C:\\Users\\marce\\OneDrive\\Documentos\\Work_Space_R\\Acessos_Banda_Larga_Fixa_2024.csv", delim = ";")

View(df_pop)
View(df_Anatel)

###Limpeza dos Dados###
#Retirar colunas que não serão necessarias##
df_Anatel <- df_Anatel %>%
  dplyr::select(-CNPJ, -`Meio de Acesso`, -`Tipo de Produto`)

# Remover acentos de todas as colunas
df_Anatel <- df_Anatel %>%
  mutate(across(everything(), ~ stri_trans_general(.x, "Latin-ASCII")))

df_pop <- df_pop %>%
  mutate(across(everything(), ~ stri_trans_general(.x, "Latin-ASCII")))

#Arrumar nome das colunas#
colnames(df_Anatel)[7] <- "municipio"
colnames(df_pop)[4] <- "municipio"

#Criar coluna Municipio + UF#
df_Anatel <- df_Anatel %>%
  dplyr::mutate(municipio_uf = paste(municipio, UF, sep = " - "))

df_pop <- df_pop %>%
  dplyr::mutate(municipio_uf = paste(municipio, UF, sep = " - "))


# Corrigir os nomes dos municípios para não ficar dados vazios
df_Anatel <- df_Anatel %>%
  mutate(municipio_uf = case_when(
    municipio_uf == "Santa Teresinha - BA" ~ "Santa Terezinha - BA",
    municipio_uf == "Florinia - SP" ~ "Florinea - SP",
    municipio_uf == "Sao Thome das Letras - MG" ~ "Sao Tome das Letras - MG",
    municipio_uf == "Muquem de Sao Francisco - BA" ~ "Muquem do Sao Francisco - BA",
    municipio_uf == "Amparo de Sao Francisco - SE" ~ "Amparo do Sao Francisco - SE",
    municipio_uf == "Grao Para - SC" ~ "Grao-Para - SC",
    municipio_uf == "Passa-Vinte - MG" ~ "Passa Vinte - MG",
    municipio_uf == "Augusto Severo - RN" ~ "Campo Grande - RN",
    municipio_uf == "Sao Luis do Paraitinga - SP" ~ "Sao Luiz do Paraitinga - SP",
    municipio_uf == "Olho-d'Agua do Borges - RN" ~ "Olho d'Agua do Borges - RN",
    municipio_uf == "Biritiba-Mirim - SP" ~ "Biritiba Mirim - SP",
    municipio_uf == "Dona Eusebia - MG" ~ "Dona Euzebia - MG",
    TRUE ~ municipio_uf 
  ))

##Unir a coluna populaçao##
dados <- df_Anatel %>%
  dplyr::left_join(df_pop %>% dplyr::select(municipio_uf, POPULAÇÃO), by = "municipio_uf")

#Arrumar nome das colunas#
colnames(dados)[10] <- "velocidade"
colnames(dados)[15] <- "populacao"

#Verificar dados vazios#
any(is.na(dados))
na_count <- colSums(is.na(dados))
na_count

#Transformar a colunas velocidade e população em numericas#
#Remover parênteses e texto
dados$populacao <- gsub("\\(.*?\\)", "", dados$populacao)

# Remover espaços em branco
dados$populacao <- trimws(dados$populacao)

# Remover os pontos
dados$populacao <- gsub("\\.", "", dados$populacao)

# Transformar a coluna em númerica#
dados$populacao <- as.numeric(dados$populacao)

#Transformar a "," em "." para classificar a coluna como numerica sem dar nenhum erro#
dados$velocidade <- gsub(",", ".", dados$velocidade)

# Transformar a coluna em númerica#
dados$velocidade <- as.numeric(dados$velocidade)

#Arredondar valor#
dados$velocidade <- round(dados$velocidade, 0)

#Transformar a "," em "." para classificar a coluna como numerica sem dar nenhum erro#
dados$Acessos <- gsub(",", ".", dados$Acessos)

# Transformar a coluna em númerica#
dados$Acessos <- as.numeric(dados$Acessos)

#Arredondar valor#
dados$velocidade <- round(dados$velocidade, 0)


##Anlise Starlink###

# Lista completa de municípios
municipios_completos <- unique(dados$municipio_uf)

# Lista de municípios atendidos pela Starlink
municipios_atendidos <- dados %>%
  filter(Empresa == "STARLINK BRAZIL SERVICOS DE INTERNET LTDA.") %>% 
  pull(municipio_uf) %>%
  unique()
print(municipios_atendidos)

#contando a quantidade de municipio Atendidos

quantidade_municipios_starlink <- length(municipios_atendidos)

quantidade_municipios_starlink


#Qual a velocidade média da Starlink nos municipios que ela atende?
velocidade_media_starlink <- dados %>%
  filter(Empresa == "STARLINK BRAZIL SERVICOS DE INTERNET LTDA.") %>%
  summarise(velocidade_media = mean(velocidade, na.rm = TRUE))
print(velocidade_media_starlink)


# Municípios que a Starlink não atende
municipios_nao_atendidos <- setdiff(municipios_completos, municipios_atendidos)
print(municipios_nao_atendidos)

# Tabela para demonstrar os municipios não atendidos
tabela <- municipios_nao_atendidos %>%
  head(536) %>%
  kable("html", col.names = c("Município"), 
        caption = "Municipios não atendidos") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(tabela)

#contando a quantidade de municipio Atendidos

quantidade_municipios_nao_atendidos <- length(municipios_nao_atendidos)

quantidade_municipios_nao_atendidos


#População municipios não atendidos
municipios_nao_atendidos_populacao <- dados %>%
  filter(municipio_uf %in% municipios_nao_atendidos) %>%
  select(municipio_uf, populacao, Empresa, `Faixa de Velocidade`, velocidade, Acessos, Tecnologia, `Tipo de Pessoa`)

print(municipios_nao_atendidos_populacao)
head(municipios_nao_atendidos_populacao)

# Ordenar os municípios não atendidos pela população
municipios_nao_atendidos_populacao <- municipios_nao_atendidos_populacao %>%
  arrange(desc(populacao))  
print(municipios_nao_atendidos_populacao)

# Criando a tabela para demonstrar os top 25 municipios
tabela <- municipios_nao_atendidos_populacao %>%
  select(municipio_uf, populacao) %>%
  distinct() %>%
  head(25) %>%
  kable("html", col.names = c("Município", "Populacao"), 
        caption = "Municipios não atendidos") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
print(tabela)

  
# Filtra os municípios com mais de 25 mil habitantes
municipios_acima_25k <- municipios_nao_atendidos_populacao  %>%
  filter(populacao > 25000)

# Criando a tabela
tabela_municipios_acima_25k <- municipios_acima_25k %>%
  select(municipio_uf, populacao) %>%
  distinct() %>%
  head(10) %>%
  kable("html", col.names = c("Município", "População"),
        caption = "Municípios com mais de 25 mil habitantes") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  
  print(tabela_municipios_acima_25k)


# Agrupar os dados por tipo de pessoa e calcular o total de acessos
acessos_por_municipio_tipo <- municipios_acima_25k %>%
  group_by(municipio_uf, `Tipo de Pessoa`) %>%
  summarise(total_acessos = sum(Acessos, na.rm = TRUE),
            .groups = 'drop')
print(acessos_por_municipio_tipo)


# Conta a quantidade de empresas atuando e soma os acessos
empresas_por_municipio <- municipios_acima_25k %>%
  group_by(municipio_uf, populacao) %>%
  summarise(quantidade_empresas = n_distinct(Empresa),
            total_acessos = sum(Acessos, na.rm = TRUE),
            .groups = "drop")

# Criar o gráfico total de acessos
ggplot(empresas_por_municipio, aes(x = total_acessos, y = reorder(municipio_uf, total_acessos))) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  geom_text(aes(label = total_acessos), 
            hjust = 0.5,  
            vjust = 0.5,  
            color = "black") +  
  labs(title = "Total de Acessos por Município com Mais de 25 mil Habitantes",
       x = "Total de Acessos",
       y = "Município") +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 0))

# Criar o gráfico para demonstrar total de empresas
ggplot(empresas_por_municipio, aes(x = reorder(municipio_uf, -quantidade_empresas), y = quantidade_empresas)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = quantidade_empresas), 
            vjust = -0.2,  
            color = "black") +  
  labs(title = "Quantidade de Empresas por Município com Mais de 25 mil Habitantes",
       x = "Município",
       y = "Quantidade de Empresas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


# Analisando as principais empresas por município
principais_empresas <- municipios_acima_25k %>%
  group_by(Empresa) %>%
  summarise(total_acessos = sum(Acessos, na.rm = TRUE),
            total_municipios = n_distinct(municipio_uf),
            .groups = "drop") %>%
  arrange(desc(total_acessos)) %>%


# Gráfico da quantidade total de acessos por empresa
ggplot(principais_empresas, aes(x = reorder(Empresa, total_acessos), y = total_acessos)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() + 
  labs(title = "Principais Concorrentes",
       x = "Empresa",
       y = "Total de Acessos") +
  theme_minimal()


# Calcular o total de acessos por município
total_acessos_por_municipio <- municipios_acima_25k %>%
  group_by(municipio_uf) %>%
  summarise(total_acessos_municipio = sum(Acessos, na.rm = TRUE), .groups = 'drop')

# Calcular o total de acessos por empresa
acessos_por_empresa <- municipios_acima_25k %>%
  group_by(Empresa, municipio_uf) %>%
  summarise(total_acessos_empresa = sum(Acessos, na.rm = TRUE), .groups = 'drop')

# Filtrar as top 20 empresas
top_20_empresas <- acessos_por_empresa %>%
  group_by(Empresa) %>%
  summarise(total_acessos = sum(total_acessos_empresa, na.rm = TRUE), .groups = 'drop') %>%
  top_n(20, wt = total_acessos)

# Juntar os dados e calcular o share
share_top_20 <- acessos_por_empresa %>%
  filter(Empresa %in% top_20_empresas$Empresa) %>%
  left_join(total_acessos_por_municipio, by = "municipio_uf") %>%
  mutate(share_empresa = (total_acessos_empresa / total_acessos_municipio) * 100)

# Criar o gráfico do share de mercado
ggplot(share_top_20, aes(x = reorder(municipio_uf, -total_acessos_empresa), 
                         y = share_empresa, fill = reorder(Empresa, -total_acessos_empresa))) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(title = "Share de Mercado das Top 20 Empresas por Município",
       x = "Município",
       y = "Share (%)") +
  theme_minimal()

# Calcular a demanda reprimida
demanda_reprimida <- municipios_acima_25k %>%
  group_by(municipio_uf, populacao) %>%
  summarise(total_acessos = sum(Acessos, na.rm = TRUE), .groups = "drop") %>%
  mutate(demanda_reprimida = populacao - total_acessos) %>%
  filter(demanda_reprimida > 0) 
print(demanda_reprimida)

# Gráfico da demanda reprimida

ggplot(demanda_reprimida, aes(x = reorder(municipio_uf, -demanda_reprimida), y = demanda_reprimida)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +  
  geom_text(aes(label = demanda_reprimida), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "black", check_overlap = TRUE) + 
  labs(title = "Demanda Reprimida por Município",
       x = "Município",
       y = "Demanda Reprimida") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
