# ENTREGA FINAL: ECONOLAB 
# GRUPO: AMANDA RIBEIRO, GUSTAVO MENDES, GUILHERME ROCCATO, LUÍS FELIPE YAMASHITAFUJI E PEDRO DUTRA


# REPLICANDO A METODOLOGIA DO PAPER

# PERÍODO: 2004 - 2023

# EQUAÇÃO LINEAR, SEMELHANTE A UMA CURVA DE PHILLIPS 


# Inicio do código


# Carregar pacotes
library(readxl)
library(dplyr)
library(lubridate)


# Ler cada planilha
df_2000_2010 <- read_excel("expectativa_inflacao_anual_2000_2010.xls", skip = 1) #dado anual 
df_2011_2021 <- read_excel("expectativa_inflacao_anual_2011_2021.xls", skip = 1) #dado anual
df_2022_2025 <- read_excel("expectativa_inflacao_anual_2022_2025.xls", skip = 1) #dado anual

df_salario_minimo <- read_excel("salario_minimo.xlsx") #dado mensal

df_hiato_produto <- read_excel("hiato_produto.xlsx")

df_ipca_servicos <- read_excel("ipca_servicos.xlsx") #dado mensal 
names(df_ipca_servicos)[names(df_ipca_servicos) == "10844 - Índice Nacional de Preços ao Consumidor - Amplo (IPCA) - Serviços - Var. % mensal"] <- "ipca_servicos"  # renomeando a coluna do arquivo ipca servicos


# Verifique os nomes das colunas e padronize, se necessário
names(df_2000_2010)
names(df_2011_2021)
names(df_2022_2025)

# Unir os três dataframes
df_unido <- bind_rows(df_2000_2010, df_2011_2021, df_2022_2025)

# resumo da estrutura dos nosso dados
# glimpse(df_unido)


# filtrando o primeiro dia de cada mês
df_filtrado <- df_unido %>%
  mutate(Data = as.Date(Data),              # Converter para Date
         Ano = year(Data), 
         Mes = month(Data)) %>%             # Extrair ano e mês
  group_by(Ano, Mes) %>%
  slice_min(order_by = Data, n = 1) %>%     # Pega a primeira data do mês
  ungroup()


# calculando o reajuste salarial

reajuste_anual <- df_salario_minimo %>%
  filter(month(data) == 1) %>%             # filtra só janeiro
  arrange(data) %>%                        # ordena por data
  mutate(
    ano = year(data),
    reajuste_percentual = (`salario minimo` / lag(`salario minimo`) - 1) * 100
  )


# comentário teste


