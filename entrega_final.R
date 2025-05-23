# ENTREGA FINAL: ECONOLAB 
# GRUPO: AMANDA RIBEIRO, GUSTAVO MENDES, GUILHERME ROCCATO, LU?S FELIPE YAMASHITAFUJI E PEDRO DUTRA


# REPLICANDO A METODOLOGIA DO PAPER ---------
# Site de referência: https://www.bcb.gov.br/noticiablogbc/27/noticia

# PER?ODO: 2004 - 2023

install.packages("lmtest")
install.packages("sandwich")
install.packages("stringr")


# Carregar pacotes
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(stringr)
library(mFilter)

# Ler cada planilha

df_salario_minimo <- read_excel("salario_minimo.xlsx") #dado mensal

df_hiato_produto <- read_excel("hiato_produto.xlsx", skip = 8)
 
df_ipca_servicos <- read_excel("ipca_servicos.xlsx") #dado mensal 

# df_2000_2010 <- read_excel("expectativa_inflacao_anual_2000_2010.xls", skip = 1) #dado anual 
# df_2011_2021 <- read_excel("expectativa_inflacao_anual_2011_2021.xls", skip = 1) #dado anual
# df_2022_2025 <- read_excel("expectativa_inflacao_anual_2022_2025.xls", skip = 1) #dado anual 
# 
# # Verifique os nomes das colunas e padronize, se necess?rio
# names(df_2000_2010)
# names(df_2011_2021)
# names(df_2022_2025)
# 
# Unir os tr?s dataframes
# df_unido <- bind_rows(df_2000_2010, df_2011_2021, df_2022_2025)
# 
# # resumo da estrutura dos nosso dados
# # glimpse(df_unido)

# DATAFRAME DE REAJUSTE ANUAL DO SALARIO MINIMO ----------
df_reajuste_anual <- df_salario_minimo %>%
  # Renomear colunas
  rename(data = 1, salario = 2) %>%
  # Converter salÃ¡rio para numÃ©rico
  mutate(salario = as.numeric(salario)) %>%
  # Calcular variaÃ§Ã£o para todos os registros
  mutate(variacao = c(NA, salario[2:n()] / salario[1:(n()-1)] * 100 - 100)) %>%
  # Arredondar para duas casas decimais
  mutate(variacao = round(variacao, 2)) %>%
  # Filtrar anos entre 2004 e 2023
  filter(year(data) >= 2004 & year(data) <= 2023) %>%
  # Filtrar apenas onde hÃ¡ variaÃ§Ã£o
  filter(!is.na(variacao) & variacao != 0) %>%
  # Agrupar por ano e pegar o primeiro mÃªs com variaÃ§Ã£o
  group_by(year(data)) %>%
  slice_min(order_by = month(data), n = 1) %>%
  ungroup() %>%
  # Selecionar colunas finais
  select(data, salario, variacao)

#tail(df_reajuste_anual)


# DATAFRAME DE INFLACAO DE SERVICOS ACUMULADA NOS ULTIMOS 12 MESES -------------
# Preparar df_ipca_servicos2: converter datas e valores para formatos adequados
df_ipca_servicos2 <- df_ipca_servicos %>%
  # Renomear colunas para nomes mais simples
  rename(Data = 1, ipca = 2) %>%
  # Filtrar apenas linhas com Data contendo nÃºmeros inteiros (nÃºmeros seriais Excel)
  filter(grepl("^[0-9]+$", Data)) %>%
  # Converter Data para numÃ©rico (serial Excel)
  mutate(Data = as.numeric(Data)) %>%
  # Converter nÃºmero serial Excel para objeto Date do R
  mutate(Data = as.Date(Data, origin = "1899-12-30")) %>%
  # Converter ipca para numÃ©rico
  mutate(ipca = as.numeric(ipca))

#head(df_ipca_servicos2)

# Calcular IPCA acumulado nos 12 meses anteriores para cada data de reajuste
df_servicos12m_anteriores <- df_reajuste_anual %>%
  mutate(
    servicos12m = map_dbl(data, function(d) {
      # Definir janela: do mÃªs 12 meses antes atÃ© o mÃªs imediatamente anterior a d
      inicio <- d %m-% months(12)
      fim <- d %m-% months(1)
      
      # Filtrar dados do IPCA dentro da janela temporal e calcular acumulado percentual
      df_ipca_servicos2 %>%
        filter(Data >= inicio, Data <= fim) %>%
        summarise(acum = (prod(1 + ipca / 100) - 1) * 100) %>%
        pull(acum)
    })
  ) %>%
  # Selecionar colunas finais para o output
  select(data, servicos12m) %>%
  # Arredondar resultado para duas casas decimais
  mutate(servicos12m = round(servicos12m, 2))

#tail(df_servicos12m_anteriores)


# DATAFRAME DE INFLACAO DE SERVICOS ACUMULADA NOS 12 MESES FUTUROS -------------
df_servicos12m_posteriores <- df_reajuste_anual %>%
  mutate(
    servicos12m_adiante = map_dbl(data, function(d) {
      # Janela: de d atÃ© (d + 11 meses) â€” 12 meses no total
      inicio <- d
      fim <- d %m+% months(11)
      
      # Filtrar dados do IPCA dentro da janela temporal e calcular acumulado percentual
      df_ipca_servicos2 %>%
        filter(Data >= inicio, Data <= fim) %>%
        summarise(acum = (prod(1 + ipca / 100) - 1) * 100) %>%
        pull(acum)
    })
  ) %>%
  # Selecionar colunas finais
  select(data, servicos12m_adiante) %>%
  # Arredondar resultado para duas casas decimais
  mutate(servicos12m_adiante = round(servicos12m_adiante, 2))

#tail(df_servicos12m_posteriores)


# DATAFRAME EXPECTATIVAS DE INFLACAO ---------
valores_expectativa <- c(
  5.52, 5.69, 4.40, 3.84, 4.44, 4.91, 4.36, 5.60,
  5.45, 5.42, 6.16, 6.60, 7.11, 4.79, 4.04, 3.34,
  4.20, 4.12, 5.91, 5.45
)

df_expectativas <- df_reajuste_anual %>%
  arrange(data) %>%
  mutate(expectativa = valores_expectativa) %>%
  select(data, expectativa)

#tail(df_expectativas)


# DATAFRAME DE HIATO DO PRODUTO ----------
# Preparar df_hiato_produto: renomear colunas e converter datas
df_hiato_produto2 <- df_hiato_produto %>%
  rename(data_hiato = Trimestre, hiato = `Hiato`) %>%
  mutate(data_hiato = as.Date(data_hiato))

# Combinar datas de reajuste com datas de hiato
df_hiato <- df_reajuste_anual %>%
  select(data) %>%
  mutate(key = 1) %>%
  inner_join(
    df_hiato_produto2 %>% mutate(key = 1),
    by = "key",
    relationship = "many-to-many"
  ) %>%
  select(-key) %>%
  # Filtrar apenas datas anteriores
  filter(data_hiato < data) %>%
  # Para cada data, pegar o hiato mais recente anterior
  group_by(data) %>%
  filter(data_hiato == max(data_hiato)) %>%
  ungroup() %>%
  select(data, hiato)

#tail(df_hiato)


# DATAFRAME ESPELHO DO RELATORIO DO BANCO CENTRAL ---------
df_relatorio_bc <- df_reajuste_anual %>%
  left_join(df_servicos12m_anteriores, by = "data") %>%
  left_join(df_servicos12m_posteriores, by = "data") %>%
  left_join(df_expectativas, by = "data") %>%
  left_join(df_hiato, by = "data")

print(df_relatorio_bc)


# ESTIMANDO MODELO --------
modelo <- lm(
  servicos12m_adiante ~ variacao + servicos12m + expectativa + hiato,
  data = df_relatorio_bc
  )

coeftest(modelo, vcov = NeweyWest(modelo, lag = 1, prewhite = FALSE))


# GRAFICO --------
# Preparar dados
df_plot <- df_relatorio_bc %>%
  mutate(ano = year(data))

# Ajuste da regressÃ£o simples
fit <- lm(servicos12m_adiante ~ variacao, data = df_plot)
coef_int   <- coef(fit)[1]
coef_slope <- coef(fit)[2]
r2         <- summary(fit)$r.squared
eq_label   <- paste0("y = ", round(coef_slope, 1),
                     " x + ", round(coef_int,   1),
                     "\nR² = ", round(r2,        2))

# Valores mÃ­nimos e mÃ¡ximos para posicionar o label
x_min <- min(df_plot$variacao)
x_max <- max(df_plot$variacao)
y_min <- min(df_plot$servicos12m_adiante)
y_max <- max(df_plot$servicos12m_adiante)

# ConstruÃ§Ã£o do grÃ¡fico
ggplot(df_plot, aes(x = variacao, y = servicos12m_adiante)) +
  
  # Pontos e rÃ³tulos
  geom_point(size = 3, color = "#2C3E50") +
  geom_text(aes(label = ano), vjust = -1.2, size = 3.5, color = "#34495E") +
  
  # Linha de regressÃ£o pontilhada
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "#E74C3C") +
  
  # AnotaÃ§Ã£o da equaÃ§Ã£o e RÂ²
  annotate(
    "text",
    x    = x_min + 0.001 * (x_max - x_min),
    y    = y_max - 0.05 * (y_max - y_min),
    label= eq_label,
    hjust= 0,
    size = 4,
    color= "#E74C3C"
  ) +
  
  # Escalas com quebras principais e secundÃ¡rias
  scale_x_continuous(
    breaks      = seq(floor(x_min), ceiling(x_max), by = 1),
    minor_breaks= seq(floor(x_min), ceiling(x_max), by = 1),
    expand      = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    breaks      = seq(floor(y_min), ceiling(y_max), by = 1),
    minor_breaks= seq(floor(y_min), ceiling(y_max), by = 1),
    expand      = expansion(mult = c(0.1, 0.1))
  ) +
  
  # TÃ­tulos e rÃ³tulos
  labs(
    title = "Variação do Salário Mínimo em t vs Inflação de Serviços de t a t+11 (%)",
    x     = "Variação do Salário Mínimo em t (%)",
    y     = "Inflação de Salário de t a t+11 (%)"
  ) +
  
  # Tema com grades
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    panel.grid.major       = element_line(color = "gray80", size = 0.5),
    panel.grid.minor       = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y     = element_line(linetype = "dashed"),
    panel.grid.minor.x     = element_line(linetype = "dashed"),
    panel.grid.major.x     = element_line(linetype = "solid"),
    panel.grid.major.y     = element_line(linetype = "solid"),
    axis.ticks             = element_line(color = "gray50"),
    plot.title             = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title             = element_text(face = "bold", size = 14)
  )



# CONTORNOS INDIVIDUAIS (ESTRESSANDO O MODELO) ---------


# INCC (ÍNDICE NACIONAL DE CUSTO DA CONSTRUÇÃO) -----

#Importando a base
df_incc <- read_excel("incc-di.xls") #dados anuais (1945-2024)
#Removendo linhas e renomeando colunas
df_incc<- df_incc %>%
  # Renomear colunas
  rename(data = 1, incc_di = 2)
#Filtrando o período desejado (dados filtrado entre 2000 e 2024)
df_incc_filtrado <- df_incc %>%
  filter(data >= 2004 & data <= 2024)
#Arrendondando
df_incc_filtrado <- df_incc_filtrado %>%
  mutate(incc_di = round(incc_di, 2))

#Adicionando uma nova métrica ao relatório BC (ou fazendo um novo relatório)

#Renomeando a coluna para facilitar o join
df_incc_filtrado <- df_incc_filtrado %>%
  rename(ano = 1) %>% 
  mutate(ano = as.numeric(ano))

# Adicionando a coluna ano para que seja possível fazer o join
df_relatorio_bc_modificado <- df_relatorio_bc %>%
  mutate(data = ymd(data),
         ano = year(data))     

#Join feito
df_relatorio_bc_modificado <- df_relatorio_bc_modificado %>% 
  left_join(df_incc_filtrado, by = "ano")

#Reordenando as colunas
df_relatorio_bc_modificado <- df_relatorio_bc_modificado %>%
  select(data, ano, everything())

print(df_relatorio_bc_modificado)

#Modelo com incc

modelo <- lm(
  servicos12m_adiante ~ variacao + incc_di + hiato,
  data = df_relatorio_bc_modificado
)

coeftest(modelo, vcov = NeweyWest(modelo, lag = 1, prewhite = FALSE))



# IPS (ÍNIDICE DE PREÇOS DE SERVIÇOS) [FERCOMERCIO-SP] -----

#fonte (https://www.fecomercio.com.br/pesquisas/indice/ips)

# Esse dados começaram a ser disponibilizados somente a partir de 2011

# Preparação do excel

df_ips <- read_excel("ips.xlsx") #mensal

# Tranformando a primeira coluna em data e arredondando a segunda coluna para 2 casas decimais
df_ips <- df_ips %>%
  mutate(data = as.Date(data)) %>% 
  mutate(ips = round(ips,2))

#Acumulando anualmente os dados mensais
df_ips_anual <- df_ips %>%
  mutate(ano = year(data)) %>% 
  group_by(ano) %>% 
  summarise(ips_anual = (prod(1 + ips/100) - 1)*100) %>% 
  mutate(ips_anual = round(ips_anual,2))

# Join feito    
df_relatorio_bc_modificado <- df_relatorio_bc_modificado %>% 
  left_join(df_ips_anual, by = "ano")

# Os dados de ips começam a ser medido de 2011 até 2025 por isso fiz o novo corte
df_relatorio_bc_modificado_ips <- df_relatorio_bc_modificado %>%
  filter(ano >= 2011 & ano <= 2024)

print(df_relatorio_bc_modificado_ips)


# Modelo com ips 

modelo <- lm(
  servicos12m_adiante ~ variacao + ips_anual + hiato,
  data = df_relatorio_bc_modificado_ips
)

coeftest(modelo, vcov = NeweyWest(modelo, lag = 1, prewhite = FALSE))




# Hiato do produto (utilizando PIB Industria) ---------


#Importando base (excel com os dados) - Período entre 1947 e 2024 
#Dado anual em milhões de reais
#Fonte: IBGE

pib <- read_excel("pib_industria.xls")  

# Tratamentos iniciais: renomar as colunas e limitar o período ao analisado no estudo
pib <- pib %>% 
  rename(ano=1,pib_industria =2) %>% 
  filter(ano >= 2000 & ano <= 2024 )

# vetor do PIB 
pib_valores <- pib$pib_industria

# filtro HP (lambda = 100)
hp <- hpfilter(pib_valores, freq = 100, type = "lambda")

# PIB potencial (tendência)
pib_potencial <- hp$trend

# PIB ciclo (desvio do potencial)
pib_ciclo <- hp$cycle

hiato <- (pib_valores - pib_potencial) / pib_potencial * 100

hiato_industria <- pib %>%
  mutate(PIB_Potencial = pib_potencial,
         Hiato = hiato) %>% 
  mutate(ano = as.numeric(ano))

print(hiato_industria)

#Relatório incorporando a nova coluna

df_relatorio_bc_hiato_industria <- df_relatorio_bc %>%
  mutate(data = ymd(data),
         ano = year(data)) 

#Join feito
df_relatorio_bc_hiato_industria <- df_relatorio_bc_hiato_industria %>% 
  left_join(hiato_industria, by = "ano") %>% 
  select(data, ano, everything()) 
  


# ESTIMANDO MODELO
modelo <- lm(
  servicos12m_adiante ~ variacao + servicos12m + expectativa + Hiato[,1],
  data = df_relatorio_bc_hiato_industria
)

coeftest(modelo, vcov = NeweyWest(modelo, lag = 1, prewhite = FALSE))




#CORRELAÇÃO ENTRE INFLAÇÃO-SP E INFLAÇÃO BR -------
#Apoio para o índice do IPS Fecomércio


#Inflação brasileira 

ipca_br <- read_excel("inflacao_br.xls") %>%
  rename( data = 1, var_ipca = 2) %>% 
  filter(data>=2000 & data<=2024) %>% 
  mutate(var_ipca = round(var_ipca,2))
  

# IPC (Inflação do município de São Paulo)
ipc_fipe <- read_excel("ipc_sp.xls") %>%
  rename( data = 1, ipc = 2) %>% 
  filter(data>=2000 & data<=2024) %>% 
  mutate(ipc = round(ipc,2))

# Join das duas bases
tabela_ipc_ipca <- left_join(ipca_br, ipc_fipe, by = "data")


# Correlação
correlacao <- cor(tabela_ipc_ipca$var_ipca, tabela_ipc_ipca$ipc)
print(paste("Correlação: ", round(correlacao, 3)))

# Gráfico de dispersão com linha de tendência
ggplot(tabela_ipc_ipca, aes(x = var_ipca, y = ipc)) +
  geom_point(color = "deeppink", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Correlação entre IPCA e IPC-Fipe",
    subtitle = paste("Correlação de Pearson =", round(correlacao, 3)),
    x = "Inflação Brasil (IPCA)",
    y = "Inflação São Paulo (IPC-Fipe)"
  ) +
  theme_minimal()













