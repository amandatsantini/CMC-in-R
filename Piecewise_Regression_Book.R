rm(list=ls())

setwd("/Users/sua_pasta_aqui")
list.files()

# Carrega os pacotes necessários
library(data.table)    # Manipulação de dados
library(segmented)     # Regressão linear segmentada
library(ggplot2)       # Visualização de resultados
library(vegan)
library(dplyr)
library(lme4)
library(gridExtra)

# Lê os dados
dados <- fread("nomedoseuarquivo.csv")
head(dados)  # Verifique as primeiras linhas para confirmar os nomes das colunas

# Supondo que os dados possuam as colunas "x" (variável independente) e "y" (variável dependente)
# Modelo linear
modelo <- lm(y ~ x, data = dados) #trocar "y" pelo nome da sua coluna com os resultados da tensão superficial, e trocar "x" pelo nome da coluna com as concentrações testadas.

# Regressão linear segmentada utilizando Concentração como variável de segmentação
seg_modelo <- segmented(modelo, seg.Z = ~x, psi = list(x = median(dados$x))) #trocar "x" pelo nome da coluna com as concentrações testadas.

summary(seg_modelo)
print(seg_modelo$psi)

# Exibe o(s) ponto(s) de quebra estimado(s)
print(seg_modelo$psi)

# Visualizando os resultados com ggplot2
# Acrescenta os valores ajustados pelo modelo segmentado aos dados
dados$fitted <- fitted(seg_modelo)

# Extrai o valor do breakpoint (CMC) do modelo segmentado
cmc_value <- seg_modelo$psi[,"Est."]

# Cria o gráfico usando as variáveis corretas e adiciona a legenda com a CMC 
plot1 <- ggplot(dados, aes(x = Concentracao, y = Tensao_superficial)) + #Em "x = Concentracao, y = Tensao_superficial", trocar pelos nomes originais da sua tabela.
  geom_point() +
  geom_line(aes(y = fitted), color = "blue", linewidth = 1) +
  geom_vline(xintercept = cmc_value, linetype = "dashed", color = "red") +
  # Adiciona uma anotação com o valor da CMC; ajuste a posição 'y' se necessário
  annotate("text", x = cmc_value, y = max(dados$y, na.rm = TRUE), #Em dados$y,trocar y pelo nome da coluna com os resultados da tensão superficial da sua tabela.
           label = paste("CMC =", round(cmc_value, 2)),
           hjust = -0.1, vjust = -0.5, color = "red") +
  theme_minimal() +
  labs(title = "Concentração Micelar Crítica",
       x = "Concentração",
       y = "Tensão Superficial")
pdf("grafico_piecewise.pdf", width = 8, height = 6)
print(plot1)
dev.off()


