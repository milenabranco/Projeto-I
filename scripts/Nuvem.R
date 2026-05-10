library(openxlsx)
library(gtsummary)
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",") # formatação em português (vírgula pra decimais e ponto para milhares)



library(tidyverse)
# CARREGANDO DADOS ----
Dados = read.xlsx("Nutricao.xlsx")
#selecionando dados para nuvem 



library(openxlsx)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(ggwordcloud)
library(forcats) # caso queira manipular fatores
library(purrr)

# 2) Carregar dados
Dados <- read.xlsx("Nutricao.xlsx")

# 3) Garantir coluna de agrupamento e, se houver mais de 4 grupos, reduzir para 4
if (!"Bebidas_Alcoolicas" %in% names(Dados)) {
  stop("Coluna Bebidas_Alcoolicas não encontrada no dataset.")
}

# Se houver mais de 4 níveis, pegamos os 4 primeiros (ou ajuste conforme necessidade)
levels_atuais <- unique(as.character(Dados$Bebidas_Alcoolicas))
if (length(levels_atuais) > 4) {
  grp_levels <- levels_atuais[1:4]
  Dados <- Dados %>% filter(Bebidas_Alcoolicas %in% grp_levels)
}
Dados$Bebidas_Alcoolicas <- factor(Dados$Bebidas_Alcoolicas)

# 4) Lista de stopwords em PT-BR (ampliar conforme domínio)
palavras_proibidas <- c("bolsonaro","aumentado","basta","ideação","após","alguém","falta","sentimentos","acima","ambos","coisas","amei","vontade","alteração","alarmante","beire","muitos","aumentou","muito","bate","aumento","visitas","receber","tempo","trabalho","pessoas","diante","com","senti","desconto","bastante","breve","começo","caso","casa","amigas","também","pandemia", "governo","governamental","pra","fui","tive","conseguia","tinha","área","tais","mas","mais","disso","menos",
                        "ter","grande","antes","desses","por","dessa","item","sou","ainda","pelos","maior",
                        "estava","sim","oque","que","altoestima","baixa","nao","não","das","etc","dias","categórica","dos","fazer","certamente","ano")

# 5) Preparação do texto e cálculo de frequência por grupo
frequencia_palavras_pandemia <- Dados %>%
  # Passo 1: extrair texto
  mutate(texto_limpo = as.character(sentimentos_pandemia)) %>%
  # Passo 2: remover pontuação
  mutate(texto_limpo = str_replace_all(texto_limpo, "[\\.,!?;:()\\[\\]{}“”\"'-]", " ")) %>%
  # Passo 3: deixar tudo em minúsculas
  mutate(texto_limpo = str_to_lower(texto_limpo)) %>%
  # Passo 4: tokenizar
  unnest_tokens(palavra, texto_limpo) %>%
  # Passo 5: limpar
  filter(palavra != "") %>%
  filter(str_length(palavra) > 2) %>%          # Passo 6: palavras > 2 chars
  filter(!palavra %in% palavras_proibidas) %>%# Passo 7: remover stopwords/domínio
  group_by(Bebidas_Alcoolicas, palavra) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Bebidas_Alcoolicas) %>%
  mutate(porc = round((n / sum(n)) * 100, 2)) %>%  # opcional: porcentagem por grupo
  ungroup()

# 6) Top palavras por grupo (evita nuvens muito carregadas)
top_words <- frequencia_palavras_pandemia %>%
  group_by(Bebidas_Alcoolicas) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  slice_head(n =19) %>%  # ajuste o número de palavras negras por nuvem
  ungroup()

# 7) Ranking de frequência para colorir (opcional)
top_words <- top_words %>%
  group_by(Bebidas_Alcoolicas) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(ranking = dense_rank(desc(n))) %>%
  ungroup()

# 8) Plotagem: 4 nuvens estratificadas em grid 2x2
plot <- ggplot(top_words, aes(label = palavra, size = n^0.5, color = ranking)) +
  geom_text_wordcloud(
    seed = 123,
    min.size = 20,
    max.size = 16
  ) +
  facet_wrap(~Bebidas_Alcoolicas, ncol = 2) +
  scale_size_area(max_size = 18) +
  scale_color_gradientn(
    colours = colorRampPalette(
      c("#6D071A", "#FFD60A", "#2563EB")
    )(15)
  )+
  theme_minimal() +
  
  theme(
    
    # FUNDO GERAL
    plot.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # FUNDO DOS PAINÉIS
    panel.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # REMOVE GRID
    panel.grid = element_blank(),
    
    # BORDA DOS FACETS
    panel.border = element_rect(
      color = "#1e293b",
      fill = NA,
      linewidth = 1
    ),
    
    # TÍTULOS DOS FACETS
    strip.background = element_rect(
      fill = "#0f172a",
      color = "#334155"
    ),
    
    strip.text = element_text(
      face = "bold",
      color = "white",
      size = 12
    ),
    
    # TÍTULO PRINCIPAL
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "white"
    ),
    
    # SUBTÍTULO
    plot.subtitle = element_text(
      color = "gray80",
      size = 11
    ),
    
    # LEGENDA
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    
    # FUNDO DA LEGENDA
    legend.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # TEXTO DOS EIXOS
    axis.text = element_blank(),
    axis.title = element_blank(),
    
    # REMOVE EIXOS
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Sentimentos relatados durante a pandemia por consumo de bebida alcoólica",
    color = "Ranking (freq.)"
  )

# 9) Salvar/imprimir
print(plot)
# opcional: salvar
ggsave("nuvens_estratificadas_4.png", plot = plot, width = 12, height = 8, dpi = 300)


# 8) Plotagem: 4 nuvens estratificadas em grid 2x2
plot <- ggplot(top_words, aes(label = palavra, size = n^0.5, color = ranking)) +
  geom_text_wordcloud(
    seed = 123,
    min.size = 3,
    max.size = 10
  ) +
  facet_wrap(~Bebidas_Alcoolicas, ncol = 2) +
  scale_size_area(max_size = 12) +
  scale_color_gradientn(
    colours = colorRampPalette(
      c("#6D071A", "#FFD60A", "#2563EB")
    )(15)
  )+
  theme_minimal() +
  
  theme(
    
    # FUNDO GERAL
    plot.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # FUNDO DOS PAINÉIS
    panel.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # REMOVE GRID
    panel.grid = element_blank(),
    
    # BORDA DOS FACETS
    panel.border = element_rect(
      color = "#1e293b",
      fill = NA,
      linewidth = 1
    ),
    
    # TÍTULOS DOS FACETS
    strip.background = element_rect(
      fill = "#0f172a",
      color = "#334155"
    ),
    
    strip.text = element_text(
      face = "bold",
      color = "white",
      size = 12
    ),
    
    # TÍTULO PRINCIPAL
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "white"
    ),
    
    # SUBTÍTULO
    plot.subtitle = element_text(
      color = "gray80",
      size = 11
    ),
    
    # LEGENDA
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    
    # FUNDO DA LEGENDA
    legend.background = element_rect(
      fill = "#050816",
      color = NA
    ),
    
    # TEXTO DOS EIXOS
    axis.text = element_blank(),
    axis.title = element_blank(),
    
    # REMOVE EIXOS
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Sentimentos relatados durante a pandemia por consumo de bebida alcoólica",
    color = "Ranking (freq.)"
  )

# 9) Salvar/imprimir
print(plot)

ggsave(
  "nuvens_estratificadas_4.png",
  plot = plot,
  width = 13,
  height = 7,
  dpi = 300
)
