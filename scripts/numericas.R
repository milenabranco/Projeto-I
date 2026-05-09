library(dplyr)
library(ggplot2)
library(openxlsx)
library(gtsummary)
library(gt)
library(car)
library(ggpubr)
library(rstatix)
library(multcompView)
library(patchwork)
library(qqplotr)
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",") # formatação em português (vírgula pra decimais e ponto para milhares)

# CARREGANDO DADOS ----
Dados = read.xlsx("Nutricao.xlsx")

# iniciando a analise das variaveis quantitivas
DadosQuanti = Dados %>% 
  select(Idade,Altura,Peso, IMC)

#INVESTIGANDO ASSOCIAÇÃO 
Dados1 = Dados %>% 
  select(Idade,Altura,Peso, IMC, Bebidas_Alcoolicas)

#teste de normalidade para aplicar o teste t 

ggplot(Dados1, aes(x = Idade)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Bebidas_Alcoolicas) +
  xlab("Idade") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = Idade)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Bebidas_Alcoolicas,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))
by(Dados1$Idade, Dados1$Bebidas_Alcoolicas, shapiro.test)

#nega normalidade
# altura

ggplot(Dados1, aes(x = Altura)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Bebidas_Alcoolicas) +
  xlab("Altura") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))


ggplot(Dados1, aes(sample = Altura)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Bebidas_Alcoolicas,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))
by(Dados1$Altura, Dados1$Bebidas_Alcoolicas, shapiro.test)


#Peso

ggplot(Dados1, aes(x = Peso)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Bebidas_Alcoolicas) +
  xlab("Peso") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = Peso)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Bebidas_Alcoolicas,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(Dados1$Peso, Dados1$Bebidas_Alcoolicas, shapiro.test)

#IMC

ggplot(Dados1, aes(x = IMC)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Bebidas_Alcoolicas) +
  xlab("IMC") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = IMC)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Bebidas_Alcoolicas,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))
by(Dados1$IMC, Dados1$Bebidas_Alcoolicas, shapiro.test)

LeveneTest(formula = Idade~Bebidas_Alcoolicas,data = Dados1)

bartlett.test(formula = Altura~Bebidas_Alcoolicas,data = Dados1)

LeveneTest(formula = Peso~Bebidas_Alcoolicas,data = Dados1)

LeveneTest(formula = IMC~Bebidas_Alcoolicas,data = Dados1)


#indo para tabela final 
tbl_summary(
  data = Dados1,
  by = Bebidas_Alcoolicas,
  statistic = all_continuous() ~ "{mean} ({sd})"
) |>
  add_p(
    test = everything() ~ "oneway.test",
    test.args = list(
      Idade  ~ list(var.equal = FALSE), # Welch
      Peso   ~ list(var.equal = TRUE),
      Altura ~ list(var.equal = TRUE),
      IMC    ~ list(var.equal = TRUE)
    )
  )

tabela_quanti <- tbl_summary(
  data = Dados1,
  by = Bebidas_Alcoolicas,
  statistic = all_continuous() ~ "{mean} ({sd})",
  label = list(
    Idade ~ "Idade<sup>W</sup>",
    Altura ~ "Altura<sup>T</sup>",
    Peso ~ "Peso<sup>T</sup>",
    IMC ~ "IMC<sup>T</sup>"
  )
) %>%
  add_p(
    test = everything() ~ "oneway.test",
    test.args = list(
      Idade  ~ list(var.equal = FALSE),
      Altura ~ list(var.equal = TRUE),
      Peso   ~ list(var.equal = TRUE),
      IMC    ~ list(var.equal = TRUE)
    ),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  modify_spanning_header(all_stat_cols() ~ "**Consumo de Bebidas Alcoólicas**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%
  fmt_markdown(columns = label) %>%
  tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
tabela_quanti


modelo <- aov(IMC ~ Bebidas_Alcoolicas, data = Dados1)
modelo
TukeyHSD(modelo)

modelo1 <- aov(Peso ~ Bebidas_Alcoolicas, data = Dados1)

TukeyHSD(modelo1)
modelo2 <- aov(Altura ~ Bebidas_Alcoolicas, data = Dados1)
TukeyHSD(modelo2)

require(rstatix)
games_howell_test(Idade ~ Bebidas_Alcoolicas,
                  data=Dados1)

# Pacotes
library(dplyr)
library(gtsummary)
library(gt)
library(multcompView)
library(rstatix)

# Ordem das categorias
Dados1$Bebidas_Alcoolicas <- factor(
  Dados1$Bebidas_Alcoolicas,
  levels = c(
    "Aumentou",
    "Diminuiu",
    "Não alterou",
    "Não consumo"

 )
)
formatar_letras <- function(x) {
  paste(
    names(x),
    x,
    sep = " = ",
    collapse = "<br>"
  )
}
letras_idade <- c(
  "Aumentou" = "a",
  "Diminuiu" = "a",
  "Não alterou" = "b",
  "Não consumo" = "a"
)

letras_altura <- c(
  "Aumentou" = "b",
  "Diminuiu" = "a",
  "Não alterou" = "ab",
  "Não consumo" = "b"
)

letras_peso <- c(
  "Aumentou" = "a",
  "Diminuiu" = "a",
  "Não alterou" = "a",
  "Não consumo" = "a"
)

letras_imc <- c(
  "Aumentou" = "a",
  "Diminuiu" = "a",
  "Não alterou" = "a",
  "Não consumo" = "a"
)

#---------------------------
# TABELA FINAL
#---------------------------

tabela_quanti_final <-
  tbl_summary(
    data = Dados1,
    by = Bebidas_Alcoolicas,
    
    statistic = all_continuous() ~ "{mean} ({sd})",
    
    label = list(
      Idade  ~ "Idade<sup>W</sup>",
      Altura ~ "Altura<sup>A</sup>",
      Peso   ~ "Peso<sup>A</sup>",
      IMC    ~ "IMC<sup>A</sup>"
    )
  ) %>%
  
  add_p(
    test = everything() ~ "oneway.test",
    
    test.args = list(
      Idade  ~ list(var.equal = FALSE),
      Altura ~ list(var.equal = TRUE),
      Peso   ~ list(var.equal = TRUE),
      IMC    ~ list(var.equal = TRUE)
    ),
    
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  
  modify_header(
    label ~ "**Características**",
    p.value ~ "**p-valor**"
  ) %>%
  
  bold_p(t = 0.05) %>%
  
  modify_table_body(
    ~ .x %>%
      mutate(
        comparacoes = case_when(
          variable == "Idade"  ~ formatar_letras(letras_idade),
          variable == "Altura" ~ formatar_letras(letras_altura),
          variable == "Peso"   ~ formatar_letras(letras_peso),
          variable == "IMC"    ~ formatar_letras(letras_imc),
          
          TRUE ~ NA_character_
        )
      )
  ) %>%
  
  modify_header(
    comparacoes ~ "**Comparações múltiplas**"
  ) %>%
  
  modify_spanning_header(
    all_stat_cols() ~ "**Consumo de bebidas alcoólicas**"
  ) %>%
  
  modify_table_styling(
    columns = comparacoes,
    text_interpret = "html"
  ) %>%
  
  bold_labels() %>%
  
  modify_footnote(
    update = everything() ~ NA
  ) %>%
  
  as_gt() %>%
  
  fmt_markdown(
    columns = c(label, comparacoes)
  ) %>%
  
  tab_source_note(
    source_note = md(
      "Média (Desvio Padrão) <br>
      W: ANOVA de Welch + Games-Howell<br>
      A: ANOVA clássica + Tukey"
    )
  ) %>%
  
  tab_options(
    table.font.size = "20px",
    column_labels.font.size = "22px"
  )

# Visualizar tabela
tabela_quanti_final


# salvando tabela em rds
saveRDS(tabela_quanti_final,file="tabela_quanti.rds")



#--------------------------------------------------------------------------------

library(ggplot2)
library(patchwork)

criar_violin <- function(y, titulo, ylab, ylim = NULL) {
  
  ggplot(Dados1, aes(x = Bebidas_Alcoolicas, y = {{y}}, fill = Bebidas_Alcoolicas)) +
    
    # Distribuição (variabilidade)
    geom_violin(trim = FALSE, alpha = 0.7, color = "gray40") +
    
    # Mediana e quartis
    geom_boxplot(width = 0.12, fill = "white", color = "black", outlier.size = 0.5) +
    
    # Pontos individuais (variabilidade real)
    geom_jitter(width = 0.08, alpha = 0.2, size = 0.8) +
    
    # Média (destaque)
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "yellow",
      color = "black"
    ) +
    
    # Cores por grupo (ESSENCIAL para seu objetivo)
    scale_fill_brewer(palette = "Set2") +
    
    labs(
      title = titulo,
      x = NULL,
      y = ylab
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 15, hjust = 1)
    ) +
    
    # Controle de escala opcional
    {if (!is.null(ylim)) coord_cartesian(ylim = ylim) else NULL}
}

# Gráficos individuais
p1 <- criar_violin(Idade, "Idade", "Idade (anos)")
p2 <- criar_violin(Altura, "Altura", "Altura (m)")
p3 <- criar_violin(Peso, "Peso", "Peso (kg)")
p4 <- criar_violin(IMC, "IMC", "IMC (kg/m²)")

# Painel final
painel_final <- (p1 | p2) / (p3 | p4)

# Exibir
painel_final

# Salvar imagem
ggsave(
  filename = "painel_violin_artigo.png",
  plot = painel_final,
  width = 12,
  height = 8,
  dpi = 300
)

library(ggplot2)
library(ggpubr)
library(patchwork)

criar_violin_final <- function(y, titulo, ylab, ylim = NULL) {
  
  ggplot(Dados1, aes(x = Bebidas_Alcoolicas, y = {{y}}, fill = Bebidas_Alcoolicas)) +
    
    # Distribuição
    geom_violin(trim = FALSE, alpha = 0.65, color = "gray40") +
    
    # Boxplot (mediana + IQR)
    geom_boxplot(width = 0.12, fill = "white", color = "black", outlier.size = 0.4) +
    
    # Dados individuais
    #geom_jitter(width = 0.08, alpha = 0.15, size = 0.7) +
    
    # Média
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "red",
      color = "black"
    ) +
    
    # Intervalo de confiança da média
    stat_summary(
      fun.data = mean_cl_normal,
      geom = "errorbar",
      width = 0.15,
      color = "black"
    ) +
    
    # 🔥 P-VALUE GLOBAL (ANOVA/WELCH)
    stat_compare_means(
      method = "anova",
      label = "p.format",
      label.y.npc = "top"
    ) +
    
    scale_fill_brewer(palette = "Set2") +
    
    labs(
      title = titulo,
      x = NULL,
      y = ylab
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 15, hjust = 1)
    ) +
    
    {if (!is.null(ylim)) coord_cartesian(ylim = ylim) else NULL}
}


p1 <- criar_violin_final(Idade, "Idade", "Idade (anos)")
p2 <- criar_violin_final(Altura, "Altura", "Altura (m)")
p3 <- criar_violin_final(Peso, "Peso", "Peso (kg)")
p4 <- criar_violin_final(IMC, "IMC", "IMC (kg/m²)")
painel_final <- (p1 | p2) / (p3 | p4)

ggsave(
  filename = "painel_violin_artigo.png",
  plot = painel_final,
  width = 12,
  height = 8,
  dpi = 300
)
library(ggplot2)
library(patchwork)

criar_violin <- function(y, titulo, ylab, ylim = NULL) {
  
  ggplot(Dados1, aes(x = Bebidas_Alcoolicas, y = {{y}}, fill = Bebidas_Alcoolicas)) +
    
    # Violino (distribuição)
    geom_violin(trim = FALSE, alpha = 0.75, color = "gray30") +
    
    # Boxplot (mediana + IQR)
    geom_boxplot(width = 0.12, fill = "white", color = "gray10", outlier.size = 0.4) +
    
    # Média
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3.2,
      fill = "red",
      color = "black"
    ) +
    
    # p-valor global (ANOVA)
    stat_compare_means(
      method = "anova",
      label = "p.format",
      label.y.npc = "top"
    ) +
    
    # 🎨 Paleta sóbria (azul petróleo + vinho + tons neutros)
    scale_fill_manual(values = c(
      "#0F4C5C",  # azul petróleo escuro
      "#5F0F40",  # vinho
      "#335C67",  # azul acinzentado
      "#6C757D"   # cinza neutro
    )) +
    
    labs(
      title = titulo,
      x = NULL,
      y = ylab
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 15, hjust = 1),
      axis.title = element_text(face = "bold")
    ) +
    
    {if (!is.null(ylim)) coord_cartesian(ylim = ylim) else NULL}
}

# Gráficos
p1 <- criar_violin(Idade, "Idade", "Idade (anos)")
p2 <- criar_violin(Altura, "Altura", "Altura (m)")
p3 <- criar_violin(Peso, "Peso", "Peso (kg)")
p4 <- criar_violin(IMC, "IMC", "IMC (kg/m²)")

# Painel final
painel_final <- (p1 | p2) / (p3 | p4)

painel_final
ggsave(
  filename = "painel_violin_soberio.png",
  plot = painel_final,
  width = 12,
  height = 8,
  dpi = 300
)

library(ggplot2)
library(patchwork)
library(ggpubr)
library(dplyr)

#-----------------------------
# FUNÇÃO ÚNICA (CORRIGIDA)
#-----------------------------
criar_violin <- function(data, var, titulo, ylab) {
  
  ggplot(data, aes(
    x = Bebidas_Alcoolicas,
    y = .data[[var]],
    fill = Bebidas_Alcoolicas
  )) +
    
    # distribuição
    geom_violin(alpha = 0.75, color = "gray30", trim = FALSE) +
    
    # boxplot
    geom_boxplot(width = 0.12, fill = "white", color = "black", outlier.size = 0.4) +
    
    # média
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "white",
      color = "black"
    ) +
    
    # p-value global
    stat_compare_means(
      method = "anova",
      label = "p.format",
      label.y.npc = "top"
    ) +
    
    # paleta sóbria (azul petróleo + vinho)
    scale_fill_manual(values = c(
      "Aumentou"     = "#0F4C5C",
      "Diminuiu"     = "#5F0F40",
      "Não alterou"  = "#335C67",
      "Não consumo"   = "#6C757D"
    )) +
    
    labs(
      title = titulo,
      x = NULL,
      y = ylab
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 15, hjust = 1)
    )
}

#-----------------------------
# GRÁFICOS
#-----------------------------
p1 <- criar_violin(Dados1, "Idade",  "Idade",  "Idade (anos)")
p2 <- criar_violin(Dados1, "Altura", "Altura", "Altura (m)")
p3 <- criar_violin(Dados1, "Peso",   "Peso",   "Peso (kg)")
p4 <- criar_violin(Dados1, "IMC",    "IMC",    "IMC (kg/m²)")

#-----------------------------
# PAINEL FINAL
#-----------------------------
painel_final <- (p1 | p2) / (p3 | p4)

painel_final

#-----------------------------
# SALVAR
#-----------------------------
ggsave(
  filename = "painel_violin_final.png",
  plot = painel_final,
  width = 12,
  height = 8,
  dpi = 300
)

