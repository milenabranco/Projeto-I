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
          
          variable == "IMC" ~
            formatar_letras(letras_imc),
          
          variable == "Peso" ~
            formatar_letras(letras_peso),
          
          variable == "Altura" ~
            formatar_letras(letras_altura),
          
          variable == "Idade" ~
            formatar_letras(letras_idade),
          
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
      "W: ANOVA de Welch + Games-Howell<br>
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

# Ordem dos grupos
Dados1$Bebidas_Alcoolicas <- factor(
  Dados1$Bebidas_Alcoolicas,
  levels = c(
    "Aumentou",
    "Diminuiu",
    "Não alterou",
    "Não consumo"
  )
)

# Tema geral
tema <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#1d4ed8"
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.line = element_line(color = "#334155"),
    axis.ticks = element_line(color = "#334155")
  )

# Função para criar boxplots
criar_box <- function(y, titulo, ylab, ylim = NULL) {
  
  ggplot(Dados1, aes(x = Bebidas_Alcoolicas, y = {{y}})) +
    
    geom_boxplot(
      fill = "#355C7D",
      alpha = 0.85,
      outlier.alpha = 0.4,
      outlier.size = 1
    ) +
    
    labs(
      title = titulo,
      x = NULL,
      y = ylab
    ) +
    
    tema +
    
    {if (!is.null(ylim)) coord_cartesian(ylim = ylim) else NULL}
}

# Gráficos
p1 <- criar_box(
  Idade,
  "Idade",
  "Idade (anos)"
)

p2 <- criar_box(
  Altura,
  "Altura",
  "Altura (m)"
)

p3 <- criar_box(
  Peso,
  "Peso",
  "Peso (kg)",
  ylim = c(40, 130)
)

p4 <- criar_box(
  IMC,
  "IMC",
  expression(IMC~(kg/m^2))
)

# Combinar gráficos
painel_boxplots <- (p1 | p2) / (p3 | p4)

png("boxplots_painel.png", width = 4800, height = 3000, res = 400)
print(painel_boxplots)
dev.off()

