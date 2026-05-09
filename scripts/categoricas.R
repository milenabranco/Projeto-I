#Projeto I
#CARREGANDO PACOTES-----

require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene
library()
#------------------------------------------------------

theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",") # formatação em português (vírgula pra decimais e ponto para milhares)

# CARREGANDO DADOS ----
Dados = read.xlsx("Nutricao.xlsx")

#selecionando dados qualitativos ----
DadosQuali = Dados %>% select(Bebidas_Alcoolicas,
                              Genero,Raca_cor,
                              Regiao,
                              Isolamento,
                              Trabalha,
                              Profissional_Saude,
                              Renda_familiar,
                              Escolaridade,
                              Covid,
                              Consulta_Nutricionista,
                              Dificuldade_Financeira,
                              Acesso_Alimento,
                              Tempo_Preparo_Refeicao,
                              cigarro,
                              atividade_fisica_pandemia)
#-------------------------------------------------------
tbl_summary(data = DadosQuali,
            by = Bebidas_Alcoolicas,
            percent= "row" )




#------------------------------------------------------
# investigando associação----

chisq.test(Dados$Genero,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Raca_cor,Dados$Bebidas_Alcoolicas)$expected #fisher

chisq.test(Dados$Regiao,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Isolamento,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Trabalha,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Profissional_Saude,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Renda_familiar,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Escolaridade,Dados$Bebidas_Alcoolicas)$expected #fisher

chisq.test(Dados$Covid,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Consulta_Nutricionista,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Dificuldade_Financeira,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Acesso_Alimento,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$Tempo_Preparo_Refeicao,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$cigarro,Dados$Bebidas_Alcoolicas)$expected

chisq.test(Dados$atividade_fisica_pandemia,Dados$Bebidas_Alcoolicas)$expected

#raca_cor e escolaridade tem frequencias esperadas menores que 5 

#investigando associações significativas 

tbl_summary(
  data = DadosQuali,
  by = Bebidas_Alcoolicas,
  percent = "row"
) %>%
  add_p(
    test = list(
      Escolaridade ~ "fisher.test",
      Raca_cor     ~ "fisher.test",
      everything() ~ "chisq.test"
    )
  ) %>% 
  bold_p(t = 0.05)

# investigando residuos padronizados:
chisq.test(Dados$Raca_cor,Dados$Bebidas_Alcoolicas)$stdres


chisq.test(Dados$Regiao,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Isolamento,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Trabalha,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Profissional_Saude,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Renda_familiar,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Escolaridade,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Covid,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Dificuldade_Financeira,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$Acesso_Alimento,Dados$Bebidas_Alcoolicas)$stdres


chisq.test(Dados$Tempo_Preparo_Refeicao,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$cigarro,Dados$Bebidas_Alcoolicas)$stdres

chisq.test(Dados$atividade_fisica_pandemia,Dados$Bebidas_Alcoolicas)$stdres

#Tabela final ----
# Função que calcula o coeficiente de Cramer
cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}
# Tabela final ----

tabela_quali<-tbl_summary(data = DadosQuali,
                          by = Bebidas_Alcoolicas,
                          percent = "row",
                          label = list(
                            Genero ~ "Gênero<sup>Q</sup>",
                            Raca_cor ~ "Raça/Cor<sup>F</sup>",
                            Regiao ~ "Região<sup>Q</sup>",
                            Isolamento ~ "Isolamento Social<sup>Q</sup>",
                            Trabalha ~ "Trabalha<sup>Q</sup>",
                            Profissional_Saude ~ "Profissional de Saúde<sup>Q</sup>",
                            Renda_familiar ~ "Renda Familiar<sup>Q</sup>",
                            Escolaridade ~ "Escolaridade<sup>F</sup>",
                            Covid ~ "Covid<sup>Q</sup>",
                            Consulta_Nutricionista ~ " Consulta_Nutricionsta<sup>Q</sup>",
                            Dificuldade_Financeira ~ "Dificuldade Financeira<sup>Q</sup>",
                            Acesso_Alimento ~ "Acesso a Alimento<sup>Q</sup>",
                            Tempo_Preparo_Refeicao ~ "Tempo de Preparo da Refeição<sup>Q</sup>",
                            cigarro ~ "Consumo de Cigarro<sup>Q</sup>",
                            atividade_fisica_pandemia ~ "Atividade Física na Pandemia<sup>Q</sup>"
                          )
            )%>%
  add_p(  
    test = list(
      Escolaridade ~ "fisher.test",
      Raca_cor     ~ "fisher.test",
      everything() ~ "chisq.test"
    ) )%>% 
  bold_p(t = 0.05) %>%
  add_stat(fns = all_categorical() ~ cramer_fun) %>%
  modify_spanning_header(all_stat_cols() ~ "**Consumo de Bebidas Alcoólicas**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") %>%
  
  modify_bold(
    columns = stat_2,
    rows = (variable == "Regiao" & label == "Norte") |
      (variable == "Regiao" & label == "Sudeste")
  ) %>%
  modify_bold(
    columns = stat_4,
    rows = (variable == "Regiao" & label == "Norte") |
      (variable == "Regiao" & label == "Sudeste")
  ) %>%
  
  modify_bold(
    columns = stat_4,
    rows = (variable == "Isolamento")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_2, stat_3, stat_4),
    rows = (variable == "Trabalha")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_4),
    rows = (variable == "Profissional_Saude")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_3, stat_4),
    rows = (variable == "Renda_familiar" & label == "até R$ 1254,00")
  ) %>%
  modify_bold(
    columns = stat_1,
    rows = (variable == "Renda_familiar" & label == "entre R$ 1.255 -  R$ 8.640")
  ) %>%
  modify_bold(
    columns = c(stat_1, stat_2, stat_3, stat_4),
    rows = (variable == "Renda_familiar" & label == "mais de R$ 8.640")
  ) %>%
  modify_bold(
    columns = stat_4,
    rows = (variable == "atividade_fisica_pandemia" & label == "Diminuiu")
  ) %>%
  
  modify_bold(
    columns = c(stat_1,stat_3, stat_4),
    rows = (variable == "Escolaridade" & label == "Ensino Fundamental Completo")
  ) %>%
  modify_bold(
    columns = c(stat_1),
    rows = (variable == "Escolaridade" & label == "Analfabeto")
  ) %>%
  modify_bold(
    columns = c(stat_1, stat_2, stat_3),
    rows = (variable == "Escolaridade" & label == "Ensino Médio Completo")
  ) %>%
  modify_bold(
    columns = c(stat_1, stat_2, stat_3, stat_4),
    rows = (variable == "Escolaridade" & label == "Pós-graduação")
  ) %>%
  
  modify_bold(
    columns = stat_4,
    rows = (variable == "Covid")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_2, stat_3, stat_4),
    rows = (variable == "Dificuldade_Financeira")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_2, stat_3, stat_4),
    rows = (variable == "Acesso_Alimento")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_2, stat_3),
    rows = (variable == "Tempo_Preparo_Refeicao" & label == "Aumentou")
  ) %>%
  modify_bold(
    columns = stat_1,
    rows = (variable == "Tempo_Preparo_Refeicao" & label == "Diminuiu")
  ) %>%
  modify_bold(
    columns = c(stat_1, stat_3, stat_4),
    rows = (variable == "Tempo_Preparo_Refeicao" & label == "Não alterou")
  ) %>%
  
  modify_bold(
    columns = c(stat_1, stat_4),
    rows = (variable == "cigarro")
  ) %>%
  
  modify_bold(
    columns = stat_1,
    rows = (variable == "atividade_fisica_pandemia" & label == "Diminuiu")
  ) %>%
  modify_bold(
    columns = c(stat_2, stat_3),
    rows = (variable == "atividade_fisica_pandemia" & label == "Não alterou")
  ) %>%
  modify_bold(
    columns = stat_4,
    rows = (variable == "atividade_fisica_pandemia" & label == "Não tenho o hábito")
  ) %>%
  modify_bold(
    columns = c(stat_1),
    rows = (variable == "Raca_cor" & label == "Indígena")
  ) %>%
  modify_bold(
    columns = c(stat_4),
    rows = (variable == "Raca_cor" & label == "Branca")
  ) %>%
  modify_bold(
    columns = c(stat_1),
    rows = (variable == "Raca_cor" & label == "Amarela")
  ) %>%
  modify_bold(
    columns = c(stat_4),
    rows = (variable == "Raca_cor" & label == "Parda")
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%
  tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
tabela_quali

tabela_quali <- tabela_quali%>%
  tab_style(
    style = cell_fill(color = "#dbeafe"),
    locations = cells_body(
      rows = p.value < 0.05
    )
  ) %>% 
  tab_style(
    style = gt::cell_borders(
      sides = "bottom",
      color = "#7a7a7a",
      weight = gt::px(1.5) 
    ),
    locations = gt::cells_body(
      rows = !duplicated(variable, fromLast = TRUE)
    )
  )

tabela_quali1 <- tabela_quali%>%
  gt::fmt_markdown(
    columns = "label",
    rows = !grepl("R\\$", label)
  )



tabela_categorica <- tabela_quali1 %>%
  tab_source_note(
    source_note = md("**Q**: Teste do qui-quadrado de Pearson; <br>
                     **F**: Teste exato de Fisher; <br>
                     Valores em negrito na coluna *Valor-p* indicam significância estatística (p < 0,05); <br>
                     Valores em negrito nas tabelas de contingência indicam células com resíduos padronizados elevados.")
  ) %>% 
  tab_options(
    source_notes.font.size = "13px"
  ) %>%
  tab_style(
    style = cell_text(color = "#666666"),
    locations = cells_source_notes()
  )
tabela_categorica

cor_pos <- "#1f3a5f"  # turquesa suave
cor_neg <- "#7f1d1d"  # coral suave

tabela_categorica_colorida <- tabela_categorica %>%
  
  # POSITIVOS (> 1.96)
  tab_style(
    style = cell_text(color = cor_pos, weight = "bold"),
    locations = cells_body(columns = stat_1, rows =
                             (variable == "Raca_cor" & label == "Indígena") |
                             (variable == "Trabalha" & label == "Sim") |
                             (variable == "Profissional_Saude" & label == "Sim") |
                             (variable == "Renda_familiar" & label == "mais de R$ 8.640") |
                             (variable == "Escolaridade" & label == "Analfabeto") |
                             (variable == "Escolaridade" & label == "Pós-graduação") |
                             (variable == "Dificuldade_Financeira" & label == "Não") |
                             (variable == "Acesso_Alimento" & label == "Não") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Aumentou") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Diminuiu") |
                             (variable == "atividade_fisica_pandemia" & label == "Diminuiu") |
                             (variable == "cigarro" & label == "Sim")
    )
  ) %>%
  
  # NEGATIVOS (< -1.96)
  tab_style(
    style = cell_text(color = cor_neg, weight = "bold"),
    locations = cells_body(columns = stat_1, rows =
                             (variable == "Raca_cor" & label == "Amarela") |
                             (variable == "Trabalha" & label == "Não") |
                             (variable == "Profissional_Saude" & label == "Não") |
                             (variable == "Renda_familiar" & label == "até R$ 1254,00") |
                             (variable == "Renda_familiar" & label == "entre R$ 1.255 - R$ 8.640") |
                             (variable == "Escolaridade" & label == "Ensino Fundamental Completo") |
                             (variable == "Escolaridade" & label == "Ensino Médio Completo") |
                             (variable == "Dificuldade_Financeira" & label == "Sim") |
                             (variable == "Acesso_Alimento" & label == "Sim") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Não alterou") |
                             (variable == "cigarro" & label == "Não")
    )
  ) %>%
  
  # POSITIVOS
  tab_style(
    style = cell_text(color = cor_pos, weight = "bold"),
    locations = cells_body(columns = stat_2, rows =
                             (variable == "Regiao" & label == "Sudeste") |
                             (variable == "Trabalha" & label == "Não") |
                             (variable == "Escolaridade" & label == "Ensino Médio Completo") |
                             (variable == "Dificuldade_Financeira" & label == "Não") |
                             (variable == "Acesso_Alimento" & label == "Sim") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Aumentou")
    )
  ) %>%
  
  # NEGATIVOS
  tab_style(
    style = cell_text(color = cor_neg, weight = "bold"),
    locations = cells_body(columns = stat_2, rows =
                             (variable == "Regiao" & label == "Norte") |
                             (variable == "Trabalha" & label == "Sim") |
                             (variable == "Renda_familiar" & label == "mais de R$ 8.640") |
                             (variable == "Escolaridade" & label == "Pós-graduação") |
                             (variable == "Dificuldade_Financeira" & label == "Sim") |
                             (variable == "Acesso_Alimento" & label == "Não") |
                             (variable == "atividade_fisica_pandemia" & label == "Não alterou")
    )
  ) %>%
  
  # POSITIVOS
  tab_style(
    style = cell_text(color = cor_pos, weight = "bold"),
    locations = cells_body(columns = stat_3, rows =
                             (variable == "Trabalha" & label == "Sim") |
                             (variable == "Renda_familiar" & label == "mais de R$ 8.640") |
                             (variable == "Escolaridade" & label == "Pós-graduação") |
                             (variable == "Dificuldade_Financeira" & label == "Não") |
                             (variable == "Acesso_Alimento" & label == "Não") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Não alterou") |
                             (variable == "atividade_fisica_pandemia" & label == "Não alterou")
    )
  ) %>%
  
  # NEGATIVOS
  tab_style(
    style = cell_text(color = cor_neg, weight = "bold"),
    locations = cells_body(columns = stat_3, rows =
                             (variable == "Trabalha" & label == "Não") |
                             (variable == "Renda_familiar" & label == "até R$ 1254,00") |
                             (variable == "Escolaridade" & label == "Ensino Médio Completo") |
                             (variable == "Escolaridade" & label == "Ensino Fundamental Completo") |
                             (variable == "Dificuldade_Financeira" & label == "Sim") |
                             (variable == "Acesso_Alimento" & label == "Sim") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Aumentou")
    )
  ) %>%
  
  # POSITIVOS
  tab_style(
    style = cell_text(color = cor_pos, weight = "bold"),
    locations = cells_body(columns = stat_4, rows =
                             (variable == "Raca_cor" & label == "Parda") |
                             (variable == "Regiao" & label == "Norte") |
                             (variable == "Isolamento" & label == "Sim") |
                             (variable == "Trabalha" & label == "Não") |
                             (variable == "Profissional_Saude" & label == "Não") |
                             (variable == "Renda_familiar" & label == "até R$ 1254,00") |
                             (variable == "Escolaridade" & label == "Ensino Fundamental Completo") |
                             (variable == "Covid" & label == "Não") |
                             (variable == "Dificuldade_Financeira" & label == "Sim") |
                             (variable == "Acesso_Alimento" & label == "Sim") |
                             (variable == "Tempo_Preparo_Refeicao" & label == "Não alterou") |
                             (variable == "atividade_fisica_pandemia" & label == "Não tenho o hábito") |
                             (variable == "cigarro" & label == "Não")
    )
  ) %>%
  
  # NEGATIVOS
  tab_style(
    style = cell_text(color = cor_neg, weight = "bold"),
    locations = cells_body(columns = stat_4, rows =
                             (variable == "Raca_cor" & label == "Branca") |
                             (variable == "Regiao" & label == "Sudeste") |
                             (variable == "Isolamento" & label == "Não") |
                             (variable == "Trabalha" & label == "Sim") |
                             (variable == "Profissional_Saude" & label == "Sim") |
                             (variable == "Renda_familiar" & label == "mais de R$ 8.640") |
                             (variable == "Escolaridade" & label == "Pós-graduação") |
                             (variable == "Covid" & label == "Sim") |
                             (variable == "Dificuldade_Financeira" & label == "Não") |
                             (variable == "Acesso_Alimento" & label == "Não") |
                             (variable == "cigarro" & label == "Sim") |
                             (variable == "atividade_fisica_pandemia" & label == "Diminuiu")
    )
  )
tabela_categorica_colorida

# salvando tabela em rds
saveRDS(tabela_categorica_colorida,file="tabela_quali.rds")
library(dplyr)
library(ggplot2)
library(rstatix)

#========================
# VARIÁVEIS CATEGÓRICAS
#========================

vars <- c(
  "Genero",
  "Raca_cor",
  "Regiao",
  "Isolamento",
  "Trabalha",
  "Profissional_Saude",
  "Renda_familiar",
  "Escolaridade",
  "Covid",
  "Consulta_Nutricionista",
  "Dificuldade_Financeira",
  "Acesso_Alimento",
  "Tempo_Preparo_Refeicao",
  "cigarro",
  "atividade_fisica_pandemia"
)

#========================
# USANDO A MESMA LÓGICA
# DA FUNÇÃO cramer_fun
#========================

resultado <- lapply(vars, function(v){
  
  tab <- table(
    Dados[[v]],
    Dados$Bebidas_Alcoolicas
  )
  
  teste <- suppressWarnings(
    chisq.test(tab)
  )
  
  data.frame(
    Variavel = v,
    Cramer = round(cramer_v(tab), 3),
    p = teste$p.value
  )
  
}) %>%
  bind_rows()

#========================
# NOMES BONITOS
#========================

resultado$Variavel <- c(
  "Gênero",
  "Raça/cor",
  "Região",
  "Isolamento",
  "Trabalha",
  "Profissional de saúde",
  "Renda familiar",
  "Escolaridade",
  "Covid",
  "Consulta nutricionista",
  "Dificuldade financeira",
  "Acesso ao alimento",
  "Tempo preparo refeição",
  "Cigarro",
  "Atividade física"
)

#========================
# FOREST PLOT
#========================

grafico_cat <- ggplot(
  resultado,
  aes(
    x = Cramer,
    y = reorder(Variavel, Cramer),
    color = Cramer
  )
) +
  
  geom_segment(
    aes(
      x = 0,
      xend = Cramer,
      y = Variavel,
      yend = Variavel
    ),
    linewidth = 2
  ) +
  
  geom_point(
    size = 5
  ) +
  
  geom_text(
    aes(label = round(Cramer, 3)),
    hjust = -0.3,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_color_gradient(
    low = "#60a5fa",
    high = "#0f172a"
  ) +
  
  labs(
    title = "Magnitude das associações categóricas",
    subtitle = "Coeficiente V de Cramér",
    x = "V de Cramér",
    y = NULL
  ) +
  
  xlim(0, max(resultado$Cramer) + 0.05) +
  
  theme_minimal(base_size = 18) +
  
  theme(
    legend.position = "none",
    
    panel.grid.major.y = element_blank(),
    
    axis.text.y = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

grafico_cat

#========================
# SALVAR
#========================

ggsave(
  "forest_categoricas.png",
  grafico_cat,
  width = 12,
  height = 7,
  dpi = 400
)

