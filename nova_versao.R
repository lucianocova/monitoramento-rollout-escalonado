

#############################################################################################################################################################################
#                                                                       TCC - Luciano Leite da Cova                                                                         #
#############################################################################################################################################################################



# %%  
#############################################################################################################################################################################
#                                                                         CARREGAMENTO DE PACOTES                                                                           #
#############################################################################################################################################################################

# Pacote utilizado para manipulação eficiente das bases de dados.
library(data.table) 
library(Kendall) 
 
# %% 
#############################################################################################################################################################################
#                                                                         CRIANDO REPRODUTIBILIDADE                                                                         #
#############################################################################################################################################################################
 
# Define a semente para garantir a reprodutibilidade dos sorteios realizados na simulação. Dessa forma, a mesma execução produz os mesmos resultados. 

set.seed(123) 
 
# %%  
#############################################################################################################################################################################
#                                                                       CRIANDO AS CARTEIRAS FICTÍCIAS                                                                      #
#############################################################################################################################################################################

# Número de carteiras consideradas na população simulada. 
# A carteira constitui a unidade de análise do estudo. 

n_carteiras <- 1000 
 
# %%  
 
# Criação da base inicial de carteiras.
#
# Cada linha representa uma carteira, que constitui a unidade de análise
# da simulação. O identificador é único e sequencial.
base_carteiras <- data.frame( 
  id_carteira = 1:n_carteiras 
) 
 
# Visualização das primeiras observações para verificar a estrutura 
# da base criada. 
head(base_carteiras) 
 
# %%  
 
# Define o número exato de carteiras que receberão o tratamento.
#
# Neste estudo, o tratamento representa a implementação do CRM.
n_tratadas <- 200 
 
# %%  
 
# Sorteio aleatório das carteiras que receberão o tratamento.
#
# O sorteio é realizado sem reposição, garantindo que uma mesma carteira
# não seja selecionada mais de uma vez.
carteiras_tratadas <- sample( 
  base_carteiras$id_carteira, 
  size = n_tratadas, 
  replace = FALSE 
) 
 
# %%  
 
# Criação da variável indicadora de tratamento:
# 1 = carteira selecionada para receber o tratamento;
# 0 = carteira não selecionada.
#
# Neste momento, a variável indica apenas a condição de tratamento.
# A efetiva entrada do tratamento ao longo do tempo será definida
# posteriormente pela variável TR.
base_carteiras$tratamento <- ifelse( 
  base_carteiras$id_carteira %in% carteiras_tratadas, 
  1, 
  0 
) 
 
# %%  
 
# Verificação da quantidade de carteiras em cada grupo.
# Permite confirmar se o sorteio produziu exatamente 200 carteiras
# tratadas e 800 não tratadas.
table(base_carteiras$tratamento) 
 
# Conversão da variável de tratamento para o formato inteiro.
#
# A expressão lógica retorna TRUE/FALSE e a conversão para inteiro
# transforma esses valores em 1/0, mantendo a codificação binária
# utilizada na simulação.
base_carteiras$tratamento <- as.integer( 
  base_carteiras$id_carteira %in% carteiras_tratadas 
)

# Mantém apenas as carteiras selecionadas para tratamento.
base_carteiras <- base_carteiras[
  base_carteiras$tratamento == 1,
]

# %% 
#############################################################################################################################################################################
#                                                                           Dia de início do tratamento                                                                     #
#############################################################################################################################################################################

# Inicializa a variável que armazenará o momento de início do tratamento.
# Para carteiras não tratadas, o valor permanece NA.
base_carteiras$d_inicio_tratamento <- NA_integer_

# %% 

# Sorteio do dia de início do tratamento somente entre as carteiras tratadas.
#
# O tratamento pode ser iniciado a partir de 2024 e até o final de 2025,
# respeitando o mínimo de 120 observações no período pós-tratamento.
#
# O sorteio é realizado com reposição, permitindo que diferentes carteiras
# iniciem o tratamento no mesmo dia.
base_carteiras$d_inicio_tratamento[
  base_carteiras$tratamento == 1
] <- sample(
  261:763,
  size = sum(base_carteiras$tratamento == 1),
  replace = TRUE
)

# %% 

# Visualização da base após a atribuição dos dias de início do tratamento.
head(base_carteiras)

# %% 

# Verificação da distribuição dos dias de início do tratamento.
summary(base_carteiras$d_inicio_tratamento)


#############################################################################################################################################################################
#                                                                           Modelo de simulação                                                                             #
#############################################################################################################################################################################

# A variável Y é gerada a partir de um modelo de regressão linear simples.
# A formulação permite controlar separadamente o nível da carteira,
# sua tendência natural, o efeito do tratamento e a variabilidade aleatória.
#
# A estrutura do modelo é:
#
# Y(i,t) = beta_0(i) + (1+TR(i,t)*(beta_2(i)-1))*beta_1(i)*t + u(i,t)
#
# onde:
#
# - beta_0: nível inicial da carteira;
# - beta_1: intensidade e direção da tendência linear natural da carteira;
# - beta_2: efeito relativo do tratamento sobre a tendência;
# - TR: indicador de ativação do tratamento ao longo do tempo;
# - t: índice temporal;
# - u: componente aleatório do modelo.
#
# O tratamento é incorporado à função por meio de beta_2. Antes da
# implementação, TR = 0 e, portanto, a trajetória esperada é:
#
# Y(i,t) = beta_0(i) + beta_1(i) * t + u(i,t)
#
# Após a implementação, TR = 1 e a trajetória passa a ser:
#
# Y(i,t) = beta_0(i) + beta_2(i) * beta_1(i) * t + u(i,t)
#
# Dessa forma, beta_2 representa o efeito verdadeiro do tratamento sobre
# a tendência da carteira:
#
# beta_2 = 0.90 -> redução de 10% na tendência;
# beta_2 = 1.10 -> aumento de 10% na tendência.
#
# O parâmetro beta_0 não é afetado pelo tratamento. O efeito é aplicado
# exclusivamente ao coeficiente de tendência beta_1 e passa a atuar a
# partir do dia definido em d_inicio_tratamento, por meio da variável TR.
#
# O erro u representa a variabilidade aleatória do processo.
#
# A geração dos dados é realizada na escala original de Y. A transformação
# de Box-Cox não faz parte do processo gerador e será avaliada posteriormente
# durante a etapa de análise, como uma possível transformação da variável
# resposta antes da estimação da regressão linear.


#############################################################################################################################################################################
#                                                                         PARÂMETROS DA SIMULAÇÃO                                                                           #
#############################################################################################################################################################################


########################################################################## BETA 0 — nível inicial ###########################################################################

BETA_0 <- 150000


######################################################################## BETA 1 — tendência natural #########################################################################

# Probabilidade de cada classe

PROB_BETA_1_QUEDA       <- 1/2
PROB_BETA_1_CRESCIMENTO <- 1/2

# Distribuição de beta_1 — classe "queda"

BETA_1_QUEDA_MEDIA      <- -100
BETA_1_QUEDA_SD         <- +50

# Distribuição de beta_1 — classe "crescimento"
BETA_1_CRESCIMENTO_MEDIA <- 100
BETA_1_CRESCIMENTO_SD    <- 50


################################################################## BETA 2 — efeito verdadeiro do tratamento ###################################################################

# beta_2 < 1  → redução da tendência
# beta_2 > 1  → aumento da tendência

BETA_2_QUEDA       <- 0.50
BETA_2_CRESCIMENTO <- 2.00

# Probabilidade de cada efeito
PROB_BETA_2_QUEDA        <- 1/2
PROB_BETA_2_CRESCIMENTO  <- 1/2

########################################################################### Parâmetro de Erros ################################################################################

erro_minimo <- -150000
erro_maximo <- +150000

######################################################################## PARÂMETROS DA MÉDIA MÓVEL ############################################################################

# Tipo de agregação:
#
# "media"   -> média móvel
# "mediana" -> mediana móvel
TIPO_MM <- "media"

# Tamanho da janela móvel.
JANELA_MM <- 30

###############################################################################################################################################################################
#                                                                    GERAÇÃO DOS PARÂMETROS DAS CARTEIRAS                                                                     #
###############################################################################################################################################################################


################################################################################# BETA 0 ######################################################################################

# Define o intercepto da função geradora.
base_carteiras$beta_0 <- BETA_0


################################################################################## BETA 1 #####################################################################################

# Define a classe da tendência linear de cada carteira.
base_carteiras$classe_beta_1 <- sample(
  c("queda", "crescimento"),
  size = nrow(base_carteiras),
  replace = TRUE,
  prob = c(
    PROB_BETA_1_QUEDA,
    PROB_BETA_1_CRESCIMENTO
  )
)

# Inicializa beta_1.
base_carteiras$beta_1 <- NA_real_

# Sorteia beta_1 para carteiras cuja trajetória pertence à classe "queda".
base_carteiras$beta_1[
  base_carteiras$classe_beta_1 == "queda"
] <- rnorm(
  sum(base_carteiras$classe_beta_1 == "queda"),
  mean = BETA_1_QUEDA_MEDIA,
  sd = BETA_1_QUEDA_SD
)

# Sorteia beta_1 para carteiras cuja trajetória pertence à classe "crescimento".
base_carteiras$beta_1[
  base_carteiras$classe_beta_1 == "crescimento"
] <- rnorm(
  sum(base_carteiras$classe_beta_1 == "crescimento"),
  mean = BETA_1_CRESCIMENTO_MEDIA,
  sd = BETA_1_CRESCIMENTO_SD
)


################################################################################### BETA 2 ####################################################################################

# Define o efeito verdadeiro do tratamento.
base_carteiras$beta_2 <- sample(
  c(
    BETA_2_QUEDA,
    BETA_2_CRESCIMENTO
  ),
  size = nrow(base_carteiras),
  replace = TRUE,
  prob = c(
    PROB_BETA_2_QUEDA,
    PROB_BETA_2_CRESCIMENTO
  )
)

# Cria uma variável categórica para identificar
# a direção do efeito verdadeiro.
base_carteiras$classe_beta_2 <- factor(
  base_carteiras$beta_2,
  levels = c(
    BETA_2_QUEDA,
    BETA_2_CRESCIMENTO
  ),
  labels = c(
    "queda",
    "crescimento"
  )
)


########################################################################## Validação dos parâmetros ###########################################################################

# Os histogramas abaixo permitem verificar visualmente as distribuições
# dos principais parâmetros utilizados na geração dos dados sintéticos.
#
# Essa etapa permite verificar se a população simulada apresenta as
# características esperadas antes da expansão da base para o nível diário.

par(mfrow = c(1, 1))


# beta_1
# Distribuição do parâmetro responsável pela tendência linear natural
# das carteiras.
hist(
  base_carteiras$beta_1,
  main = "Distribuição de beta_1",
  xlab = "beta_1",
  breaks = 50
)


# beta_2
# Distribuição dos efeitos verdadeiros utilizados na simulação.
# Como existem dois valores possíveis, o histograma permite verificar
# a frequência de cada intensidade de efeito.
hist(
  base_carteiras$beta_2,
  main = "Distribuição de beta_2",
  xlab = "beta_2",
  breaks = 5
)


# d_inicio_tratamento
# Distribuição dos dias sorteados para o início da implantação entre
# as carteiras tratadas.
hist(
  base_carteiras$d_inicio_tratamento[
    !is.na(base_carteiras$d_inicio_tratamento)
  ],
  main = "Distribuição do início do tratamento",
  xlab = "Dia de início",
  breaks = 30
)


# Restaura a configuração padrão para a geração de gráficos.
par(mfrow = c(1, 1))

# %% 

# %%
########################################################################## EXPANSÃO PARA A BASE DIÁRIA ########################################################################

# Define o número de períodos da série temporal simulada.
#
# A simulação considera 882 dias úteis, correspondentes ao período
# de observação definido para o estudo.
n_dias <- 882

# %%

# Expande a base de carteiras para o nível diário.
#
# Cada carteira passa a possuir uma observação para cada um dos
# 882 períodos da simulação.
#
# Com 200 carteiras, a base resultante terá:
#
# 200 carteiras x 882 períodos = 176.400 observações.
base_diaria <- base_carteiras[
  rep(seq_len(nrow(base_carteiras)), each = n_dias),
]

# Converte a base para data.table para permitir a utilização
# dos operadores e funções específicas do pacote.
setDT(base_diaria)

# %%

# Cria o índice temporal da simulação.
#
# Para cada carteira, t assume valores de 1 a 882, representando
# a sequência dos períodos de observação.
base_diaria[
  ,
  t := rep(1:n_dias, times = nrow(base_carteiras))
]

# %%

# Verifica a estrutura da base diária.
head(base_diaria)

# Verifica o número total de observações geradas.
nrow(base_diaria)


############################################################################# ATIVAÇÃO DO TRATAMENTO ##########################################################################

# Cria o indicador de ativação do tratamento ao longo do tempo.
#
# TR = 0 representa o período anterior à implantação do CRM.
# TR = 1 representa o período a partir do dia de implantação.
#
# Como a base contém somente carteiras tratadas, todas as carteiras
# terão TR = 0 antes da implantação e TR = 1 a partir do dia definido
# em d_inicio_tratamento.
base_diaria[
  ,
  TR := as.integer(
    t >= d_inicio_tratamento
  )
]

# %%

# Verifica a quantidade de observações em cada condição de tratamento.
#
# A tabela permite confirmar a quantidade de observações nos períodos
# pré e pós-tratamento.
table(base_diaria$TR)


################################################################################# ERRO ALEATÓRIO ##############################################################################

# Define o intervalo de variação do erro aleatório.
#
# O erro pode assumir qualquer valor entre -100.000 e +100.000,
# com igual probabilidade.
base_diaria[
  ,
  u := runif(
    .N,
    min = erro_minimo,
    max = erro_maximo
  )
]

# %%

# Estatísticas descritivas do erro aleatório.
summary(base_diaria$u)

# Verificação do desvio-padrão observado na simulação.
sd(base_diaria$u)

# %%

# Histograma para verificar visualmente a distribuição simulada do erro.
hist(
  base_diaria$u,
  breaks = 30,
  main = "Distribuição do erro aleatório",
  xlab = "u"
)

# %%
###############################################################################################################################################################################
#                                                                                 CÁLCULO DE Y                                                                                #
###############################################################################################################################################################################

# Calcula a resposta simulada Y a partir dos parâmetros da carteira,
# da ativação do tratamento, da tendência temporal, do momento da
# implantação e do erro aleatório.
#
# A geração é realizada diretamente na escala original de Y, segundo
# um modelo de regressão linear simples com mudança no coeficiente
# angular a partir do momento da implantação.
#
# A estrutura do modelo é:
#
# Y(i,t) =
#   beta_0(i) +
#   beta_1(i) * t +
#   TR(i,t) * (beta_2(i) - 1) *
#   beta_1(i) * (t - d_i) +
#   u(i,t)
#
# em que:
#
# beta_0(i) = nível inicial da carteira;
# beta_1(i) = coeficiente angular antes da implantação;
# beta_2(i) = fator de alteração do coeficiente angular;
# d_i      = período de implantação da carteira i;
# TR(i,t)  = indicador de tratamento;
# u(i,t)   = erro aleatório.
#
# Antes da implantação, TR = 0:
#
# Y(i,t) = beta_0(i) + beta_1(i) * t + u(i,t)
#
# Após a implantação, TR = 1:
#
# Y(i,t) = beta_0(i) + beta_1(i)*t + (beta_2(i)-1)*beta_1(i)*(t-d_i) + u(i,t)
#
# A formulação mantém a trajetória contínua no momento da implantação,
# pois, quando t = d_i, o termo associado ao tratamento é igual a zero.
#
# Após a implantação, o coeficiente angular passa de beta_1(i) para:
#
# beta_1_pós(i) = beta_2(i) * beta_1(i)
#
# O intercepto da reta pós-implantação é determinado implicitamente
# pela própria equação, não sendo necessário gerar um beta_0_pós
# separadamente.
#
# Dessa forma, beta_2 representa o efeito verdadeiro da implantação
# sobre o coeficiente angular da trajetória da carteira, enquanto a
# continuidade no ponto de implantação é preservada.

base_diaria[
  ,
  Y := beta_0 +
       beta_1 * t +
       TR * (beta_2 - 1) * beta_1 *
       (t - d_inicio_tratamento) +
       u
]

# %%

# Verifica os valores gerados para Y.
summary(base_diaria$Y)

# %%

# Verifica se foram gerados valores ausentes ou não finitos.
sum(is.na(base_diaria$Y))
sum(!is.finite(base_diaria$Y))

# %%

hist(
  base_diaria$Y,
  breaks = 30,
  main = "Distribuição do desembolso diário simulado",
  xlab = "Desembolso diário (Y)",
  xaxt = "n"
)

axis(
  1,
  at = axTicks(1),
  labels = format(
    axTicks(1),
    scientific = FALSE,
    big.mark = ".",
    decimal.mark = ","
  )
)


 plot(
  base_diaria[id_carteira == 5, t],
  base_diaria[id_carteira == 5, Y],
  type = "l",
  xlab = "Período (t)",
  ylab = "Desembolso diário (Y)",
  main = "Trajetória do desembolso diário — Carteira 5"
)


###############################################################################################################################################################################
#                                                                         TABELA COM A MÉDIA/MEDIANA MÓVEL                                                                    #
###############################################################################################################################################################################

# Garante a ordenação temporal dentro de cada carteira.
setorder(
  base_diaria,
  id_carteira,
  t
)

# Cria uma cópia da base diária para realizar os cálculos.
base_mm <- copy(base_diaria)


# %% 

# Calcula a média ou mediana móvel.
#
# A janela é alinhada à direita: o valor em t representa
# a agregação dos JANELA_MM períodos encerrados em t.

if (TIPO_MM == "media") {
  
  base_mm[
    ,
    Y_mm := frollmean(
      Y,
      n = JANELA_MM,
      align = "right"
    ),
    by = id_carteira
  ]
  
} else if (TIPO_MM == "mediana") {
  
  base_mm[
    ,
    Y_mm := frollmedian(
      Y,
      n = JANELA_MM,
      align = "right"
    ),
    by = id_carteira
  ]
  
} else {
  
  stop(
    "TIPO_MM deve ser 'media' ou 'mediana'."
  )
}


# %% 

# Calcula quantos dos períodos da janela estão sob tratamento.
base_mm[
  ,
  n_TR := frollsum(
    TR,
    n = JANELA_MM,
    align = "right"
  ),
  by = id_carteira
]


# %% 

# Classifica cada janela.
#
# "pre" = todos os períodos são anteriores à implantação;
# "mix" = a janela contém períodos pré e pós-implantação;
# "pos" = todos os períodos são posteriores à implantação.

base_mm[
  ,
  periodo := fifelse(
    n_TR == 0,
    "pre",
    fifelse(
      n_TR == JANELA_MM,
      "pos",
      "mix"
    )
  )
]


# %% 

# Remove os primeiros JANELA_MM - 1 períodos, que não possuem
# observações suficientes para o cálculo da janela completa.

base_mm <- base_mm[
  !is.na(Y_mm)
]


# %% 

# Remove as janelas que misturam períodos pré e pós-implantação.

base_mm <- base_mm[
  periodo != "mix"
]


# %% 

# Verifica a quantidade de observações em cada período.

table(
  base_mm$periodo
)


# %% 

# Define o nome da agregação para utilização nos títulos.

nome_mm <- ifelse(
  TIPO_MM == "media",
  "Média",
  "Mediana"
)


# %% 

# Histograma da distribuição da média/mediana móvel.

hist(
  base_mm$Y_mm,
  breaks = 100,
  main = paste(
    "Distribuição do desembolso diário —",
    nome_mm,
    "móvel de",
    JANELA_MM,
    "dias"
  ),
  xlab = paste(
    "Desembolso diário —",
    nome_mm,
    "móvel de",
    JANELA_MM,
    "dias"
  ),
  ylab = "Frequência"
)


# %% 

# Estatísticas descritivas da média/mediana móvel.

summary(
  base_mm$Y_mm
)


# %% 

# Compara visualmente as distribuições dos períodos
# pré e pós-implantação.

ggplot(
  base_mm,
  aes(x = Y_mm)
) +
  geom_histogram(
    bins = 100
  ) +
  facet_wrap(
    ~ periodo,
    scales = "free_y"
  ) +
  labs(
    title = paste(
      "Distribuição da",
      tolower(nome_mm),
      "móvel de",
      JANELA_MM,
      "dias por período"
    ),
    x = paste(
      "Desembolso diário —",
      tolower(nome_mm),
      "móvel de",
      JANELA_MM,
      "dias"
    ),
    y = "Frequência"
  ) +
  theme_minimal()


# %% 

# Cria um identificador para cada combinação de carteira e período.

base_mm[
  ,
  id_carteira_periodo := paste(
    id_carteira,
    periodo,
    sep = "_"
  )
]


# %% 

# Separa as observações pré e pós-implantação.

base_pre <- base_mm[
  periodo == "pre"
]

base_pos <- base_mm[
  periodo == "pos"
]


# %% 

# Visualiza a trajetória da média/mediana móvel da carteira 5.

 plot(
  base_mm[id_carteira == 5, t],
  base_mm[id_carteira == 5, Y_mm],
  type = "l",
  xlab = "Período (t)",
  ylab = paste(
   "Desembolso diário —",
    nome_mm,
    "móvel de",
    JANELA_MM,
    "dias"
  ),
  main = paste(
    "Trajetória da",
    tolower(nome_mm),
    "móvel de",
    JANELA_MM,
    "dias — Carteira 5"
  )
)


###############################################################################################################################################################################
#                                                                        REGRESSÕES LINEARES — PRÉ E PÓS                                                                      #
###############################################################################################################################################################################

# Estima uma regressão linear simples para cada carteira nos períodos pré e pós-implantação.

# Modelo:

# Y_mm = beta_0 + beta_1 * t + u

modelos_pre <- base_pre[
  ,
  list(
    modelo = list(
      lm(
        Y_mm ~ t,
        data = .SD
      )
    )
  ),
  by = id_carteira
]


# %% 

# Regressões para o período pós-implantação.

modelos_pos <- base_pos[
  ,
  list(
    modelo = list(
      lm(
        Y_mm ~ t,
        data = .SD
      )
    )
  ),
  by = id_carteira
]


# %% 

###############################################################################################################################################################################
#                                                                  Extrai os parâmetros das regressões pré-implantação                                                        #
###############################################################################################################################################################################

parametros_pre <- modelos_pre[
  ,
  {
    resumo <- summary(modelo[[1]])
    coeficientes <- coef(resumo)
    
    .(
      beta_0_pre = coeficientes["(Intercept)", "Estimate"],
      b1_pre = coeficientes["t", "Estimate"],
      se_b1_pre = coeficientes["t", "Std. Error"],
      p_valor_b1_pre = coeficientes["t", "Pr(>|t|)"],
      r_quadrado_pre = resumo$r.squared,
      n_pre = nobs(modelo[[1]])
    )
  },
  by = id_carteira
]


# %% 

# Extrai os parâmetros das regressões pós-implantação.

parametros_pos <- modelos_pos[
  ,
  {
    resumo <- summary(modelo[[1]])
    coeficientes <- coef(resumo)
    
    .(
      beta_0_pos = coeficientes["(Intercept)", "Estimate"],
      b1_pos = coeficientes["t", "Estimate"],
      se_b1_pos = coeficientes["t", "Std. Error"],
      p_valor_b1_pos = coeficientes["t", "Pr(>|t|)"],
      r_quadrado_pos = resumo$r.squared,
      n_pos = nobs(modelo[[1]])
    )
  },
  by = id_carteira
]


# %% 
###############################################################################################################################################################################
#                                                             Junta os resultados das regressões pré e pós por carteira                                                       #
###############################################################################################################################################################################

tabela_regressoes <- merge(
  parametros_pre,
  parametros_pos,
  by = "id_carteira"
)


# %% 

# Inclui na tabela final as classes verdadeiras utilizadas na simulação.

tabela_regressoes <- merge(
  tabela_regressoes,
  base_carteiras[
    ,
    c(
      "id_carteira",
      "classe_beta_1",
      "classe_beta_2",
      "beta_1",
      "beta_2"
    )
  ],
  by = "id_carteira"
)


# %% 

###############################################################################################################################################################################
#                                                          Calcula a razão entre os coeficientes angulares pós e pré                                                          #
###############################################################################################################################################################################
#
# A razão estima o multiplicador aplicado à tendência:
#
# beta_1_pos / beta_1_pre ≈ beta_2

tabela_regressoes[
  ,
  b1_pos_b1_pre := b1_pos / b1_pre
]

# Calcula a variação da inclinação entre os períodos
# pré e pós-implantação.

tabela_regressoes[
  ,
  delta_b1 := b1_pos - b1_pre
]


# %% 

###############################################################################################################################################################################
#                                                                   Classifica o impacto estimado da implantação                                                              #
###############################################################################################################################################################################

# A classificação considera a mudança observada na inclinação.
#
# delta_b1 > 0 -> aumento da inclinação -> impacto positivo;
# delta_b1 < 0 -> redução da inclinação -> impacto negativo;
# delta_b1 = 0 -> ausência de alteração na inclinação.

tabela_regressoes[
  ,
  impacto_estimado := fifelse(
    delta_b1 > 0,
    "positivo",
    fifelse(
      delta_b1 < 0,
      "negativo",
      "neutro"
    )
  )
]


# %% 

# Calcula a variação verdadeira da inclinação provocada pelo tratamento.
#
# Como:
#
# beta_1_pos = beta_2 * beta_1
#
# então:
#
# Delta beta_1 = beta_1_pos - beta_1 = beta_1 * (beta_2 - 1)

tabela_regressoes[
  ,
  delta_b1_verdadeiro := beta_1 * (beta_2 - 1)
]


# Classifica o impacto verdadeiro utilizado na simulação.
#
# delta_b1_verdadeiro > 0 -> impacto positivo;
# delta_b1_verdadeiro < 0 -> impacto negativo;
# delta_b1_verdadeiro = 0 -> ausência de alteração.

tabela_regressoes[
  ,
  impacto_verdadeiro := fifelse(
    delta_b1_verdadeiro > 0,
    "positivo",
    fifelse(
      delta_b1_verdadeiro < 0,
      "negativo",
      "neutro"
    )
  )
]


# %% 

# Verifica se a direção do impacto foi identificada corretamente.

tabela_regressoes[
  ,
  acertou_impacto := impacto_estimado == impacto_verdadeiro
]


# %% 

# Verifica a quantidade de acertos e erros.

table(
  tabela_regressoes$acertou_impacto
)


# %% 

########################################################
# ACURÁCIA GLOBAL
########################################################

# Calcula a proporção de carteiras em que a direção do impacto
# foi identificada corretamente.

acuracia <- mean(
  tabela_regressoes$acertou_impacto
)

acuracia


# %% 

########################################################
# PERCENTUAL DE ACERTO POR CLASSE DE BETA 2
########################################################

tabela_acuracia <- tabela_regressoes[
  ,
  .(
    n_carteiras = .N,
    n_acertos = sum(acertou_impacto),
    percentual_acerto = mean(acertou_impacto) * 100
  ),
  by = classe_beta_2
]

tabela_acuracia


# %% 

########################################################
# TESTE DE HIPÓTESES — ACURÁCIA MAIOR QUE O ACASO
########################################################

# Número de acertos.
n_acertos <- sum(
  tabela_regressoes$acertou_impacto
)

# Número total de carteiras avaliadas.
n_total <- nrow(
  tabela_regressoes
)

# Teste binomial unilateral.
#
# H0: p = 0,50
# H1: p > 0,50
#
# p representa a probabilidade de a metodologia classificar
# corretamente a direção do impacto.

teste_acuracia <- binom.test(
  x = n_acertos,
  n = n_total,
  p = 0.50,
  alternative = "greater"
)

teste_acuracia


###############################################################################################################################################################################
#                                                                         ACURÁCIA POR PERFIL DA CARTEIRA                                                                     #
###############################################################################################################################################################################

# Cria um perfil combinando a direção da tendência natural
# (beta_1) com a direção do efeito verdadeiro (beta_2).

tabela_regressoes[
  ,
  perfil := paste(
    "tendência",
    classe_beta_1,
    "| efeito",
    classe_beta_2
  )
]


# %% 

# Calcula a quantidade de carteiras, número de acertos e percentual de acerto para cada perfil.

tabela_acuracia_perfil <- tabela_regressoes[
  ,
  .(
    n_carteiras = .N,
    n_acertos = sum(acertou_impacto),
    n_erros = sum(!acertou_impacto),
    percentual_acerto = mean(acertou_impacto) * 100
  ),
  by = .(
    classe_beta_1,
    classe_beta_2,
    perfil
  )
]


# Ordena os resultados pela tendência e pelo efeito.

setorder(
  tabela_acuracia_perfil,
  classe_beta_1,
  classe_beta_2
)


# Visualiza a tabela.

tabela_acuracia_perfil


###############################################################################################################################################################################
#                                                                        TESTE DE HIPÓTESES POR PERFIL                                                                        #
###############################################################################################################################################################################

# Para cada perfil:
#
# H0: p = 0,50
# H1: p > 0,50
#
# em que p representa a probabilidade de a metodologia
# classificar corretamente a direção do impacto da implantação.

testes_perfil <- tabela_regressoes[
  ,
  {
    teste <- binom.test(
      x = sum(acertou_impacto),
      n = .N,
      p = 0.50,
      alternative = "greater"
    )
    
    .(
      n_carteiras = .N,
      n_acertos = sum(acertou_impacto),
      n_erros = sum(!acertou_impacto),
      percentual_acerto = mean(acertou_impacto) * 100,
      p_valor = teste$p.value
    )
  },
  by = .(
    classe_beta_1,
    classe_beta_2
  )
]


# %% 

# Indica se o resultado é estatisticamente significativo ao nível de 5%.

testes_perfil[
  ,
  significativo_5pct := p_valor < 0.05
]


# %% 

# Visualiza os resultados.

testes_perfil