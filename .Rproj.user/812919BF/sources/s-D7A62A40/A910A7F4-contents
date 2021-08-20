# Fonte de dados: https://www.kaggle.com/harrywang/wine-dataset-for-clustering

# Problema de negócio: Lucia herdou uma pequena loja de vinhos do seu avô e agora
# quer vende-los em seu próprio estabelecimento. Apesar de ser uma empresaria e 
# tanto, ela não conhece muito sobre vinhos. Lucia deseja agrupar vinhos parecidos
# em uma mesma prateleria para não confundir os seus consumidores e garantir uma
# venda.

################################################################################
#                   CARREGAMENTO DOS PACOTES NECESSÁRIOS                      #
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(dendextend)

################################################################################
#                        CARREGAMENTO DA BASE DE DADOS                         #
################################################################################

wines <- read.table('wines.csv', sep = ',', dec = '.', header = T)

################################################################################
#                            ANALISE EXPLORATÓRIA                              #
################################################################################

head(wines)
str(wines)
summary(wines)
any(is.na(wines))

wines %>%
  gather(Variable, Value) %>%
  ggplot(aes(y = Value)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = 'free')

################################################################################
#                              PRÉ-PROCESSAMENTO                               #
################################################################################

wines_stand <- scale(wines)
summary(wines_stand)

################################################################################
#                                MODELAGEM                                     #
#                             MÉTODO HIERÁRQUICO                               #  
################################################################################

# Primeiro passo: Cálculo da distância euclidiana
dist <- dist(wines_stand, method = 'euclidean')

# Segundo passo: Definindo o tipo de cluster: ward
ward <- hclust(dist, method = 'ward.D')

# Terceiro passo: Plotando o dendograma
plot(ward)

# Quarto passo: Verificar número de clusters pelo método de elbow
fviz_nbclust(wines_stand, FUN = hcut, method = "wss")

# Quinto passo: Destacar os grupos
ward_dend_obj <- as.dendrogram(ward)
ward_col_dend <- color_branches(ward_dend_obj, k = 3)
plot(ward_col_dend)

# Sexto passo: Criar os grupos de vinhos
wine_groups <- cutree(ward, k = 3)
table(wine_groups)

# Sétimo passo: Adicionar o grupo ao data frame original
group <- data.frame(wine_groups)
colnames(group) <- 'Group'
wines_grouped <- cbind(wines, group)

# Oitavo passo: Salvar o resultado para montar um relatório a posteriori.
save(wines_grouped, file = 'wines_grouped.csv')

################################################################################
#                            ANALISE DESCRITIVA                                #
################################################################################

# Melhor número de grupos: 3

# Calcular as médias das variáveis por grupo
mean_group <- wines_grouped %>% 
  group_by(Group) %>% 
  summarise(n = n(),
            Alcohol = mean(Alcohol), 
            Malic_Acid = mean(Malic_Acid), 
            Ash = mean(Ash),
            Ash_Alcanity = mean(Ash_Alcanity), 
            Magnesium = mean(Magnesium), 
            Total_Phenols = mean(Total_Phenols),
            Flavanoids = mean(Flavanoids), 
            Nonflavanoid_Phenols = mean(Nonflavanoid_Phenols), 
            Proanthocyanins = mean(Proanthocyanins),
            Color_Intensity = mean(Color_Intensity), 
            Hue = mean(Hue),
            OD280 = mean(OD280),
            Proline = mean(Proline))

mean_group
