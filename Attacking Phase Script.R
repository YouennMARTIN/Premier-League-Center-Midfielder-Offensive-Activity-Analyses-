# Import Libraries 
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(scatterplot3d)
library(rgl)
library(plotly)
library(htmlwidgets)
library(rstatix)
library(kableExtra)
library(dplyr)
library(tidyr)
library(knitr)
library(DT)


# Import File 
Attacking_Phase <- read_excel("AttackingPhase.xlsx")
print(Attacking_Phase)

# 1st PART - Unbalance the opponent and finish with a scoring opportunity
## Players' preferred position distribution
df_counts <- Attacking_Phase %>%
  count(Preferred_Position)

ggbarplot(
  data = df_counts, 
  x = "Preferred_Position",
  y = "n",
  title = "Preferred Position Distribution",
  xlab = FALSE,
  ylab = "",
  label = TRUE,
  font.label = c(14,"bold","red"),
  width = 0.5,
  fill = "Preferred_Position",
  palette = c("#123456", "#0A35AD"),
  legend = 'none') +
  theme(plot.title = element_text(
          size = 15,        
          hjust = 0.5))

## Goals contributions
Contributions_df <- Attacking_Phase %>%
  group_by(Goals, Assists) %>%
  summarise(Players = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    Combination = paste(Goals, "G/", Assists, "A"),
    Combination = factor(
      Combination,
      levels = c("2 G/ 1 A", 
                 "1 G/ 2 A", 
                 "2 G/ 0 A", 
                 "0 G/ 2 A", 
                 "1 G/ 1 A", 
                 "1 G/ 0 A", 
                 "0 G/ 1 A", 
                 "0 G/ 0 A")
    )
  )

ggplot(Contributions_df, aes(x = Combination, y = Players, fill = Combination, )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Players), hjust = -0.5, size = 4) +  # labels au bout
  coord_flip() +  # barres horizontales
  labs(title = "Players' goals contributions",
       x = "",
       y = "Number of Players") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(
        size = 15,        
        hjust = 0.5),
        panel.grid = element_blank(),         
        axis.text.x = element_blank(),         
        axis.ticks.x = element_blank())


## xG and xA
### Méthode du coude
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("Non_Pen_xG", "OP_xA")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

### Méthode de la silhouette
fviz_nbclust(Attacking_Phase[, c("Non_Pen_xG", "OP_xA")],
             kmeans,
             method = "wss") +
  labs(title = "Détermination du nombre optimal de clusters (méthode du coude)")


### Nuage de points
xG_xA_Clusters <- kmeans(Attacking_Phase[, c("Non_Pen_xG", "OP_xA")], centers = 5)
Attacking_Phase$Cluster <- as.factor(xG_xA_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = Non_Pen_xG, y = OP_xA, color = Cluster)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Players' openplay xG and xA comparison",
       x = "xG",
       y = "xA",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
    size = 15,
    hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 0.9))

## xA and chances created
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("OP_Chances_Created", "OP_xA")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

Chances_xA_Clusters <- kmeans(Attacking_Phase[, c("OP_Chances_Created", "OP_xA")], centers = 2)
Attacking_Phase$Cluster <- as.factor(Chances_xA_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = OP_Chances_Created, y = OP_xA, color = Cluster, fill = Preferred_Position, shape = Preferred_Position)) +
  geom_point(aes(size = Preferred_Position, stroke = 1.2)) +
  geom_text_repel(aes(label = Players), size = 3) +
  scale_shape_manual(values = c("Centre Midfielder" = 19, "Defensive Midfielder" = 1)) +  
  scale_size_manual(values = c("Centre Midfielder" = 2, "Defensive Midfielder" = 3)) +   
  labs(title = "Players' openplay xA and chances created comparison",
       x = "Chances created",
       y = "xA") +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
    size = 15,
    hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 8.5), ylim = c(0, 0.75)) 


## NB of touches in opp box and chances created
### 2Dimension
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("OP_Chances_Created", "Touch_Opp_Box")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

Chances_TouchOppBox_Clusters <- kmeans(Attacking_Phase[, c("OP_Chances_Created", "Touch_Opp_Box")], centers = 4)
Attacking_Phase$Cluster <- as.factor(Chances_TouchOppBox_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = OP_Chances_Created, y = Touch_Opp_Box, color = Cluster, size = Non_Pen_xG)) +
  geom_point(stroke = 1.2, position = position_jitter(width = 0.1, height = 0.1), alpha = 0.55) +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Players' number of touches in the opp box and chances created comparison",
       x = "Chances created",
       y = "Number of touches in the opp box",
       size = "Non-penalty xG") +
  scale_color_manual(values = c( 
    "1" = "#234EFA",
    "2" = "#34AA2A",
    "3" = "#FE32EE",
    "4" = "#FF5733"), guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(
          size = 15,
          hjust = 0.5)) 

### 3Dimension with xAssists
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("OP_Chances_Created", "OP_xA", "Touch_Opp_Box")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

ThreeDOffensive_Clusters <- kmeans(Attacking_Phase[, c("OP_Chances_Created", "OP_xA", "Touch_Opp_Box")], centers = 4)
Attacking_Phase$Cluster <- as.factor(ThreeDOffensive_Clusters$cluster)

cluster_colors <- c("#ff7f0e", "#d62728", "#9467bd", "#e45ff5", "#aaad23")
colors <- cluster_colors[as.numeric(Attacking_Phase$Cluster)]

with(Attacking_Phase, {
  s3d <- scatterplot3d(
                x = OP_Chances_Created,
                xlab = "Chances created",
                y = OP_xA,
                ylab = "xAssists",
                z = Touch_Opp_Box, 
                zlab = "Touches in opp box",
                color = colors,
                pch = 15,
                type = "h")
  
  # convert 3-D coords to 2D projection
  s3d.coords <- s3d$xyz.convert(OP_Chances_Created,OP_xA,Touch_Opp_Box) 
  
  # plot text with 50% shrink and place to right of points
  text(s3d.coords$x, 
       s3d.coords$y,   
       labels = Attacking_Phase$Players,
       col = colors,
       cex = 0.5,
       pos = 4)
})
  
# 2nd PART - Keep possession and move forward with the ball
## Passes Analyses 
### Nb of Passes/90 and Pass Rate 
Attacking_Phase <- Attacking_Phase %>%
  mutate(
    Team_AVG_Possession_num = as.numeric(gsub(",", ".", as.character(Team_AVG_Possession))),
    Possession_Category = case_when(
      Team_AVG_Possession_num < 46 ~ "< 46%",
      Team_AVG_Possession_num >= 46 & Team_AVG_Possession_num < 54.9 ~ "46% - 55%",
      Team_AVG_Possession_num >= 54.9 ~ "> 55%"
      ),
    Possession_Category = factor(Possession_Category, levels = c("< 46%", "46% - 55%", "> 55%"))
  )

ggplot(Attacking_Phase, aes(x = Total_Passes_90, y = Total_Passes_Success, color = Possession_Category)) +
  geom_point(stroke = 1.2) +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Players' number of pass per 90 and pass rate",
       x = "Number of passes per 90",
       y = "Pass rate (%)",
       color = "Team Average Possession (%)") +
    scale_color_manual(values = c(
    "< 46%" = "#1AF",        
    "46% - 55%" = "blue",
    "> 55%" = "darkblue")) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
          size = 15,
          hjust = 0.5))
  

### Total and Final 3rd passes rates
PassRate_data <- Attacking_Phase %>%
  select(Players, Total_Passes_Success, Final3rd_Passes_Success) %>%
  pivot_longer(cols = c(Total_Passes_Success, Final3rd_Passes_Success),
               names_to = "Pass_Type",
               values_to = "Pass_Rate")

#### Rename xAxis labels for better clarity
PassRate_data$Pass_Type <- factor(PassRate_data$Pass_Type,
                             levels = c("Total_Passes_Success", "Final3rd_Passes_Success"),
                             labels = c("TotalPasses", "FinalThirdPasses"))

#### Passes Rate Evolution Calculation
PassRate_evolution <- PassRate_data %>%
  tidyr::pivot_wider(
    names_from = Pass_Type,
    values_from = Pass_Rate) %>%
  mutate(
    Evolution = FinalThirdPasses - TotalPasses,  # différence entre passes dans le dernier tiers et passes totales
    Evolution_Category = case_when(
      Evolution <= -10 ~ "Major drop (>10%)",
      Evolution > -10 & Evolution <= -6 ~ "Moderate drop (6-10%)",
      Evolution > -6 & Evolution <= -2.5 ~ "Minor drop (2.5-6%)",
      Evolution > -2.5 ~ "Stable or rise (<2.5%)"
      ),
    Evolution_Category = factor(Evolution_Category, levels = c("Major drop (>10%)", "Moderate drop (6-10%)", "Minor drop (2.5-6%)", "Stable or rise (<2.5%)"))
  )


#### Fusionner avec les données d'origine pour pouvoir tracer
PassRate_data <- PassRate_data %>%
  left_join(PassRate_evolution %>% select(Players, Evolution, Evolution_Category),
            by = "Players")

#### Plot the graph
ggplot(PassRate_data, aes(x = Pass_Type, y = Pass_Rate, group = Players, color = Evolution_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(data = filter(PassRate_data, Pass_Type == "Final3rd_Passes_Success"), aes(label = Players, y = Pass_Rate), size = 2.5, show.legend = FALSE) +
  labs(title = "Evolution of pass completion rate",
       x = "",
       y = "Pass success rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(
    size = 15, 
    hjust = 0.5),
    panel.grid = element_blank(),         
    axis.text.x = element_blank())
  

### Final 3rd pass proportion and completion rate
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("Final3rd_Passes_Success", "Final3rd_Passes_Perc")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

Final3rdpass_Clusters <- kmeans(Attacking_Phase[, c("Final3rd_Passes_Success", "Final3rd_Passes_Perc")], centers = 4)
Attacking_Phase$Cluster <- as.factor(Final3rdpass_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = Final3rd_Passes_Success, y = Final3rd_Passes_Perc, color = Cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Players' final 3rd passes proportion and completion rate comparison",
       x = "Final 3rd passes completion rate (%)",
       y = "Final 3rd passes proportion (%)") +
  scale_color_manual(values = c( 
    "1" = "#ff7f0e",
    "2" = "#1AF",
    "3" = "#9467bd",
    "4" = "#d62728"), guide = "none") +
  theme_minimal() +
  theme(
        plot.title = element_text(
          size = 15,
          hjust = 0.5)) 

## Carries Analyses
### Nb of carries and AVG distance of carry 
set.seed(123)

Normalized_Carries <- scale(Attacking_Phase[, c("Total_Carries", "AVG_Distance_Total_Carries")])
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Normalized_Carries, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

Carries_Clusters <- kmeans(Normalized_Carries, centers = 5, nstart = 20)
Attacking_Phase$Cluster <- as.factor(Carries_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = AVG_Distance_Total_Carries, 
                            y = Total_Carries, 
                            color = Cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Carries number VS average distance",
       y = "Number of carries",
       x = "Average distance of carries (m)") +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
      size = 15,
      hjust = 0.5))


### % prog. carries and distance made through prog. carries
set.seed(123)
Elbow_kmeans <- sapply(1:10, function(k){
  kmeans(Attacking_Phase[, c("Prog_Carries_Perc", "Distance_Prog_Carries_Perc")], centers = k, nstart = 20)$tot.withinss
})

plot(1:10, Elbow_kmeans, type = "b",
     xlab = "Nombre de clusters (k)",
     ylab = "Somme des carrés intra-clusters (WSS)",
     main = "Méthode du coude (Elbow)")

Progcarries_Clusters <- kmeans(Attacking_Phase[, c("Prog_Carries_Perc", "Distance_Prog_Carries_Perc")], centers = 3)
Attacking_Phase$Cluster <- as.factor(Progcarries_Clusters$cluster)

ggplot(Attacking_Phase, aes(x = Distance_Prog_Carries_Perc, y = Prog_Carries_Perc, color = Cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Players), size = 3) +
  labs(title = "Players' progressive carries proportion and distance covered using prog. carries proportion comparison",
       y = "Prog. carries proportion (%)",
       x = "Distance covered using prog. carries proportion (%)") +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
      size = 15,
      hjust = 0.5)) +
  coord_cartesian(xlim = c(25, 70), ylim = c(30, 70)) 


### Average distance of total and progressive carries rate
CarriesRate_data <- Attacking_Phase %>%
  select(Players, AVG_Distance_Total_Carries, AVG_Distance_Prog_Carries) %>%
  pivot_longer(cols = c(AVG_Distance_Total_Carries, AVG_Distance_Prog_Carries),
               names_to = "Carries_Type",
               values_to = "AVG_Distance")

#### Rename xAxis labels for better clarity
CarriesRate_data$Carries_Type <- factor(CarriesRate_data$Carries_Type,
                                  levels = c("AVG_Distance_Total_Carries", "AVG_Distance_Prog_Carries"),
                                  labels = c("AVG_Distance_Total_Carries", "AVG_Distance_Prog_Carries"))

#### Carries Rate Evolution Calculation
CarriesRate_evolution <- CarriesRate_data %>%
  tidyr::pivot_wider(
    names_from = Carries_Type,
    values_from = AVG_Distance) %>%
  mutate(
    Evolution = -AVG_Distance_Total_Carries + AVG_Distance_Prog_Carries,  # différence entre distance moyenne des conduites de balle totales par rapport aux conduites progressives
    Evolution_Category = case_when(
      Evolution <= -1.5 ~ "Moderate drop (<-1.5m)",
      Evolution > -1.5 & Evolution <= 0 ~ "Minor drop (0m/-1.5m)",
      Evolution > 0 ~ "Stable or rise (>0m)"
    ),
    Evolution_Category = factor(Evolution_Category, levels = c("Moderate drop (<-1.5m)", "Minor drop (0m/-1.5m)", "Stable or rise (>0m)"))
  )


#### Fusionner avec les données d'origine pour pouvoir tracer
CarriesRate_data <- CarriesRate_data %>%
  left_join(CarriesRate_evolution %>% select(Players, Evolution, Evolution_Category),
            by = "Players")

#### Plot the graph
ggplot(CarriesRate_data, aes(x = Carries_Type, y = AVG_Distance, group = Players, color = Evolution_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Evolution of average carries distance",
       x = "",
       y = "Average distance covered (m)") +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(
      size = 15, 
      hjust = 0.5),
      panel.grid = element_blank(),         
      axis.text.x = element_blank()) +
      scale_y_continuous(breaks = seq(7, 14, by = 1))

#### Interactive plot 
p <- ggplot(CarriesRate_data, aes(x = Carries_Type, y = AVG_Distance, group = Players)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of carries distance",
    x = "",
    y = "Average distance covered (m)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "none" # car tu pourras sélectionner les joueurs dans plotly
  )

fig <- ggplotly(p, tooltip = c("Players", "AVG_Distance", "Carries_Type"))
fig

#### Save plot as HTML type file 
saveWidget(as_widget(fig), "AVG_Carries_Distance_EVO.html")
browseURL("AVG_Carries_Distance_EVO.html")


## LAST PART : Principal Component Analysis (PCA) on the dataset
Attacking_Phase_num_var <- Attacking_Phase %>%
  dplyr::select_if(is.numeric)
Attacking_Phase.ACP <- PCA(Attacking_Phase_num_var, scale.unit = TRUE, graph = FALSE)

### Variances explained 
fviz_eig(Attacking_Phase.ACP, 
         addlabels = TRUE, 
         ylim = c(0, 60),
         main = "Explained variance percentage per dimension") +
  theme(
    plot.title = element_text(
      size = 15,     
      hjust = 0.5
    )
  )

### Variables ACP
fviz_pca_var(Attacking_Phase.ACP,
             axes = c(1,2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "ACP - Correlation Circle") +
  labs(color = "Variables' contribution (%)") +
  theme(legend.position = "none",
    plot.title = element_text(
    size = 15,     
    hjust = 0.5
    )
  )

#### Variables Contribution and quality of representation through x dimensions 
Attacking_Phase.ACP$var$contrib
Attacking_Phase.ACP$var$cos2

#### Top variables qui caractérisent PC1
sort(Attacking_Phase.ACP$var$contrib[,1], decreasing = TRUE)[1:5]

#### Top variables qui caractérisent PC2
sort(Attacking_Phase.ACP$var$contrib[,2], decreasing = TRUE)[1:5]

### Players ACP
fviz_pca_ind(Attacking_Phase.ACP,
             geom.ind = "point", 
             col.ind = "cos2",   
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # évite le chevauchement des labels
             title = "ACP - Players' positioning through the first 2 dimensions") +
labs(color = "Players' quality of representation (cos2)") +
  theme(legend.position = "none",
    plot.title = element_text(
      size = 15,     
      hjust = 0.5
    )
  )

### Hierarchic Classification on principal components (HCPC)
res.hcpc <- HCPC(Attacking_Phase.ACP, graph = TRUE)

names(res.hcpc)
res.hcpc$desc.var
res.hcpc$desc.ind 
res.hcpc$desc.axes 
res.hcpc$data.clust$clust

aggregate(Attacking_Phase_num_var, by = list(Cluster = res.hcpc$data.clust$clust), FUN = mean)

### Elbow method for optimal clustering calculation
set.seed(123) 
fviz_nbclust(Attacking_Phase_num_var, 
             kmeans, 
             method = "wss") + 
  labs(subtitle = "Méthode du coude pour le nombre optimal de clusters")

### Players clustering
fviz_cluster(list(data = Attacking_Phase.ACP$ind$coord[, 1:2], cluster = res.hcpc$data.clust$clust),
             ellipse.type = "convex",
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE,               # Montre le centre des clusters
             geom = "point",
             palette = "npg",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Players' clustering according to the first 2 dimensions") +
  theme(legend.position = 'none',
        plot.title = element_text(
          size = 15,     
          hjust = 0.5)
        )


pca_coords <- as.data.frame(Attacking_Phase.ACP$ind$coord[, 1:2])
colnames(pca_coords) <- c("Dim.1", "Dim.2")
pca_coords$Cluster <- as.factor(res.hcpc$data.clust$clust)
pca_coords$Players <- Attacking_Phase$Players

hulls <- pca_coords %>%
  group_by(Cluster) %>%
  slice(chull(Dim.1, Dim.2))  # prend les points du contour

ggplot(pca_coords, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_polygon(data = hulls, aes(fill = Cluster, group = Cluster), 
               alpha = 0.15, color = "black", linetype = "dashed") +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Players), size = 3, max.overlaps = Inf) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Players' clustering according to the first 2 dimensions",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5))

### Players clustering and variables biplot
fviz_pca_biplot(Attacking_Phase.ACP,
               geom.ind = "point",        # points pour les joueurs
               col.ind = res.hcpc$data.clust$clust,  # couleur par cluster
               palette = "Set1",
               addEllipses = TRUE,        # ellipses pour les clusters
               label = "var",             # flèches des variables
               repel = TRUE,              # éviter chevauchement
               title = "Biplot PCA : joueurs et variables avec clusters")


### Statistical analysis of variables' mean differences between clusters
#### Mutate dataset with clustering analysis
df <- Attacking_Phase_num_var %>%
  mutate(Cluster = res.hcpc$data.clust$clust)

#### Statistical tests selection
get_stats <- function(var, df) {
  x <- df[[var]][df$Cluster == 1]
  y <- df[[var]][df$Cluster == 2]
  
  # Shapiro-Wilk test
  shapiro1 <- shapiro.test(x)$p.value
  shapiro2 <- shapiro.test(y)$p.value
  
  # Between groups statistical test selection
  if (shapiro1 > 0.05 & shapiro2 > 0.05) {
    test <- t.test(x, y)
  } else {
    test <- wilcox.test(x, y)
  }
  
  # Mean and SD calculation
  mean1 <- mean(x, na.rm = TRUE)
  sd1   <- sd(x, na.rm = TRUE)
  mean2 <- mean(y, na.rm = TRUE)
  sd2   <- sd(y, na.rm = TRUE)
  
  # p-Value formating
  if (test$p.value < 0.001) {
    pval <- "<0.001"
  } else if (test$p.value < 0.01) {
    pval <- "<0.01"
  } else if (test$p.value < 0.05) {
    pval <- "<0.05"
  } else {
    pval <- ">0.05"
  }
  
  data.frame(
    Variable = var,
    Cluster1 = paste0(round(mean1,1), " ± ", round(sd1,1)),
    Cluster2 = paste0(round(mean2,1), " ± ", round(sd2,1)),
    `p.value` = pval,
    stringsAsFactors = FALSE
  )
}

#### Keep significative differences only (p-value < 0.05)
results <- lapply(names(Attacking_Phase_num_var), get_stats, df = df) %>%
  bind_rows()
results_sig <- results %>% filter(`p.value` != ">0.05")

#### Cluster comparison table for Ppoint
results_sig %>%
  kable("html") %>%
  kable_styling(
    full_width = F, 
    bootstrap_options = c("striped", "hover", "condensed"),
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#ff7f0e") 


#### Cluster comparison interactive table
Cluster_Comparison <- datatable(results_sig,
                                rownames = FALSE,
                                options = list(pageLength = 10, scrollX = TRUE),
                                caption = 'Tableau interactif : comparaison des variables entre clusters')
saveWidget(Cluster_Comparison, "Comparaison_Clusters.html", selfcontained = TRUE)

