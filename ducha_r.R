

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(showtext)
library(hrbrthemes)


#Densities

densities <- read_excel("C:\\Users\\braga\\OneDrive\\Área de Trabalho\\Felipe\\Doutorado\\Textos\\Duchamp, o retorno do Jedi\\Densities.xlsx")

densities <- colnames(c("Nome da Rede", "Tipo", "Nodos", "Relações", "Densidade", "Diferença"))

ggplot(densities, aes(x= Nodos, y = Densidade)) +
  geom_point() +
  theme_bw() +
  geom_point(aes(x= 435, y = 0.048, color = "red"), size = 2, show.legend = FALSE) +
  geom_point(aes(x= 435, y = 0.024, color = "blue"), size = 2, show.legend = FALSE) +
  geom_point(aes(x= 435, y = 0.012, color = "green"), size = 2, show.legend = FALSE) +
  ggtitle("Distribuição de densidades de 21 redes sociais") +
  labs(subtitle= "Intervalo de densidades entre 0 e 0.1", x= "Número de Nodos") +
  ylim(0, 0.11) +
  geom_smooth(se = FALSE, color = "gray")


#Degree

ducha <- read_excel("C:\\Users\\braga\\OneDrive\\Área de Trabalho\\Felipe\\Doutorado\\Textos\\Duchamp, o retorno do Jedi\\Dados para R.xlsx")
ducha <- ducha %>%
  mutate(Anos = as.numeric(Anos), 
         Ocupação = as.factor(Ocupação),
         Gender = as.factor(Gender),
         Nascimento = as.numeric(Nascimento),
         Morte = as.numeric(Morte))
str(ducha)
         



ducha %>%
  filter(Degree < 400) %>%
  ggplot(aes(x= Diferença, y = Degree)) +
  geom_point() +
  theme_classic() +
  geom_vline(xintercept =  0, color = "gray") +
  labs(title= "Gráfico X: Geração versus grau nodal", subtitle = "Diferença entre a idade de Duchamp e os nodos da rede", x = "Geração", y = "Grau nodal") +
  annotate("text", x = -30, y = 115, label = "Mais velho que Duchamp", fontface = 2, color = "bisque4") +
  annotate("text", x = 33, y = 115, label = "Mais novo que Duchamp", fontface = 2, color = "bisque4")




degree_ocupacao <- ducha %>%
  group_by(Ocupação) %>%
  summarise(n(), mean(Degree))


colnames(degree_ocupacao) <- c("Ocupação", "Numero", "Degree")
ocupacao_filtro <- degree_ocupacao[degree_ocupacao$Numero>7,]

ocupacao_filtro <- ocupacao_filtro[order(ocupacao_filtro$Degree, decreasing = TRUE),]

ocupacao_filtro <- ocupacao_filtro %>%
  filter(Ocupação != "NA")
ocupacao_filtro



ocupacao_filtro %>%
  mutate(Degree_round = round(Degree, 0)) %>%
ggplot(aes(x = Ocupação, y = Degree_round)) +
  geom_col(color = "pink") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  labs(title = "Gráfico X: Grau nodal médio versus ocupações", subtitle = "Grau nodal médio para as 8 ocupações mais frequentes", y = "Grau nodal médio") +
  geom_text(aes(label = Degree_round), vjust = 2, color = "white")

  


ocupacao_filtro %>%
  mutate(Degree_round = round(Degree, 0)) %>%
  ggplot(aes(x = Ocupação, y = Degree_round)) +
  geom_col(color = "black", fill = "white") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank()
    ) +
  labs(title = "Gráfico X: Grau nodal médio versus ocupações", subtitle = "Grau nodal médio para as 8 ocupações mais frequentes", y = "Grau nodal médio") +
  geom_text(aes(label = Degree_round), vjust = 2, color = "black")






#Gender

str(ducha)

ocupações_rotulos <- c("Artista", "Ator", "Colecionador de Arte", "Crítico de arte", "Curador", "Escritor", "Fotógrafo", "Galerista")

ducha %>%
  filter(Ocupação %in% ocupações_rotulos) %>%
  ggplot(aes(x = Ocupação, fill = Gender)) +
  geom_bar(color = "black") + 
  theme_classic() +
  scale_fill_manual(
    
      values = c(
      
      "Feminino" = "darkgoldenrod1",
      "Masculino" = "darkmagenta"
      
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
  ) +
  labs(title = "Gráfico X: Número de nodos por ocupação", x = "Ocupação", y = "Contagem")
  

#Degree distribution


str(ducha)

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(reorder(Label, -Degree), y = Degree)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank() 
  ) +
  labs(title = "Gráfico X: Distribuição de graus nodais")


#Ano de nascimento

ducha %>%
  filter(Nascimento != "NA") %>%
  ggplot(aes(x = Nascimento, fill = Gender)) +
  geom_bar(alpha = 2) +
  scale_fill_manual(
    
    values = c(
      
      "Feminino" = "darkgoldenrod1",
      "Masculino" = "darkmagenta"
      
    )
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  annotate("text", x = 4, y = 11, label = "1821", fontface = 2, color = "black") +
  annotate("text", x = 42, y = 11, label = "1894", fontface = 2, color = "black") +
  annotate("text", x = 80, y = 11, label = "1943", fontface = 2, color = "black") +
  annotate("rect", xmin = "1882", xmax = "1892", ymin = 0, ymax = 12, alpha = 0.1) +
  labs(y = "Contagem", title = "Gráfico X: Número de nascimentos por ano", subtitle = "Área cinza: 5 anos antes e cinco anos depois de 1887")
  


m <- as.data.frame(table(ducha$Degree))

str(m)

m[,1] <- as.numeric(m[,1])

ggplot(m, aes(x = Var1, y = Freq)) +
  geom_line()

ducha
str(ducha)



ducha %>%
  filter(Label != "Marcel Duchamp") %>%
ggplot(aes(y = Eigenvect, x = Degree)) +
  geom_point()

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(y = Closeness, x = Degree)) +
  geom_point()


ducha %>%
  filter(Morte != "NA") %>%
  mutate(Anos = as.numeric(Morte) - as.numeric(Nascimento)) %>%
  #filter(Morte <= 1968) %>%
  ggplot(aes(x = Anos)) +
  geom_histogram()
  #ggplot(aes(y = Morte, x = Label)) +
  #geom_col()


death <- read_excel("C:\\Users\\braga\\OneDrive\\Área de Trabalho\\Felipe\\Doutorado\\Textos\\Duchamp, o retorno do Jedi\\death_graph.xlsx")

str(death)


#trapézio das mortes


death %>%
  filter(Morte <= 1968) %>%
  ggplot(aes(x= reorder(Label,-Morte), y= Plot, fill = Categoria)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    
    values = c(
      
      "1_Anos" = "#69b3a2",
      "Barra_1" = "white"
      
    )) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
    ) + 
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  #linha primeira morte geom_hline(yintercept = 66 + 21, linetype = "longdash") +
  annotate("text", x = 6, y = 66, label = "1887", fontface = 2, size = 6) +
  annotate("text", x = 6, y = 66 + 81, label = "1968", fontface = 2, size = 6) +
  annotate("text", x = 6, y = 1, label = "1821", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 21, label = "1841", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 41, label = "1861", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 66 + 27, label = "1914", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 66 + 26 + 26, label = "1939", fontface = 2, size = 4)



#trapézio das mortes com inclinações I1 e I2

death %>%
  filter(Morte <= 1968) %>%
  ggplot(aes(x= reorder(Label,-Morte), y= Plot, fill = Categoria)) +
  geom_col(alpha = 0.3) +
  coord_flip() + #há aqui um gráfico de barras; esse comando faz com que o gráfico apresente as barrinhas na horizontal, e não na vertical; logo, o "y é x", e o "x é y"
  scale_fill_manual(
    
    values = c(
      
      "1_Anos" = "#69b3a2",
      "Barra_1" = "white"
      
    )) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) + 
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  #linha primeira morte geom_hline(yintercept = 66 + 21, linetype = "longdash") +
  annotate("text", x = 6, y = 66, label = "1887", fontface = 2, size = 6) +
  annotate("text", x = 6, y = 66 + 81, label = "1968", fontface = 2, size = 6) +
  annotate("segment", x = 174, xend = 127, y = 96, yend = 123, colour = "black", linewidth = 0.5) +
  annotate("segment", x = 127, xend = 6, y = 123, yend = 66 + 81, colour = "black", linewidth = 0.5) +
  annotate("text", x = 152, y = 115, label = "I1", fontface = 2, size = 4) +
  annotate("text", x = 66, y = 140, label = "I2", fontface = 2, size = 4) +
  annotate("segment", x = 127, xend = 9, y = 123, yend = 123, colour = "black", linewidth = 0.5) +
  annotate("text", x = 6, y = 123, label = "1944", fontface = 2, size = 6)

ggsave("trapezio das mortes 1944.jpg")



#trapézio das mortes, organizado por nascimentos


str(ducha)

death %>%
  #filter(Morte <= 1968) %>%
  ggplot(aes(x= reorder(Label,-Nascimento), y= Plot, fill = Categoria)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    
    values = c(
      
      "1_Anos" = "#69b3a2",
      "Barra_1" = "white"
      
    )) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) + 
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  #linha primeira morte geom_hline(yintercept = 66 + 21, linetype = "longdash") +
  annotate("text", x = 9, y = 66, label = "1887", fontface = 2, size = 5) +
  annotate("text", x = 9, y = 66 + 81, label = "1968", fontface = 2, size = 5) +
  annotate("text", x = 9, y = 1, label = "1821", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 21, label = "1841", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 41, label = "1861", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 66 + 27, label = "1914", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 66 + 26 + 26, label = "1939", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 66 + 81 + 26, label = "1994", fontface = 2, size = 3) +
  annotate("text", x = 9, y = 66 + 81 + 26 + 26, label = "2020", fontface = 2, size = 3)

ggsave("trapezio nascimentos.jpg")

1968 + 26 + 26




#trapézio das mortes


death %>%
  filter(Morte <= 1968) %>%
  ggplot(aes(x= reorder(Label,-Morte), y= Plot, fill = Categoria)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    
    values = c(
      
      "1_Anos" = "#69b3a2",
      "Barra_1" = "white"
      
    )) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) + 
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  #linha primeira morte geom_hline(yintercept = 66 + 21, linetype = "longdash") +
  annotate("text", x = 6, y = 66, label = "1887", fontface = 2, size = 6) +
  annotate("text", x = 6, y = 66 + 81, label = "1968", fontface = 2, size = 6) +
  annotate("text", x = 6, y = 1, label = "1821", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 21, label = "1841", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 41, label = "1861", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 66 + 27, label = "1914", fontface = 2, size = 4) +
  annotate("text", x = 6, y = 66 + 26 + 26, label = "1939", fontface = 2, size = 4)




#trapézio das mortes, colorido by degree



  
ggplot(NULL, aes(reorder(Label, -Morte), Plot)) +
  geom_col(data = death[death$Morte<=1968&death$Label!="Marcel Duchamp",], aes(fill = Degree)) +
  scale_fill_gradient2(
    low = "#69b3a2", 
    mid = "darkorange", 
    high = "firebrick2", 
    midpoint = 60
  ) +
  #scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  geom_col(data = death[death$Categoria == "1_Anos"&death$Morte<=1968&death$Label!="Marcel Duchamp",], fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  annotate("text", x = 6, y = 66, label = "1887", fontface = 2, size = 6, color = "black") +
  annotate("text", x = 6, y = 66 + 81, label = "1968", fontface = 2, size = 6, color = "black")
  


ggplot(NULL, aes(reorder(Label, -Morte), Plot)) +
  geom_col(data = death[death$Morte<=1968&death$Label!="Marcel Duchamp",], aes(fill = Degree)) +
  scale_fill_gradient2(
    low = "#69b3a2", 
    mid = "darkorange", 
    high = "firebrick2", 
    midpoint = 60
  ) +
  #scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  geom_col(data = death[death$Categoria == "1_Anos"&death$Morte<=1968&death$Label!="Marcel Duchamp",], fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 66, linetype = "longdash") +
  geom_hline(yintercept = 66 + 81, linetype = "longdash") +
  geom_hline(yintercept = 66 + 21, linetype = "longdash") +
  annotate("text", x = 6, y = 66, label = "1887", fontface = 2, size = 6, color = "black") +
  annotate("text", x = 6, y = 66 + 81, label = "1968", fontface = 2, size = 6, color = "black")



  
ducha[ducha$Morte == min(ducha$Morte),]


#degree 

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = reorder(Label, -Degree), y = Degree)) +
  geom_col() +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
    )

#degree versus betweeness

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree, y = Between)) +
  geom_point() + 
  theme_classic() +
  theme(
    axis.ticks.x = element_blank()
  )

ducha[ducha$Between>700,]



#degree versus closeness    
ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree, y = Closeness)) +
  geom_point() + 
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#degree versus eigenvct
ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree, y = Eigenvect)) +
  geom_point() + 
  theme_classic() +
  theme(
    #axis.text.x = element_blank()
    axis.ticks.x = element_blank()
  )


ducha[ducha$Eigenvect>0.17&ducha$Degree>80,]



str(ducha)


ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 3)

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 4)

ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 5)


ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 6)


ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 7)



ducha %>%
  filter(Label != "Marcel Duchamp") %>%
  ggplot(aes(x = Degree)) +
  geom_histogram(bins = 40)




nrow(ducha[ducha$Degree>26,])/nrow(ducha)*100



#ducha, análise das mortes


str(ducha)

ducha %>%
  filter(Nascimento != "NA", Morte != "NA") %>%
  ggplot(aes(x = as.numeric(Nascimento), y = as.numeric(Morte))) +
  geom_point(shape = 1, color = "red", size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1821,1943,30)) +
  xlab("Ano de Nascimento") +
  ylab("Ano de Morte")

  ggsave("Nasc_mort_geompoint.png")


  #nascimento e morte, geom-point com linha de regressão
  
  ducha %>%
    filter(Nascimento != "NA", Morte != "NA") %>%
    ggplot(aes(x = as.numeric(Nascimento), y = as.numeric(Morte))) +
    geom_point(shape = 1, color = "red", size = 1.5) +
    theme_bw() +
    scale_x_continuous(breaks = seq(1821,1943,30)) +
    xlab("Ano de Nascimento") +
    ylab("Ano de Morte") +
    stat_smooth(method = "lm",
               geom = "smooth",
               formula = y ~ x,
               fill = "grey",
               color = "darkblue",
               alpha = 1, 
               se = F)
  
    ggsave("Nasc_mort_geompoint_lm.png")
  
  
  
#correlação de Pearson entre ano de nascimento e ano de morte

cor(as.numeric(ducha$Nascimento), as.numeric(ducha$Morte), method = "pearson", use = "complete.obs")
  
#modelo de regressão lm para nascimento e morte

lm_ducha <- lm(as.numeric(ducha$Morte)~as.numeric(ducha$Nascimento))
summary(lm_ducha)


str(ducha)


ducha %>%
  filter(Anos != "NA") %>%
  mutate(Faixa = ifelse(Anos > 40 & Anos < 59, "Entre 40 e 59", ifelse(Anos > 60 & Anos < 79, "Entre 60 e 79", ifelse(Anos > 80 & Anos < 99, "Entre 80 e 99", "Maior que 99")))) %>%
  filter(Morte <= 1968) %>%
  ggplot(aes(x = Morte, fill = Faixa)) +
  geom_bar() +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,9,1)) 
  #scale_x_continuous(breaks = seq(as.numeric(min(ducha$Morte)),1968,10))
  


#gráfico boxplot com as mortes da geração de Duchamp (nascimentos entre 1885 e 1889)


ducha %>%
  filter(Nascimento >= 1885, Nascimento <= 1889) %>%
  ggplot(aes(y = as.numeric(Morte))) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(1915, 1995,5)) +
  theme_minimal() +
  ylab("Ano de Falecimento") +
  xlab("Geração de Duchamp") +
  theme(axis.text.x = element_blank())


# boxplot mortes de alters antes de 1968 (mortes precoces)


ducha %>%
  filter(Anos != "NA") %>%
  mutate(Anos = as.numeric(Anos)) %>%
  #filter(Morte <= 1968) %>%
  ggplot(aes(y=Anos, x = Gender)) +
  geom_boxplot(aes(fill = Gender), outlier.shape = NA) +
  geom_jitter(size = 1) +
  scale_fill_manual(values = c("darkslateblue", "coral3")) +
  scale_y_continuous(breaks = seq(30,105,5)) +
  ylab("Idade ao falecer") +
  theme_get() +
  theme(axis.text.x = element_blank()) +
  annotate("rect", xmin = 0.6, xmax = 1.4, ymin = 40, ymax = 50, alpha = 0.2) +
  annotate("rect", xmin = 1.6, xmax = 2.4, ymin = 30, ymax = 40, alpha = 0.2)

ggsave("boxplot_mortes precoces.jpg", dpi = 300)

ducha %>%
  filter(Anos != "NA") %>%
  mutate(Anos = as.numeric(Anos)) %>%
  #filter(Morte <= 1968) %>%
  ggplot(aes(y=Anos, x = Gender)) +
  geom_boxplot(aes(fill = Gender), outlier.shape = NA) +
  geom_violin() +
  scale_y_continuous(breaks = seq(30,105,5)) +
  ylab("Idade ao falecer") +
  theme_get() +
  theme(axis.text.x = element_blank()) +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 40, ymax = 50, alpha = 0.2) +
  annotate("rect", xmin = 1.7, xmax = 2.3, ymin = 30, ymax = 40, alpha = 0.2)



ducha %>%
  filter(Anos != "NA") %>%
  ggplot(aes(x=as.numeric(Anos))) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(30,105,5)) +
  xlab("Idade ao falecer") +
  theme_get() +
  theme(axis.text.y = element_blank())


summary(ducha$Ocupação)




#análise mortes precoces


str(ducha)

mortes_precoces <- ducha %>%
  filter(Anos <= 47, as.numeric(Morte) <= 1968) %>%
  arrange(as.numeric(Anos)) %>%
  select(Label, Gender, Degree, Nascimento, Morte, Ocupação, `Classe ocupação`, Anos)
  
str(mortes_precoces)


Causa <- c("Ruptura de apêndice", "Desconhecido", "Suicidio", "Pandemia de 1918", "Pandemia de 1918", "Intoxicação Alimentar", "Tuberculose", "Falência dos rins", "Primeira Guerra Mundial", "Ruptura de apêndice", "Pneumonia", "Suicidio", "Desconhecido", "Pneumonia","Suicidio", "Segunda Guerra Mundial", "Suicidio","Ataque cardiaco","Infecção no ouvido", "NA", "NA", "Embolia pulmonar")


mortes_precoces[,9] <- Causa
colnames(mortes_precoces)[9] <- "Causa_morte"
str(mortes_precoces)



#proporções de ocupações artísticas e não-artísticas nas mortes precoces (mortes com menos de 47 anos)
(table(mortes_precoces$`Classe ocupação`))/(nrow(mortes_precoces))

#proporções de ocupações artísticas e não-artísticas na rede como um todo
(table(ducha$`Classe ocupação`[ducha$`Classe ocupação`!="NA"]))/(length(ducha$`Classe ocupação`[ducha$`Classe ocupação`!="NA"]))

length(ducha$Ocupação[ducha$Ocupação!="NA"])

(table(ducha$`Classe ocupação`[ducha$`Classe ocupação`!="NA"]))/(length(ducha$`Classe ocupação`[ducha$`Classe ocupação`!="NA"]))

0.6441103*399

nrow(ducha[ducha$Ocupação!="NA",])


ducha$Ocupação

#loop experimento com 1000 amostra de 22 pessoas para descobrir a proporção de ocupações artística na rede

oi2 <- c()

for (i in 1:10000) {

  #sortear 22 classes de ocupação
  oi <- sample(ducha$`Classe ocupação`[ducha$`Classe ocupação`!="NA"],22,replace = T)
  #dividir o total de artistas pelo total de artistas + não-artistas
  oi2[i] <- table(oi)[1]/(table(oi)[1]+table(oi)[2])*100
  
}


hist(oi2, main = NULL, xlab = "Proporção de ocupações artísticas", ylab = NULL, breaks = 19)



length(oi2[oi2>=81])/length(oi2)*100

#salvar as proporções do experiemento em um data.frame, a fim de trabalhá-lo no ggplot2
experimento_prop <- data.frame(1:10000, oi2)


#gráfico de barras das proporções de artistas (retângulo rosa na média)

ggplot(experimento_prop, aes(x = oi2)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(25, 100, 5)) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.y = element_blank()) +
  annotate("rect", 
           xmin = mean(experimento_prop$oi2) - sd(experimento_prop$oi2), 
           xmax = mean(experimento_prop$oi2) + sd(experimento_prop$oi2), 
           ymin = 0, 
           ymax = 2000, 
           fill = "pink", 
           alpha = 0.4)


#gráfico de barras das proporções de artistas (retângulo rosa no + 81)

ggplot(experimento_prop, aes(x = oi2)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(25, 100, 5)) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.y = element_blank()) +
  annotate("rect", 
           xmin = 81, 
           xmax = 100, 
           ymin = 0, 
           ymax = 450, 
           fill = "pink", 
           alpha = 0.5)



#quantas das 10 mil amostras de 22 pessoas possuem mais do que 81% de artistas?

(length(experimento_prop$oi2[experimento_prop$oi2>81])/10000)*100




#se "curador" fosse artista, qual seria a probabilidade de a proporção de artistas nas mortes precoces serem produto de mero acaso?


ducha_2 <- ducha %>%
  mutate(`Classe ocupação` = ifelse(ducha$Ocupação=="Curador","Artística",`Classe ocupação`))
  
  
  oi3 <- c()

for (i in 1:10000) {
  
  #sortear 22 classes de ocupação
  oi <- sample(ducha_2$`Classe ocupação`[ducha_2$`Classe ocupação`!="NA"],22,replace = T)
  #dividir o total de artistas pelo total de artistas + não-artistas
  oi3[i] <- table(oi)[1]/(table(oi)[1]+table(oi)[2])*100
  
}

(length(oi3[oi3>81])/10000)*100

  
  
  
ducha$Ocupação  


#grupo da geração e grupo da degeneração 

str(ducha)

geracao_ducha <- ducha %>%
  filter(Nascimento >= 1884, Nascimento <= 1890) %>%
  arrange(`ID Node`)


degeneracao_ducha <- ducha %>%
  filter(Morte >= 1965, Morte <= 1971) %>%
  arrange(`ID Node`)


nrow(degeneracao_ducha)
nrow(geracao_ducha)

gerecao_degeneracao <- geracao_ducha %>%
  filter(Label %in% degeneracao_ducha$Label)


nrow(gerecao_degeneracao)/nrow(degeneracao_ducha)

ducha %>%
  filter(Nascimento == 1887) %>%
  select(Label, Gender, Nascimento, Morte, Anos)


str(ducha)


#gráfico mostrando a diferença entre a idade de falecimento de um dos sujeitos da rede de Duchamp, e a idade de Duchamp no ano em que o sueito faleceu

ducha %>%
  mutate(Anos_Ducha = Morte - 1887, 
         Dif = Anos - Anos_Ducha) %>%
  filter(Anos_Ducha != "NA", 
         Morte <= 1968,
         Dif < 65) %>%
  #summarise(cor(Morte, Dif)) %>%
  ggplot(aes(x = Morte, y = Dif)) +
  geom_point(shape = 1, color = "red", size = 2) +
  scale_y_continuous(breaks = seq(-35, 70, 5)) +
  scale_x_continuous(breaks = seq(1908,1968,5)) +
  theme_bw() +
  ylab("Diferença entre a idade da perda e a idade de Duchamp") +
  xlab("Ano")
  

ggsave("Diferença entre a idade da perda e a idade de Duchamp.png")


#correlação linear (R de Pearson) entre a variável ano de falecimento e a diferença entre a idade de Duchamp e a idade do sujeito que faleceu
ducha %>%
  mutate(Anos_Ducha = Morte - 1887, 
         Dif = Anos - Anos_Ducha) %>%
  filter(Anos_Ducha != "NA", 
         Morte <= 1968,
         Dif < 65) %>%
  summarise(cor(Morte, Dif))


ducha %>%
  filter(Morte <= 1968)


hist(ducha$Morte[ducha$Morte<=1968], col = "skyblue3", breaks = 21)
hist(ducha$Nascimento[ducha$Morte<=1968], col = "skyblue3", breaks = 21)  
  
  
ducha %>%
  filter(Morte <= 1968) %>%
  ggplot(aes(x = Morte)) +
  geom_histogram(bins = 30, fill = "white", color = "black") +
  geom_vline(xintercept = 1917, colour = "limegreen", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 1944, colour = "limegreen", size = 1.5, alpha = 0.7) +
  theme_update() +
  scale_x_continuous(breaks = seq(1900, 1970, 10)) +
  scale_y_continuous(breaks = seq(0,35,5)) +
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 1920, y = 16, label = "I1", color = "limegreen", size = 6) +
  annotate("text", x = 1947, y = 16, label = "I2", color = "limegreen", size = 6)


#número de mortes por ano, incluindo os anos com 0 mortes, entre 1887 e 1968

num_mortes_ano <- as.data.frame(ducha %>%
  filter(Morte <= 1968,
         Morte != "NA") %>%
  group_by(Morte) %>%
  summarise(n()))


colnames(num_mortes_ano)[1] <- "Ano"

#lista com todos os anos, entre 1887 e 1968
num_mortes_zero <- data.frame(Ano = c(1887:1968), Oi = "NA")

#emendar os dois dataframes: anos com mortes, anos sem mortes
num_mortes_ano <- merge(num_mortes_ano, num_mortes_zero, by = "Ano", all = T)

#colocar 0 nos anos em que não há mortes 
num_mortes_ano[is.na(num_mortes_ano)] <- 0

#visualização

num_mortes_ano %>%
  ggplot(aes(x = Ano, y = Num_mortes)) +
  geom_col(fill = "black", color = "white", width = 1.2) +
  scale_x_continuous(breaks = seq(1887, 1968, 9)) +
  geom_vline(xintercept = 1917, colour = "#69b3a2", size = 1.5, alpha = 0.9) +
  geom_vline(xintercept = 1944, colour = "#69b3a2", size = 1.5, alpha = 0.9) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,9,1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 1914, y = 9, label = "I1", color = "#69b3a2", size = 6) +
  annotate("text", x = 1941, y = 9, label = "I2", color = "#69b3a2", size = 6)

  
  
  

#histogram nascimentos da rede com curva de densidade

ducha %>%
  filter(Nascimento >= 1840) %>%
  ggplot(aes(x = Nascimento)) +
  geom_histogram(aes(y=..density..), bins = 50, fill = "white", color = "black") +
  geom_density(alpha=.2, fill="#FF6666") +
  #geom_vline(xintercept = 1887, colour = "limegreen", size = 1.5) +
  scale_x_continuous(breaks = seq(1850, 1950, 10)) +
  scale_y_continuous(breaks = seq(0,20)) +
  theme(axis.title.y = element_blank())

ggsave("Nascimentos.jpg")


#histogram nascimentos da rede, com indicação do ano de nascimento de Duchamp

ducha %>%
  filter(Nascimento >= 1840) %>%
  ggplot(aes(x = Nascimento)) +
  geom_histogram(bins = 50, fill = "white", color = "black") +
  geom_vline(xintercept = 1887, colour = "limegreen", size = 1.5) +
  scale_x_continuous(breaks = seq(1850, 1950, 10)) +
  scale_y_continuous(breaks = seq(0,20)) +
  theme(axis.title.y = element_blank())


#histogram de mortes da rede

ducha %>%
  filter(Nascimento >= 1840) %>%
  ggplot(aes(x = Morte)) +
  geom_histogram(bins = 37, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(1915,2025,10)) +
  theme(axis.title.y = element_blank())


#proporção de mortes entre os anos de 1944 e 1994

round(
(nrow( ducha %>%
  filter(Morte >= 1944, Morte <= 1994, Morte != "NA")))/
  (nrow( ducha %>% filter(Morte != "NA"))) * 100, 2
)



#histogram de mortes da rede, com indicação do ano em que Duchamp morreu

ducha %>%
  filter(Nascimento >= 1840) %>%
  ggplot(aes(x = Morte)) +
  geom_histogram(bins = 40, fill = "white", color = "black") +
  geom_vline(xintercept = 1968, colour = "limegreen", size = 1.5) +
  scale_x_continuous(breaks = seq(1915,2025,10)) +
  theme(axis.title.y = element_blank())
  
str(ducha)
  
# número de mortes por ano

mortes_anos <- ducha %>%
  group_by(Morte) %>%
  summarise(n())

# renomear nomes das colunas de "mortes_anos"

colnames(mortes_anos) <- c("Ano", "Num_mortes")


mortes_anos %>%
  filter(Ano <= 1968) %>%
  ggplot(aes(x = Ano, y = Num_mortes)) +
  geom_line(color="grey") +
  geom_point(shape=21, fill="#69b3a2", size=3, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1908,1968,6)) +
  scale_y_continuous(breaks = seq(1, 10)) +
  geom_vline(xintercept = c(1917,1944,1968), colour = "limegreen", size = 0.8, alpha = 0.8) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text", label = "1917", x = 1917, y = 0.5, fontface = 7) +
  annotate("text", label = "1944", x = 1944, y = 0.5, fontface = 7) +
  annotate("text", label = "1968", x = 1968, y = 0.5, fontface = 7)

ggsave("tempo das perdas.jpg")


#calcular os anos-lacuna


#tranformar a sequência de anos de perda em um vector
anos <- mortes_anos %>% 
  filter(Ano != "NA") %>%
  pull(Ano)

#calcular anos-lacuna


#anos-lacuna antes de 1916 (anos com morte antes de 1916, total de anos entre 1887 e 1916)
1 - (length(anos[anos < 1917])) / (length(seq(1887,1916)))
#anos-lacuna entre 1917 e 1943 (anos com morte antes entre 1917 e 1944, total de anos entre 1917 e 1943)
1 - (length(anos[anos < 1944 & anos >= 1917])) / (length(seq(1917,1943)))
#anos-lacuna entre 1944 e 1968 (anos com morte antes entre 1944 e 1968, total de anos entre 1944 e 1968)
1 - (length(anos[anos >= 1944 & anos <= 1968])) / (length(seq(1944,1968)))



#número de nascimentos por ano

nascimentos_anos <- ducha %>%
  group_by(Nascimento) %>%
  summarise(n())

# renomear nomes das colunas de "nascimentos_anos"

colnames(nascimentos_anos) <- c("Ano", "Num_nasci")

nascimentos_anos %>%
  filter(Ano %in% c(1875:1890))

oi <- nascimentos_anos %>%
  filter(Num_nasci != "NA", Ano != "NA", Ano != 1821) %>%
  ggplot() +
  geom_area(aes(x = Ano, y = Num_nasci))
  
  
oi2 <- mortes_anos %>%
  ggplot(aes(x = Ano, y= Num_mortes)) +
  geom_area()

grid.arrange(oi, oi2, ncol = 1, nrow = 2)
  
  geom_line() +
  geom_point(shape=21, fill="#69b3a2", size=2, alpha = 0.7) +
  theme_bw() +
  theme(axis.title.y = element_blank())


  
#análise ordem dos nascimentos
  
  ordem_nasci <- ducha %>%
    filter(Nascimento != "NA") %>%
    mutate(Ordem_nasci = ifelse(Nascimento < 1887, "Antes", ifelse(Nascimento == 1887, "Ducha", "Depois"))) %>%
    select(Label, Ordem_nasci, Nascimento, Gender)
  
  
#quantas pessoas naceram no ano de Duchamp  
length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Ducha"])/nrow(ordem_nasci)

#quantas pessoas nasceram antes que Duchamp
length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Antes"])/nrow(ordem_nasci)  

#quantas pessoas nasceram depois que Duchamp
length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Depois"])/nrow(ordem_nasci)  

#total
(length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Ducha"])/nrow(ordem_nasci)) + (length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Antes"])/nrow(ordem_nasci)) + (length(ordem_nasci$Ordem_nasci[ordem_nasci$Ordem_nasci=="Depois"])/nrow(ordem_nasci))



#quantas pessoas nasceram entre os anos 1882 e 1892 (geração Ducha?)
nrow(ducha %>% filter(Nascimento >= 1882, Nascimento <= 1892, Label != "Marcel Duchamp")) / nrow(ducha %>% filter(Nascimento != "NA", Label != "Marcel Duchamp"))
#quantas pessoas nasceram entre os anos 1877 e 1897 (geração Ducha extendida?)
nrow(ducha %>% filter(Nascimento >= 1877, Nascimento <= 1897, Label != "Marcel Duchamp")) / nrow(ducha %>% filter(Nascimento != "NA", Label != "Marcel Duchamp"))
  

#porcentagem de sujeitos que pertecem à geração de Duchamp e à degeneração de Duchamp

length(
(ducha %>% 
    filter(
      Nascimento >= 1877, 
      Nascimento <= 1897,
      Morte != "NA") %>% 
    pull(Label))[
(ducha %>% 
  filter(
    Nascimento >= 1877, 
    Nascimento <= 1897,
    Morte != "NA") %>% 
  pull(Label)) %in% 
  (ducha %>% 
  filter(
    Morte >= 1958,
    Morte <= 1978,
    Nascimento != "NA") %>%
  pull(Label))
    ]) / length(ducha %>% 
         filter(
           Nascimento >= 1877, 
           Nascimento <= 1897,
           Morte != "NA") %>% 
         pull(Label)) * 100

1 - (length(
ducha %>%
  filter(Nascimento < 1887,
         Nascimento != "NA") %>%
  pull(Label)) /
  length(
  ducha %>%
  filter(Nascimento != "NA") %>%
    pull(Label)))
  

#gráfico de dipersão de pontos com a diferença de idade entre Duchamp e cada alter (eixo x), e a quantidade de relações em comum (eixo y)

install.packages("showtext")
font_add_google("Special Elite", family = "special")
showtext_auto()

ducha %>%
  filter(Label != "Marcel Duchamp", Nascimento != "NA") %>%
  ggplot(aes(x = Nascimento - 1887, y = Degree)) +
  geom_point(shape = 1, size = 2, color = "red", alpha = 0.6) +
  scale_x_continuous(breaks = seq(-60, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_classic() +
  theme(axis.title = element_text(family = "special"),
        axis.line = element_blank(),
        axis.title.x = element_text(vjust = -1)) +
  geom_vline(xintercept = 0, size = 1, linetype = "dotted") +
  xlab("Diferença de idade entre Marcel e o alter") +
  ylab("Grau nodal") +
  annotate("text", x= -40, y= 120, label = "Mais velho que Duchamp", family = "special") +
  annotate("text", x= 33, y= 120, label = "Mais jovem que Duchamp", family = "special")


#alters com mais de 30 relações em comum com Duchamp, e mais de 10 anos mais jovem que ele
ducha %>%
  filter(Degree >= 30, (Nascimento - 1887) > 10) %>%
  select(Label, Degree, Nascimento, Morte, Diferença)



#número de pessoas vivas na rede de Duchamp por ano
#para cada ano, entre 1821 e 2024, calculei quantas pessoas já tinham nascido e ainda não tinham morrido

evolucao <- data.frame("NA", "NA")
colnames(evolucao) <- c("Ano", "Num")

f <- 1

for (i in 1821:2024) {
    evolucao[f,1] <- i
    
    evolucao[f,2] <- length(
      ducha %>%
        filter(Nascimento != "NA", Morte != "NA", Nascimento <= i, Morte > i) %>%
        pull(Label))
    
    f <- f + 1
    
    }


evolucao %>%
  mutate(Ano = as.numeric(Ano), Num = as.numeric(Num)) %>%
  ggplot(aes(x = Ano, y = Num)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1821, 2021, 20)) +
  geom_vline(xintercept = 1915, color = "green", size = 1) +
  geom_vline(xintercept = 1945, color = "green", size = 1)



#número de pessoas vivas na rede de Duchamp por ano (+ 15: somei 15 anos ao ano de nascimento de cada alter; ninguém entra na rede de ninguém com 0 anos)
#para cada ano, entre 1821 e 2024, calculei quantas pessoas já tinham nascido e ainda não tinham morrido


#loop
evolucao_15 <- data.frame("NA", "NA")
colnames(evolucao_15) <- c("Ano", "Num")
f <- 1

for (i in 1821:2024) {
  evolucao_15[f,1] <- i
  
  evolucao_15[f,2] <- length(
    ducha %>%
      filter(Nascimento != "NA") %>%
      #somar 15 anos ao ano de nascimento de cada um
      mutate(Nascimento = Nascimento + 15) %>%
      filter(Nascimento != "NA", Morte != "NA", Nascimento <= i, Morte > i) %>%
      pull(Label))
  
  f <- f + 1
  
}


#ggplot
evolucao_15 %>%
  mutate(Ano = as.numeric(Ano), Num = as.numeric(Num)) %>%
  filter(Ano >= 1887) %>%
  ggplot(aes(x = Ano, y = Num)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1821, 2021, 20)) +
  geom_vline(xintercept = 1941, color = "green", size = 1)
  #geom_vline(xintercept = 1945, color = "green", size = 1)


#gráfico de barras contendo o número de nascimentos e o número de mortes por ano, na mesma visualização

mortes_anos
nascimentos_anos

#emendar os dois data_frames, de morte e de nascimentos, em um único data_frame
nasci_morte_ano <- merge(nascimentos_anos, mortes_anos, by = "Ano", all.x = T, all.y = T) %>% filter(Ano != "NA")

#substitur "NA" por 0 no data_frame morte_nascimento
nasci_morte_ano[is.na(nasci_morte_ano)] <- 0


#ggplot/visualização
nasci_morte_ano %>%
  filter(Ano >= 1822) %>%
  ggplot() +
  geom_col(aes(x=Ano,y=Num_nasci), color = "white", fill = "steelblue4", alpha = 0.6) +
  geom_col(aes(x=Ano,y=Num_mortes), color = "white", fill = "firebrick2", alpha = 0.6) +
  geom_smooth(aes (x=Ano, y= Num_nasci), 
              color = "navyblue", 
              method = "auto", 
              se = F,
              size = 0.5) +
  geom_smooth(aes (x=Ano, y= Num_mortes), 
              color = "red", 
              method = "auto", 
              se = F, 
              size = 0.5) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1820, 2020, 20)) +
  scale_y_continuous(breaks = seq(0,24,1)) +
  theme(axis.title.y = element_blank()) 
  #geom_vline(xintercept = c(1887,1968), color = "green", size = 1.5)


#ggplot/visualização (preto e branco)
nasci_morte_ano %>%
  filter(Ano >= 1822) %>%
  ggplot() +
  geom_col(aes(x=Ano,y=Num_nasci), color = "white", fill = "black", alpha = 0.8) +
  geom_col(aes(x=Ano,y=Num_mortes), color = "white", fill = "black", alpha = 0.2) +
  geom_smooth(aes (x=Ano, y= Num_nasci), 
              color = "black", 
              method = "auto", 
              se = F,
              size = 0.5) +
  geom_smooth(aes (x=Ano, y= Num_mortes), 
              color = "black", 
              method = "auto", 
              se = F, 
              size = 0.5) +
  theme_void() +
  scale_x_continuous(breaks = seq(1820, 2020, 20)) +
  scale_y_continuous(breaks = seq(0,24,1)) +
  theme(axis.title.y = element_blank()) +
  annotate("segment", x = 1880, xend = 1880, y = 0, yend = 9, color = "red", size = 1.5) +
  annotate("segment", x = 1910, xend = 1910, y = 0, yend = 9, color = "red", size = 1.5) +
  annotate("text", x = 1863, y = -0.5, label = "1848 - 1880", color = "red", size = 3.5) +
  annotate("text", x = 1895, y = -0.5, label = "1881 - 1910", color = "red", size = 3.5) +
  annotate("text", x = 1925, y = -0.5, label = "1911 - 1943", color = "red", size = 3.5)

ggsave("nasci_morte_estrutura.jpg")


#numero de nascimentos e número de mortes entre os anos de 1822 e 1880


#data número de mortes 1822-1880
Mil840_mil880_morte <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento >= 1822,
         Nascimento <= 1880) %>%
  group_by(Morte) %>%
  summarise(n())

#rename colunas
Mil840_mil880_morte <- Mil840_mil880_morte %>% rename(Ano = Morte, Morte = 'n()')

#data número de nascimentos 1822 - 1880
Mil840_mil880_nasci <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento >= 1822,
         Nascimento <= 1880) %>%
  group_by(Nascimento) %>%
  summarise(n())

#rename colunas
Mil840_mil880_nasci <- Mil840_mil880_nasci %>% rename(Ano = Nascimento, Nascimento = 'n()')

#merge data mortes e nascimentos 1822 - 1880, substituir "NA"
Mil840_mil880 <- merge(Mil840_mil880_nasci, Mil840_mil880_morte, by = "Ano", all.x = T, all.y = T)
Mil840_mil880[is.na(Mil840_mil880)] <- 0
Mil840_mil880[,4] <- "1848 - 1880"


#ggplot 1822 - 1880

Mil840_mil880 %>%
  ggplot() +
  geom_col(aes(x = Ano, y = Nascimento)) +
  geom_col(aes(x = Ano, y = Morte)) +
  geom_smooth(aes (x=Ano, y= Morte), 
              color = "red", 
              method = "auto", 
              se = F, 
              size = 0.5)



#numero de nascimentos e número de mortes entre os anos de 1881 e 1910


Mil881_mil910_morte <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento > 1880,
         Nascimento <= 1910) %>%
  group_by(Morte) %>%
  summarise(n())

Mil881_mil910_morte <- Mil881_mil910_morte %>% rename(Ano = Morte, Morte = 'n()')

Mil881_mil910_nasci <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento > 1880,
         Nascimento <= 1910) %>%
  group_by(Nascimento) %>%
  summarise(n())

Mil881_mil910_nasci <- Mil881_mil910_nasci %>% rename(Ano = Nascimento, Nascimento = 'n()')


Mil880_mil910 <- merge(Mil881_mil910_nasci, Mil881_mil910_morte, by = "Ano", all.x = T, all.y = T)

Mil880_mil910[is.na(Mil880_mil910)] <- 0
Mil880_mil910[,4] <- "1881 - 1910"




Mil880_mil910_graph <- Mil880_mil910 %>%
  ggplot() +
  geom_col(aes(x = Ano, y = Nascimento)) +
  geom_col(aes(x = Ano, y = Morte)) +
  geom_smooth(aes (x=Ano, y= Morte), 
              color = "red", 
              method = "auto", 
              se = F, 
              size = 0.5)


#numero de nascimentos e número de mortes entre os anos de 1911 e 1943


Mil911_mil943_morte <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento > 1910,
         Nascimento <= 1943) %>%
  group_by(Morte) %>%
  summarise(n())

Mil911_mil943_morte <- Mil911_mil943_morte %>% rename(Ano = Morte, Morte = 'n()')

Mil911_mil943_nasci <- ducha %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Nascimento > 1910,
         Nascimento <= 1943) %>%
  group_by(Nascimento) %>%
  summarise(n())

Mil911_mil943_nasci <- Mil911_mil943_nasci %>% rename(Ano = Nascimento, Nascimento = 'n()')


Mil911_mil943 <- merge(Mil911_mil943_nasci, Mil911_mil943_morte, by = "Ano", all.x = T, all.y = T)

Mil911_mil943[is.na(Mil911_mil943)] <- 0
Mil911_mil943[,4] <- "1911 - 1943"


nrow(Mil911_mil943)


Mil911_mil943_graph <- Mil911_mil943 %>%
  ggplot() +
  geom_col(aes(x = Ano, y = Nascimento)) +
  geom_col(aes(x = Ano, y = Morte)) +
  geom_smooth(aes (x=Ano, y= Morte), 
              color = "red", 
              method = "auto", 
              se = F, 
              size = 0.5)


#emendar os três conjuntos de dados: mortes/nascimentos 1848-1880, 1881-1910, 1911-1943

Mil840_mil880[64:168,] <- Mil880_mil910[1:105,]
Mil840_mil880[169:238,] <- Mil911_mil943[1:70,]

nasci_mortes_divididos <- Mil840_mil880 


#visualização: nascimentos e mortes divididos em três intervalos de tempo: 1848-1880,1881-1910,1911-1943

nasci_mortes_divididos %>%
ggplot() +
  geom_col(aes(x=Ano,y=Nascimento), color = "white", fill = "steelblue4", alpha = 0.6) +
  geom_col(aes(x=Ano,y=Morte), color = "white", fill = "firebrick2", alpha = 0.6) +
  geom_smooth(aes (x=Ano, y= Morte), 
              color = "red", 
              method = "auto", 
              se = F, 
              size = 0.5) +
  facet_grid(rows = vars(V4)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1845, 2025, 15)) +
  scale_y_continuous(breaks = seq(0, 12, 2))


#estrutura da distribuição dos nascimentos e mortes divididos em três intervalos


Mil840_mil880 %>%
  ggplot() +
  geom_col(aes(x=Ano,y=Nascimento), color = "white", fill = "black", alpha = 0.4) +
  geom_col(aes(x=Ano,y=Morte), color = "white", fill = "black", alpha = 0.9) +
  facet_grid(rows = vars(V4)) +
  theme_void() +
  scale_x_continuous(breaks = seq(1845, 2025, 15)) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  theme(strip.text.y = element_blank())


#visualização das filas das mortes dividas em três períodos de tempo

#dividir as pessoas em três grupos geracionais
idades_todos <- ducha %>%
  filter(Nascimento != "NA", Morte != "NA", Nascimento > 1821) %>%
  arrange(Morte) %>%
  mutate(Recortes = ifelse(Nascimento > 1822 & Nascimento <= 1880, "1848 - 1880",ifelse(Nascimento > 1880 & Nascimento <= 1910, "1881 - 1910", "1911 - 1943"))) %>%
  select(Label, Nascimento, Morte, Anos, Recortes)

#acrescentar coluna ID de mortes, e nomear coluna
idades_todos[,6] <- c(1:375)
colnames(idades_todos)[6] <- "ID_morte"

#visualizar as três filas da morte, de acordo com a idade
idades_todos %>%
  ggplot(aes(x = ID_morte, y = Anos)) +
  geom_point(shape = 1, color = "red", size = 1.5, alpha = 0.9) + 
  facet_grid(rows = vars(Recortes)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  ylab("Idade ao falecer")


ggsave("fila de mortes_dividido por nascimento.jpg")


#dividir as pessoas em seis grupos geracionais
idades_todos_1 <- ducha %>%
  filter(Nascimento != "NA", Morte != "NA", Nascimento > 1821) %>%
  arrange(Morte) %>%
  mutate(Recortes = ifelse(Nascimento > 1822 & Nascimento <= 1868, "1848 - 1880",ifelse(Nascimento > 1868 & Nascimento <= 1888, "1868 - 1888", ifelse(Nascimento > 1888 & Nascimento <= 1908, "1888 - 1908", ifelse(Nascimento > 1908 & Nascimento <= 1928, "1908 - 1928", "1928 - 1943"))))) %>%
  select(Label, Nascimento, Morte, Anos, Recortes)


#acrescentar coluna ID de mortes, e nomear coluna
idades_todos_1[,6] <- c(1:375)
colnames(idades_todos_1)[6] <- "ID_morte"



#visualizar as filas da morte, de acordo com a idade, divididas em recortes de vinte anos
idades_todos_1 %>%
  ggplot(aes(x = ID_morte, y = Anos)) +
  geom_point(shape = 1, color = "red", size = 1.5, alpha = 0.9) + 
  facet_grid(rows = vars(Recortes)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  ylab("Idade ao falecer")




#ggplot das idades de falecimento divididos por geração

idades_todos %>%
  mutate(
    Recortes = as.factor(Recortes),
    Recortes = factor(Recortes, levels = c("1911 - 1943", "1881 - 1910", "1848 - 1880"))) %>%
  ggplot(aes(y = Anos, x = Recortes)) +
  geom_violin(color = "grey") +
  geom_boxplot(alpha = 0.3) + 
  scale_color_brewer(palette = "Dark2") +
  #facet_grid(cols = vars(Recortes)) +
  theme_minimal() +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
        #axis.text.x = element_blank()) +
  scale_y_continuous(breaks = seq(30, 105, 5)) +
  ylab("Idade ao falecer")

ggsave("box_plot_geracoes.jpg")


#resumo das métricas para as idades de falecimentos, recortados pelas três gerações
tapply(idades_todos$Anos, idades_todos$Recortes, summary)
summary(ducha$Anos)



#gráfico do número de mortes por anos, separado por geração, e pintado de acordo com os quartis

idades_todos %>%
  group_by(Recortes) %>%
  mutate(Quartil = cut(Anos, include.lowest = T, breaks = quantile(Anos, seq(0, 1, by = 0.25)), labels = c("primeiro", "segundo", "terceiro", "quarto"))) %>%
  mutate(Quartil = ifelse(Quartil == "segundo" | Quartil == "terceiro", "Metade central", ifelse(Quartil == "primeiro", "Precoces", "Longevos"))) %>%
  ggplot(aes(x = Morte, fill = Quartil)) +
  geom_histogram(binwidth = 1) +
  facet_grid(rows = vars(Recortes)) +
  theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_blank()) +
  geom_vline(xintercept = c(1917,1944,1968)) +
  scale_x_continuous(breaks = seq(1915, 2025, 10)) +
  scale_fill_discrete(breaks = c("Precoces", "Metade central", "Longevos")) +
  xlab("Ano")

ggsave("ano_falecimento_geracao_quartil.jpg")


idades_todos %>%
  group_by(Recortes) %>%
  mutate(Quartil = cut(Anos, include.lowest = T, breaks = quantile(Anos, seq(0, 1, by = 0.25)), labels = c("primeiro", "segundo", "terceiro", "quarto"))) %>%
  mutate(Quartil = ifelse(Quartil == "segundo" | Quartil == "terceiro", "Metade central", ifelse(Quartil == "primeiro", "Precoces", "Longevos"))) %>%
  ggplot(aes(x = Morte, fill = Quartil)) +
  geom_density(linetype = "dashed", alpha = 0.4) +
  facet_grid(rows = vars(Recortes)) +
  #theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_blank()) +
  geom_vline(xintercept = c(1917,1944,1968)) +
  scale_x_continuous(breaks = seq(1915, 2025, 10)) +
  scale_fill_discrete(breaks = c("Precoces", "Metade central", "Longevos")) +
  xlab("Ano")

#calcular o valor esperado das mortes

probabilidades_mortes <- idades_todos %>%
  group_by(Recortes) %>%
  group_by(Anos) %>%
  summarise(n(),(n()/375)) %>%
  print(n = 100)

colnames(probabilidades_mortes)[2:3] <- c("Número", "Probabilidade")

#valor esperado das mortes
sum(probabilidades_mortes$Anos * probabilidades_mortes$Probabilidade)



# gráfico de dispersão: no eixo x, o dregree; no eixo y, o tempo do compartilhamento (quantos anos Marcel Duchamp foi contemporâneo de qualquer um dos demais alters)
ducha %>%
  filter(Nascimento != "NA", Morte != "NA", Label != "Marcel Duchamp") %>%
  mutate(Tempo_compartilhamento = (ifelse(Morte <= 1968, Morte, 1968) - (ifelse(Nascimento >= 1887, Nascimento, 1887)))) %>%
  select(Label, Degree, Nascimento, Morte, Tempo_compartilhamento) %>%
  ggplot(aes(y = Tempo_compartilhamento, x = Degree)) +
  geom_point(shape = 1, size = 2) +
  theme_classic() +
  scale_y_continuous(breaks = seq(20, 80, 5)) +
  scale_x_continuous(breaks = seq(0,100,10))




#gráfico de barras horizontais do tempo do compartilhamento: a parte da barra em vermelho é o tempo que Duchamp teve para ser contemporâneo dos seus alters; em preto, a parte da vida dos alters "dessincronizada" da vida de Duchamp
ducha %>%
  arrange(desc(Degree)) %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Label %in% Label[1:21]) %>%
  mutate(Label_data = paste(Label, "(", Nascimento, "-", Morte, ")"),
         Gap = Nascimento - 1875,
         Vida = Gap + (Morte - Nascimento),
         Tempo_compartilhamento = (ifelse(Morte <= 1968, Morte, 1968) - (ifelse(Nascimento >= 1887, Nascimento, 1887))),
         Preto = ifelse(Morte>1968,Vida, "NA"),
         Vermelho = ifelse(Morte>1968,(1887-1875) + (1968 - 1887), "NA")) %>%
  select(Label, Nascimento, Morte, Gap, Vida, Tempo_compartilhamento, Label_data, Preto, Vermelho) %>%
  ggplot() +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = Vida), fill = "red") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = as.numeric(Preto)), fill = "black") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = as.numeric(Vermelho)), fill = "red") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = 1887-1875), fill = "black") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = Gap), fill = "white") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept = c(1887-1875,((1887-1875)+(1968-1887))), color = "white", size = 1) +
  annotate("rect", xmin = 1887 - 1875, xmax = (1887 - 1875) + (1968 - 1887), ymin = 6.55, ymax = 7.45, fill = "darkred", alpha = 0.3)
  
ggsave("tempo_de_compartilhamento_vinte.jpg")


# tempo do compartilhamento, apenas com os alters de menor degree
ducha %>%
  arrange(Degree) %>%
  filter(Nascimento != "NA",
         Morte != "NA",
         Label %in% Label[1:31]) %>%
  mutate(Label_data = paste(Label, "(", Nascimento, "-", Morte, ")"),
         Gap = Nascimento - 1875,
         Vida = Gap + (Morte - Nascimento),
         Tempo_compartilhamento = (ifelse(Morte <= 1968, Morte, 1968) - (ifelse(Nascimento >= 1887, Nascimento, 1887))),
         Preto = ifelse(Morte>1968,Vida, "NA"),
         Vermelho = ifelse(Morte>1968,(1887-1875) + (1968 - 1887), "NA")) %>%
  select(Label, Nascimento, Morte, Gap, Vida, Tempo_compartilhamento, Label_data, Preto, Vermelho) %>%
  ggplot() +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = Vida), fill = "red") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = as.numeric(Preto)), fill = "black") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = as.numeric(Vermelho)), fill = "red") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = 1887-1875), fill = "black") +
  geom_col(aes(y = reorder(Label_data, -Nascimento), x = Gap), fill = "white") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept = c(1887-1875,((1887-1875)+(1968-1887))), color = "white", size = 1)
  #annotate("rect", xmin = 1887 - 1875, xmax = (1887 - 1875) + (1968 - 1887), ymin = 5.55, ymax = 6.45, fill = "darkred", alpha = 0.3)





