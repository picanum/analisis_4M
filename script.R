library(pdftools)
library(tm)
library(SnowballC)
library(stringr)
library(fastmatch)
library(wordcloud)
library(tidytext)
library(igraph)
library(ggraph)
library(tidyverse)
library(extrafont)

#### CARGA DE LOS PROGRAMAS ####
#Hay que descargarlos en el ordenador, yo me bajé los pdfs salvo el de Ciudadanos que lo saqué de la web y está en formato .txt

#El preprocesamiento esta vez ha sido mucho más sencillo que en otras ocasiones, y se resume en quitar algunos términos (pies de página, encabezados, etc.)
#que se repiten página tras página, y quitar también los End Of Line (\r\n)

cs <- as.character(read.table("cs.txt", sep = "\t", encoding = "UTF-8")[,1])

pp <- tabulizer::extract_text("pp.pdf", encoding = "UTF-8")
pp <- str_replace_all(pp, "-\r\n", "")
pp <- str_replace_all(pp, "-\r\n", "")
pp <- str_replace_all(pp, "\r\n", " ")
pp <- str_replace_all(pp, "\\r\\n", " ")
pp <- str_remove_all(pp, "CU M PL IDO")
pp <- str_remove_all(pp, "C U M P LI D O")
pp <- str_remove_all(pp, "E N    C U R S O")
pp <- str_remove_all(pp, "EN   C URSO")
pp <- str_remove_all(pp, "comunidad de madrid 2021")

pod <- tabulizer::extract_text("pod.pdf", encoding = "UTF-8")
pod <- str_replace_all(pod, "-\r\n", "")
pod <- str_replace_all(pod, "-\r\n", "")
pod <- str_replace_all(pod, "\r\n", " ")
pod <- str_replace_all(pod, "Pag. ", " ")

psoe <- tabulizer::extract_text("psoe.pdf", encoding = "UTF-8")
psoe <- str_replace_all(psoe, "-\r\n", "")
psoe <- str_replace_all(psoe, "-\r\n", "")
psoe <- str_replace_all(psoe, "\r\n", " ")
for(i in 4:84){
  psoe <- str_replace_all(psoe, paste0("PROGRAMA DE GOBIERNO. ",i," PSOE MADRID"), " ")
}

mm <- tabulizer::extract_text("masmadrid.pdf", encoding = "UTF-8")
mm <- str_replace_all(mm, "-\r\n", "")
mm <- str_replace_all(mm, "-\r\n", "")
mm <- str_replace_all(mm, "\r\n", " ")
mm <- str_replace_all(mm, "\t", " ")
mm <- str_remove_all(mm, "INTRODUCCIÓN")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS SALUDABLE")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS FEMINISTA Y DIVERSA")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS VERDE")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS INNOVADORA")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS LIBRE")
mm <- str_remove_all(mm, "UNA COMUNIDAD DE DERECHOS, MÁS JUSTA")
mm <- str_remove_all(mm, "UNA COMUNIDAD MÁS DEMOCRÁTICA")
mm <- str_remove_all(mm, "ANEXO: RECURSOS PARA SANAR UNA REGIÓN")

#Luego ya con estas dos funciones de preprocesamiento, quito todos los caracteres extraños y las palabras vacías,
#en parte usando el paquete {tm} y en parte con mis propias aportaciones (que se pueden observar abajo)

preprocesamiento <- function(x) removePunctuation(removeNumbers(tolower(x)))

preproc <- function(texto){
  #quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))
  
  texto2 <- preprocesamiento(texto)
  texto2 <- removeWords(texto2, stopwords("spanish"))
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")
  texto2 <- str_remove_all(texto2, "¦")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "—")
  texto2 <- str_remove_all(texto2, "-")
  texto2 <- str_replace_all(texto2, "·l", "l")
  texto2 <- str_remove_all(texto2, "<U+200B>")
  texto2 <- str_remove_all(texto2, "€")
  texto2 <- str_remove_all(texto2, "´")
  texto2 <- str_replace_all(texto2, "’", " ")
  texto2 <- str_remove_all(texto2, "●")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "–")

  DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(texto2))))
  DTM1 <- as.matrix(DTM1)
  DTM1 <- as.data.frame(DTM1)
  return(DTM1)
}

d_pp <- preproc(pp)

d_psoe <- preproc(psoe)

d_mm <- preproc(mm)

d_pod <- preproc(pod)

d_cs <- preproc(cs)

#Ahora localizo todas las palabras que aparecen en los programas (que se almacenan en los colnames de cada dataset)
#y hago que cada programa tenga una fila dedicada a las frecuencias también de las palabras que no están,
#haciendo un colSums() a cada uno y poniéndolo como columna en un data.frame provisional 

palabras <- c(colnames(d_pp), colnames(d_psoe), colnames(d_mm), colnames(d_pod), colnames(d_cs))
palabras <- names(table(palabras))

palabrizador <- function(d, palabras = palabras){
  ret <- merge(data.frame(palabras = palabras), data.frame(palabras = colnames(d), freq = apply(d,2,sum)), all.x = T)
  return(ret)
}

d2pp <- palabrizador(d_pp, palabras)
d2psoe <- palabrizador(d_psoe, palabras)
d2mm <- palabrizador(d_mm, palabras)
d2pod <- palabrizador(d_pod, palabras)
d2cs <- palabrizador(d_cs, palabras)

#Hecho este paso para todos los programas, los junto finalmente en un dataset

d <- data.frame(palabras = d2pp[,1], pp = d2pp[,2], psoe = d2psoe[,2], mm = d2mm[,2], 
                pod = d2pod[,2], cs = d2cs[,2])

#Y aquí saco también los datasets con las frecuencias pasadas a tf y a tf-idf

d_tfidf <- d
for(i in 2:ncol(d_tfidf)){
  d_tfidf[,i] <- d_tfidf[,i]/max(d_tfidf[,i],na.rm=T) * log((ncol(d_tfidf)-1)/(apply(d[,2:ncol(d_tfidf)], 1, function(x) sum(x>0, na.rm =T))))
}

d_tf <- d
for(i in 2:ncol(d_tf)){
  d_tf[,i] <- d_tf[,i]/max(d_tf[,i],na.rm=T) 
}

palabras_no_vacias <- apply(d[,2:ncol(d)], 2, sum, na.rm = T)
#   pp  psoe    mm   pod    cs 
#15949 11612 32008 30032  5541  

palabras_totales <- c(str_count(pp, pattern = "\\S+"),
  str_count(psoe, pattern = "\\S+"),
  str_count(mm, pattern = "\\S+"),
  str_count(pod, pattern = "\\S+"),
  sum(str_count(cs, pattern = "\\S+")))

data.frame(part = c("PP", "PSOE", "Más Madrid", "UP", "Ciudadanos"),
           term = palabras_no_vacias,
           term_vac = palabras_totales) %>%
  mutate(porc = 100*term/term_vac) %>%
  mutate(part = factor(part, levels = part[order(term)])) %>%
  ggplot(aes(x = part, y = term, 
             label = paste0(term, " no vacías\n(",
                            round(porc, 2), " %)"))) + 
  geom_col(fill = c("dodgerblue", "red2", "springgreen2", "purple", "orange")) +
  geom_col(aes(x = part, y = term_vac), alpha = .5,
           fill = c("dodgerblue", "red2", "springgreen2", "purple", "orange")) +
  geom_text(nudge_y = 0, family = "Liberation Sans", size = 5) +
  geom_text(aes(x = part, y = term_vac,
                label = paste0(term_vac, " palabras")),
            nudge_y = 1500, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black")) +
  ggplot2::annotate("text", x = 1.5, y = 60000,
                    label = "Twitter/Instagram: @Picanumeros",
                    family = "Forte", size = 6) +
  labs(y = "Número de palabras", x = "",
       title = "Número de palabras totales y no vacías, y % de palabras no vacías,\nen cada programa electoral de las elecciones madrileñas del 4 de mayo de 2021",
       subtitle = "Palabra = cadena de caracteres entre dos espacios.\nPalabra no vacía = cualquier palabra que no sea un número, un signo de puntuación\no una palabra sin significado (artículo, preposición, etc.)"
       )
ggsave("numpalabras.png", dpi = 300, height = 12, width = 12)

for(i in 2:ncol(d)){
  d[which(is.na(d[,i])),i] <- 0
  d[,i] <- d[,i]/sum(d[,i])
}

#### WORDCLOUDS ####

library(patchwork)
library(ggwordcloud)

g1 <- d %>% filter(pp >= quantile(d$pp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 13) + theme_minimal(base_size = 20) +
  labs(title = "PP") + theme(title = element_text(colour = "dodgerblue"),
                             plot.title = element_text(hjust = 0.5))
g2 <- d %>% filter(psoe >= quantile(d$psoe,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 13) + theme_minimal(base_size = 20) +
  labs(title = "PSOE") + theme(title = element_text(colour = "red2"),
                               plot.title = element_text(hjust = 0.5),
                               plot.subtitle = element_text(hjust = 0.5))
g3 <- d %>% filter(cs >= quantile(d$cs,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = cs)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 13) + theme_minimal(base_size = 20) +
  labs(title = "Ciudadanos") + theme(title = element_text(colour = "orange"),
                                     plot.title = element_text(hjust = 0.5))
g4 <- d %>% filter(pod >= quantile(d$pod,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 13) + theme_minimal(base_size = 20) +
  labs(title = "Unidas Podemos") + theme(title = element_text(colour = "purple"),
                                         plot.title = element_text(hjust = 0.5))
g5 <- d %>% filter(mm >= quantile(d$mm,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = mm)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 13) + theme_minimal(base_size = 20) +
  labs(title = "Más Madrid") + theme(title = element_text(colour = "springgreen2"),
                                     plot.title = element_text(hjust = 0.5))

(g1 + g2 + g3) / (g4 + g5) + 
  plot_annotation(title = "Términos más repetidos en los programas electorales de las\nelecciones madrileñas del 4M de los principales partidos",
                  caption = "Twitter/Instagram: @Picanumeros",
                  theme = theme(plot.title = element_text(size = 24, family = "Liberation Sans"),
                                plot.caption = element_text(family = "Forte", size = 18)))
ggsave("wordcloud.png", dpi = 300, width = 10.9, height = 8)

g1 <- d %>% filter(palabras %in% c("comunidad", "madrid") == F) %>%
  filter(pp >= quantile(d$pp,(1-22/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PP") + theme(title = element_text(colour = "dodgerblue"),
                             plot.title = element_text(hjust = 0.5))
g2 <- d %>% filter(palabras %in% c("comunidad", "madrid") == F) %>%
  filter(psoe >= quantile(d$psoe,(1-22/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PSOE") + theme(title = element_text(colour = "red2"),
                               plot.title = element_text(hjust = 0.5),
                               plot.subtitle = element_text(hjust = 0.5))
g3 <- d %>% filter(palabras %in% c("comunidad", "madrid") == F) %>%
  filter(cs >= quantile(d$cs,(1-22/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = cs)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Ciudadanos") + theme(title = element_text(colour = "orange"),
                                     plot.title = element_text(hjust = 0.5))
g4 <- d %>% filter(palabras %in% c("comunidad", "madrid") == F) %>%
  filter(pod >= quantile(d$pod,(1-22/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Unidas Podemos") + theme(title = element_text(colour = "purple"),
                              plot.title = element_text(hjust = 0.5))
g5 <- d %>% filter(palabras %in% c("comunidad", "madrid") == F) %>%
  filter(mm >= quantile(d$mm,(1-22/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = mm)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Más Madrid") + theme(title = element_text(colour = "springgreen2"),
                                        plot.title = element_text(hjust = 0.5))

(g1 + g2 + g3) / (g4 + g5) + 
  plot_annotation(title = "Términos más repetidos en los programas electorales (quitando 'comunidad' y 'madrid')\nde las elecciones madrileñas del 4M de los principales partidos",
                  caption = "Twitter/Instagram: @Picanumeros",
                  theme = theme(plot.title = element_text(size = 24, family = "Liberation Sans"),
                                plot.caption = element_text(family = "Forte", size = 18)))
ggsave("wordcloud_sin_madrid.png", dpi = 300#, width = 12, height = 13)

#### TERMINOS MÁS PROPIOS DE CADA PROGRAMA ####

d$media <- apply(d[,2:ncol(d)],1,mean)

d_tfidf %>% 
  mutate(pp = ifelse(is.na(pp), 0, pp),
         psoe = ifelse(is.na(psoe), 0, psoe),
         mm = ifelse(is.na(mm), 0, mm),
         pod = ifelse(is.na(pod), 0, pod),
         cs = ifelse(is.na(cs), 0, cs)) %>%
  pivot_longer(-palabras, names_to = "partido", values_to = "apps") %>%
  mutate(part = rep(c("PP", "PSOE", "Más Madrid", "Unidas Podemos", "Ciudadanos"),
                    n()/5)) %>%
  group_by(part) %>% arrange(desc(apps)) %>% slice_max(apps, n = 15) %>%
  ungroup() %>%
  #'niñas' aparece tanto en el de MM como en el de UP así que tengo que hacer un truco
  #un poco cutre para que no salga desordenado: añadir un espacio al principio
  mutate(palabras = as.character(palabras)) %>%
  mutate(palabras = ifelse(palabras == "niñas" & partido == "pod",
                           " niñas", palabras)) %>%
  ggplot(aes(apps, fct_reorder(palabras, apps), fill = part)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("orange", "springgreen2", "dodgerblue", "red2", "purple")) +
  theme_minimal(base_size = 16) +
  facet_wrap(~part, ncol = 3, scales = "free") +
  theme(text = element_text(family = "Liberation Sans"),
        plot.caption = element_text(family = "Forte", size = 18)) +
  labs(x = "Valor de tf-idf", y = NULL,
       title = str_wrap("Términos más repetidos en el programa para las elecciones madrileñas del 4M de cada partido en comparación con el resto de partidos (usando tf-idf)", 75),
       subtitle = "Cuanto mayor sea el tf-idf, más aparece el término en ese programa y menos en los demás",
       caption = "Twitter/Instagram: @Picanumeros") +
  ggsave("masrepes2.png", dpi = 300, height = 12, width = 12)

#Aquí debajo, el código que he usado para sacar los diagramas de barras individuales para cada programa.
#Podéis ir cambiando el partido cuyo diagrama queráis sacar modificando el último filter()

d_tfidf %>% 
  mutate(pp = ifelse(is.na(pp), 0, pp),
         psoe = ifelse(is.na(psoe), 0, psoe),
         mm = ifelse(is.na(mm), 0, mm),
         pod = ifelse(is.na(pod), 0, pod),
         cs = ifelse(is.na(cs), 0, cs)) %>%
  pivot_longer(-palabras, names_to = "partido", values_to = "apps") %>%
  mutate(part = rep(c("PP", "PSOE", "Más Madrid", "Unidas Podemos", "Ciudadanos"),
                    n()/5)) %>%
  group_by(part) %>% arrange(desc(apps)) %>% slice_max(apps, n = 15) %>%
  ungroup() %>%
  #'niñas' aparece tanto en el de MM como en el de UP así que tengo que hacer un truco
  #un poco cutre para que no salga desordenado: añadir un espacio al principio
  mutate(palabras = as.character(palabras)) %>%
  mutate(palabras = ifelse(palabras == "niñas" & partido == "pod",
                           " niñas", palabras)) %>%
  filter(part == "Ciudadanos") %>%
  ggplot(aes(apps, fct_reorder(palabras, apps))) +
  geom_col(show.legend = FALSE, fill = "orange") +
  theme_minimal(base_size = 16) +
  #facet_wrap(~part, ncol = 3, scales = "free") +
  theme(text = element_text(family = "Liberation Sans"),
        plot.caption = element_text(family = "Forte", size = 18),
        axis.text.y = element_text(size = 24)) +
  labs(x = "Valor de tf-idf", y = NULL,
       title = str_wrap("Términos más repetidos en el programa de Ciudadanos para las elecciones madrileñas del 4M en comparación con el resto de partidos (usando tf-idf)", 75),
       subtitle = "Cuanto mayor sea el tf-idf, más aparece el término en ese programa y menos en los demás",
       caption = "Twitter/Instagram: @Picanumeros") +
  ggsave("cs.png", dpi = 300, height = 12, width = 12)

#### ANÁLISIS DE COMPONENTES PRINCIPALES ####

#Creamos una copia del dataset con las frecuencias de palabras para poder trabajar sobre él
d_comp <- d

library(factoextra)
rownames(d_comp) <- d_comp[,1]
d_comp <- d_comp[-which(d_comp[,1] %in% c("comunidad","madrid")),]  #Si queremos hacer el análisis con las palabras 'comunidad' y 'madrid', ponemos un hash delante de esta línea
colnames(d_comp)[2:6] <- c("PP","PSOE","MM","UP", "Cs")
res.pca <- FactoMineR::PCA(d_comp[,2:6])
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
) + labs(
  title = "Análisis de componentes principales sobre los programas\nelectorales, según frecuencia relativa de palabras utilizadas\n(quitando los términos 'comunidad' y 'madrid')",
  caption = "Twitter/Instagram: @Picanumeros",
  x = paste0("Componente 1 (",round(res.pca$eig[1,2], 1)," % de varianza explicada)"), 
  y = paste0("Componente 2 (",round(res.pca$eig[2,2], 1)," % de varianza explicada)")
) +
  theme(text = element_text(family = "Liberation Sans", size = 16),
        plot.caption = element_text(family = "Forte", size = 16))
ggsave("comp1_2.png", dpi = 300)

fviz_pca_biplot(res.pca, repel = F,
                col.var = "red",
                geom.var = c("arrow", "text")) + labs(
  title = str_wrap("Análisis de componentes principales sobre los programas electorales, según frecuencia relativa de palabras utilizadas (quitando los términos 'comunidad' y 'madrid')",90),
  caption = "Twitter/Instagram: @Picanumeros",
  x = paste0("Componente 1 (",round(res.pca$eig[1,2], 1)," % de varianza explicada)"), 
  y = paste0("Componente 2 (",round(res.pca$eig[2,2], 1)," % de varianza explicada)") 
) +
  theme(text = element_text(family = "Liberation Sans", size = 18),
        plot.caption = element_text(family = "Forte", size = 18))
ggsave("comp2_2.png", dpi = 300)

#### ESCALAMIENTO MULTIDIMENSIONAL ####

mds <- dist(t(d_comp[,2:6]))

fit <- cmdscale(mds,eig=TRUE, k=2)
puntos <- as.data.frame(fit$points)
ggplot(puntos, aes(x = V1, y = V2, label = row.names(fit$points))) + 
  geom_point() + ggrepel::geom_label_repel(size = 6.5) +
  theme_classic(base_size = 15) + labs(x = paste0("Dimensión 1 (", round(100*fit$eig[1]/sum(fit$eig),1)," % suma autovalores)"),
                                       y = paste0("Dimensión 2 (", round(100*fit$eig[2]/sum(fit$eig),1)," % suma autovalores)"),
                                       title = str_wrap("Representación bidimensional de los programas de cada partido mediante escalamiento multidimensional (MDS) según las distancias entre ellos",70),
                                       subtitle = "Distancias obtenidas a partir de la distancia euclídea entre frecuencias relativas de palabras",
                                       caption = "Twitter/Instagram: @Picanumeros") +
  theme(text = element_text(family = "Liberation Sans", size = 18),
        plot.caption = element_text(family = "Forte", size = 18))

ggsave("mds.png", dpi = 300, height = 12, width = 12)

#### FRECUENCIA DE TERMINOS CONCRETOS ####

data.frame(porc = apply(d_tf[which(substr(palabras, 1, 7) == "científ" |
                                      substr(palabras, 1, 7) == "pseudoci" |
                                      substr(palabras, 1, 7) == "ciencia"), 2:ncol(d_tf)], 2, sum, na.rm = T),
           part = c("PP", "PSOE", "Más Madrid", "UP", "Ciudadanos")) %>% 
  mutate(part = factor(part, levels = part[order(porc)])) %>%
  ggplot(aes(x = part, y = porc, label = paste0(round(porc, 3), ""))) + 
  geom_col(fill = "dodgerblue") +
  geom_text(nudge_y = 0.01, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black")) +
  ggplot2::annotate("text", x = 1.5, y = 0.25,
                    label = "Twitter/Instagram: @Picanumeros",
                    family = "Forte", size = 6) +
  geom_curve(x = 4, xend = 5, y = 0.25 , yend = 0.21,
               arrow = arrow(), curvature = -0.2, 
             size = 1.05) +
  annotate("text", x = 3.4, y = 0.24,
           label = str_wrap("Los términos de ciencia se dan con una frecuencia equivalente a casi una quinta parte (0.192 veces) del término más repetido en el programa de Ciudadanos", 35)) +
  labs(y = "Núm. apariciones / núm. máximo de apariciones de una palabra", x = "",
       title = "Frecuencia normalizada de apariciones de términos relacionados con la ciencia con respecto al total\nde palabras no vacías en cada programa electoral de las elecciones madrileñas del 4M",
       subtitle = "Términos con raíz 'científ', 'ciencia' o 'pseudoci'"
       )
ggsave("porc_ciencia.png", dpi = 300)

data.frame(porc = apply(d_tf[which(str_detect(d_tf$palabras, "industria")), 2:ncol(d_tf)], 2, sum, na.rm = T),
           #porc = apply(d_tf[which(str_detect(d_tf$palabras, "industria")), 2:ncol(d_tf)], 2, sum, na.rm = T),
           part = c("PP", "PSOE", "Más Madrid", "UP", "Ciudadanos")) %>% 
  mutate(part = factor(part, levels = part[order(porc)])) %>%
  ggplot(aes(x = part, y = porc, label = paste0(round(porc, 3), ""))) + 
  geom_col(fill = "dodgerblue") +
  geom_text(nudge_y = 0.005, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black")) +
  ggplot2::annotate("text", x = 1.5, y = 0.125,
                    label = "Twitter/Instagram: @Picanumeros",
                    family = "Forte", size = 6) +
  labs(y = "Núm. apariciones / núm. máximo de apariciones de una palabra", x = "",
       title = "Frecuencia normalizada de términos relacionados con la industria con respecto al total\nde palabras no vacías en cada programa electoral de las elecciones madrileñas del 4M",
       subtitle = "Términos que contengan 'industria'"
       #, caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros"
  )
ggsave("porc_industria.png", dpi = 300)

data.frame(porc = apply(d_tf[which(str_detect(d_tf$palabras, "salud|sani")), 2:ncol(d_tf)], 2, sum, na.rm = T),
           part = c("PP", "PSOE", "Más Madrid", "UP", "Ciudadanos")) %>% 
  mutate(part = factor(part, levels = part[order(porc)])) %>%
  ggplot(aes(x = part, y = porc, label = paste0(round(porc, 3), ""))) + 
  geom_col(fill = "dodgerblue") +
  geom_text(nudge_y = 0.05, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black")) +
  ggplot2::annotate("text", x = 1.5, y = 1.25,
                    label = "Twitter/Instagram: @Picanumeros",
                    family = "Forte", size = 6) +
  labs(y = "Núm. apariciones / núm. máximo de apariciones de una palabra", x = "",
       title = "Frecuencia normalizada de apariciones de términos relacionados con la salud o la sanidad con\nrespecto al total de palabras no vacías en cada programa electoral de las elecciones madrileñas del 4M",
       subtitle = "Términos que contengan 'salud' o 'sani'"
       #, caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros"
  )
ggsave("porc_sanidad.png", dpi = 300)

data.frame(porc = apply(d_tf[which(str_detect(d_tf$palabras, "laboral|trabajo|trabajad")), 2:ncol(d_tf)], 2, sum, na.rm = T),
           part = c("PP", "PSOE", "Más Madrid", "UP", "Ciudadanos")) %>% 
  mutate(part = factor(part, levels = part[order(porc)])) %>%
  ggplot(aes(x = part, y = porc, label = paste0(round(porc, 3), ""))) + 
  geom_col(fill = "dodgerblue") +
  geom_text(nudge_y = 0.025, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black")) +
  ggplot2::annotate("text", x = 1.5, y = 0.6,
                    label = "Twitter/Instagram: @Picanumeros",
                    family = "Forte", size = 6) +
  labs(y = "Núm. apariciones / núm. máximo de apariciones de una palabra", x = "",
       title = str_wrap("Frecuencia normalizada de apariciones de términos relacionados con el trabajo o lo laboral con respecto al total de palabras no vacías en cada programa electoral de las elecciones madrileñas del 4M", 95),
       subtitle = "Términos que contengan 'laboral', 'trabajo', 'trabajad'"
       #, caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros"
  )
ggsave("porc_laboral.png", dpi = 300)


#### N-GRAMAS ####

preproc_tidy <- function(texto){
  #quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))
  
  texto2 <- preprocesamiento(texto)
  texto2 <- removeWords(texto2, stopwords("spanish"))
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")
  texto2 <- str_remove_all(texto2, "¦")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "—")
  texto2 <- str_remove_all(texto2, "-")
  texto2 <- str_replace_all(texto2, "·l", "l")
  texto2 <- str_remove_all(texto2, "<U+200B>")
  texto2 <- str_remove_all(texto2, "€")
  texto2 <- str_remove_all(texto2, "´")
  texto2 <- str_replace_all(texto2, "’", " ")
  texto2 <- str_remove_all(texto2, "●")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "–")

  return(texto2)
}

pp_2gram <- preproc_tidy(pp) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

cs_2gram <- preproc_tidy(cs) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

psoe_2gram <- preproc_tidy(psoe) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

pod_2gram <- preproc_tidy(pod) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

mm_2gram <- preproc_tidy(mm) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% #filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% 
              mutate(programa = "PSOE*")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos*")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "Unidas Podemos")) %>%
  bind_rows(mm_2gram %>% mutate(programa = "Más Madrid")) %>%
  bind_tf_idf(bigram, programa, n) %>%
  group_by(programa) %>% slice_max(tf_idf, n = 6) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = programa)) +
  geom_col(show.legend = FALSE) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("orange", "springgreen2", "dodgerblue", "red2", "purple")) +
  scale_x_continuous(breaks = c(0, 0.00025, 0.0005, 0.00075, 0.001)) +
  theme(plot.caption = element_text(family = "Forte", size = 18),
        text = element_text(family = "Liberation Sans")) +
  facet_wrap(~programa, ncol = 2, scales = "free") +
  labs(x = "Valor de tf-idf\n*En los del PSOE y Ciudadanos aparecen más por haber múltiples empates", y = NULL,
       subtitle = "Cuanto mayor sea el tf-idf, más aparece el bigrama en ese programa y menos en los demás",
       title = str_wrap("Bigramas más repetidos en el programa de cada partido en comparación con el resto de partidos (según la métrica tf-idf)", 80),
       caption = "Twitter/Instagram: @Picanumeros") +
  ggsave("bigramas_masrepes2.png", dpi = 300, height = 12, width = 12)
  
#### TÉRMINOS QUE MÁS ACOMPAÑAN A OTROS TÉRMINOS DETERMINADOS (p. ej. cuáles acompañanan más a 'ciencia') ####
#Lo tenemos que hacer con los bigramas, aunque modificando el código no será complicado cambiarlo a trigrama o lo que se quiera

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% #filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% 
              mutate(programa = "PSOE")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "Unidas Podemos")) %>%
  bind_rows(mm_2gram %>% mutate(programa = "Más Madrid")) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                           substr(palabras, 1, 7) == "pseudoci" |
                                           substr(palabras, 1, 7) == "ciencia"), "palabras"]) |
           word2 %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                             substr(palabras, 1, 7) == "pseudoci" |
                                             substr(palabras, 1, 7) == "ciencia"), "palabras"])) %>%
  select(word1, word2, programa, n) %>% pivot_longer(-c(programa, n), names_to = "orden", values_to = "word") %>% group_by(word, programa) %>% summarise(n = sum(n)) %>%
  filter(word %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                          substr(palabras, 1, 7) == "pseudoci" |
                                          substr(palabras, 1, 7) == "ciencia"), "palabras"])==F) %>%
  ggplot(., aes(x = 1, y = 1, size = n, label = word)) +
  ggrepel::geom_text_repel(segment.size = 0, segment.alpha = 0) +
  scale_size(range = c(3, 9), guide = FALSE) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_line(colour = "white", size=1),
        plot.caption = element_text(family = "Forte", size = 18),
        axis.text = element_text(colour = "white", size = 0),
        axis.ticks = element_line(colour = "white", size = 0)) +
  facet_wrap(~programa) +
  labs(title = "Wordcloud de los términos que acompañan a aquellos relacionados con la ciencia en cada programa de las elecciones madrileñas del 4M",
       x = "", y = "",
       subtitle = "A más grande el término, más aparece acompañando a términos relacionados con la ciencia.\nNo se han filtrado por número de apariciones, por lo que los wordclouds son totalmente exhaustivos.",
       caption = "Twitter/Instagram: @Picanumeros")
ggsave("words_ciencia.png", dpi = 300, width = 18, height = 10)

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% #filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% 
              mutate(programa = "PSOE")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "Unidas Podemos")) %>%
  bind_rows(mm_2gram %>% mutate(programa = "Más Madrid")) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(str_detect(word1, "industria") | str_detect(word2, "industria")) %>%
  select(word1, word2, programa, n) %>% pivot_longer(-c(programa, n), names_to = "orden", values_to = "word") %>% 
  group_by(word, programa) %>% summarise(n = sum(n)) %>%
  filter(str_detect(word, "industria")==F) %>%
  ggplot(., aes(x = 1, y = 1, size = n, label = word)) +
  ggrepel::geom_text_repel(segment.size = 0, segment.alpha = 0) +
  scale_size(range = c(3, 9), guide = FALSE) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_line(colour = "white", size=1),
        plot.caption = element_text(family = "Forte", size = 18),
        axis.text = element_text(colour = "white", size = 0),
        axis.ticks = element_line(colour = "white", size = 0)) +
  facet_wrap(~programa) +
  labs(title = "Wordcloud de los términos que acompañan a aquellos relacionados con la industria en cada programa de las elecciones madrileñas del 4M",
       x = "", y = "",
       subtitle = "A más grande el término, más aparece acompañando a términos relacionados con la industria.\nNo se han filtrado por número de apariciones, por lo que los wordclouds son totalmente exhaustivos.",
       caption = "Twitter/Instagram: @Picanumeros")
ggsave("words_industria.png", dpi = 300, width = 18, height = 10)

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% #filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% 
              mutate(programa = "PSOE")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "Unidas Podemos")) %>%
  bind_rows(mm_2gram %>% mutate(programa = "Más Madrid")) %>% 
  #group_by(programa) %>% mutate(n = n/sum(n)) %>% ungroup %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(str_detect(word1, "salud|sani") | str_detect(word2, "salud|sani")) %>%
  select(word1, word2, programa, n) %>% pivot_longer(-c(programa, n), names_to = "orden", values_to = "word") %>% 
  group_by(word, programa) %>% summarise(n = sum(n)) %>%
  filter(str_detect(word, "salud|sani")==F) %>%
  filter((n < 2) == F) %>%
  filter((programa == "Más Madrid" & n < 4) == F) %>%
  ggplot(., aes(x = 1, y = 1, size = n, label = word)) +
  ggrepel::geom_text_repel(segment.size = 0, segment.alpha = 0) +
  scale_size(range = c(3, 9), guide = FALSE) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_line(colour = "white", size=1),
        plot.caption = element_text(family = "Forte", size = 18),
        axis.text = element_text(colour = "white", size = 0),
        axis.ticks = element_line(colour = "white", size = 0)) +
  facet_wrap(~programa) +
  labs(title = "Wordcloud de los términos que acompañan a aquellos relacionados con la salud y la sanidad en cada programa de las elecciones madrileñas del 4M",
       x = "", y = "",
       subtitle = "A más grande el término, más aparece acompañando a términos relacionados con la salud y la sanidad.\nSe toman los bigramas que aparecen más de una vez, salvo para Más Madrid (se toman los que aparecen más de 3 veces)",
       caption = "Twitter/Instagram: @Picanumeros")
ggsave("words_salud.png", dpi = 300, width = 18.5, height = 10)

#### BURBUJAS ####

d %>% 
  filter(palabras %in% c("españa", "madrid", "libertad", "mena", "empleo", "comunismo", "socialismo")) %>%
  pivot_longer(-c("palabras", "media"), names_to = "partido", values_to = "apps") %>%
  mutate(part = rep(c("PP", "PSOE", "Más Madrid", "Unidas Podemos", "Ciudadanos"),
                    n()/5)) %>%
  mutate(part = factor(part, levels = c("PP", "Ciudadanos", "PSOE", "Más Madrid", "Unidas Podemos")),
         palabras = factor(palabras, levels = c("españa", "madrid", "libertad", "empleo")[4:1])) %>%
  ggplot(aes(x = palabras, y = part, size = apps/media, fill = part,
             label = round(apps/media,2))) + 
  geom_point(pch = 21, alpha = .75, col = "black") +
  geom_text(size = 6) +
  scale_fill_manual(values = c("dodgerblue", "orange", "red2", "springgreen2", "purple")) +
  scale_size_continuous(range = c(10, 45)) + 
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 18),
        plot.caption = element_text(family = "Forte", size = 18),
        legend.position = "none") +
  labs(x = "Término", y = "",
       title = str_wrap("Frecuencia de los términos 'españa', 'madrid', 'libertad' y 'empleo' en el programa para las elecciones madrileñas del 4M de cada partido en comparación con el resto de partidos", 90),
       subtitle = "Calculado dividiendo el % de aparición de cada término en un programa concreto\nentre el % medio de aparición en los 5 programas",
       caption = "Twitter/Instagram: @Picanumeros") +
  ggsave("burbujas.png", dpi = 300)
