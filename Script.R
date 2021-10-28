library(readxl)
FV_perfiles <- read_excel("FV_perfiles.xlsx", 
                          sheet = "Hoja1")
View(FV_perfiles)
attach(FV_perfiles)

library(tidyverse)
FV_perfiles <- FV_perfiles %>%
  mutate(Sexo=recode_factor(Sexo,Hombre="Hombre",Mujer="Mujer"))%>%
  mutate(Deporte=recode_factor(Deporte,
                                 Lucha="Lucha",Judo="Judo")) 

#subsets 
library(dplyr)
# Lucha
Lucha <- FV_perfiles %>% filter(Deporte == "Lucha")
# Judo
Judo <- FV_perfiles %>% filter(Deporte == "Judo")

# Analysis de Fmax
# Descriptivos ##Actualizar  el mean y sd que son el promedio y  la deviacion
group_by(FV_perfiles, Deporte) %>% summarise(count = n(), 
    mean = mean(Fmax, na.rm = TRUE),
    sd = sd(Fmax, na.rm = TRUE))

# plot
library("ggpubr")
ggboxplot(FV_perfiles, x = "Deporte", y = "Fmax", 
          color = "Deporte", palette = c("#00AFBB", "#E7B800"),
          ylab = "Fmax", xlab = "Deporte")

# analysis  de normalidad
library(rstatix)
# Fmax
FV_perfiles %>% group_by(Deporte) %>% shapiro_test(Fmax)
# independent t test para diferencias (si estan normalmente distribuidas)
t.test(Lucha$Pmax, Judo$Pmax, alternative = "two.sided", var.equal = FALSE)

# effecto de cohen's D con correcion de hedges'g para un sample pequeño <30
FV_perfiles  %>% cohens_d(Pmax ~ Deporte,
                 paired = FALSE, hedges.correction = TRUE) 


## agregar los test independientes de T para las otras variables



#asociaciones

cor(Distancia_de_empuje, Vmax)

## Plot, ajustar el metodo y los labels
ggscatter(FV_perfiles, x = "Distancia_de_empuje", y = "Vmax",
          ylab = "Distancia", xlab = "",
          add = "reg.line", conf.int = TRUE, add.params = list(color = "blue",
          fill = "lightgray")) +
  stat_cor(method = "pearson", label.x = .4, label.y = 4)

##Guardar plot
ggsave("Horas_semanalvsFmax.png")
