


## ---- voorbereiding


# Packages laden ----------------------------------------------------------
#workspace leegmaken.
rm(list = ls())

#benodigde packages laden.
library(tidyverse)
library(lubridate)
library(ggplot2)
library(knitr)
source( "./latex_utils/latex_utils.R")


# Instellingen ----------------------------------------------------------------------------------------------------





# Data -----------------------------------------------------------------------------------------------------------

# voorbeeld data
data("iris")





# Functies --------------------------------------------------------------------------------------------------------






# Berekeningen ----------------------------------------------------------------------------------------------------


test_table <- iris %>% 
  group_by(Species) %>% 
  summarise(meanlength = round(mean(Sepal.Length, na.rm = TRUE)), 
            meanwidth = round(mean(Sepal.Width, na.rm = TRUE))) %>% 
  add_row(
    iris %>% 
      group_by(Species = 'Total') %>% 
      summarise(meanlength = round(mean(Sepal.Length, na.rm = TRUE)), 
                'meanwidth' = round(mean(Sepal.Width, na.rm = TRUE)))
  )





# Latex tabellen --------------------------------------------------------------------------------------------------


# tabbelen met gegevens over aantal unieke clienten
# out <- kable(test_table, format = 'latex')

out <- latex_table(test_table, caption = 'test titel')

# latex bestand wegschrijven
write(out, file = "./tables/tabel.tex")



# Grafieken -------------------------------------------------------------------------------------------------------

# thema
mytheme <- theme_grey()
mytheme$axis.line.y <- mytheme$axis.line.x <- mytheme$axis.line
mytheme$axis.line.y$colour <- "white"
mytheme$axis.line.x$colour <- "black"
mytheme <-  theme(axis.title = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(colour = "#6D6D6B", size = 0.2),
                  panel.border = element_blank(), panel.background = element_blank(), 
                  strip.background = element_rect(fill = "white"), axis.text = element_text(color = "black"), 
                  strip.text = element_text(face = "bold"), panel.margin = unit(c(2, 1, 1, 1), "cm"), 
                  plot.title = element_text(face = "bold"), text = element_text(size = 14), 
                  legend.title = element_blank(), 
                  legend.background = element_rect(fill = "transparent"),
                  axis.ticks.x = element_blank(), axis.ticks.y = element_line(size = 0.3, colour = "#6D6D6B"), 
                  axis.text.y = element_text(color = "#6D6D6B"), legend.text = element_text(color = "#6D6D6B")) 


colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")


ggplot(test_table, aes(x = Species, y=meanlength)) + 
  geom_bar(position="dodge", stat="identity")


# ggsave(plot, filename = "grafieken/plot.pdf", height = 75, width = 200, units = "mm") 




# Output ----------------------------------------------------------------------------------------------------------

# write.csv2(tabel, file = "output/tabel.csv", row.names = FALSE)


