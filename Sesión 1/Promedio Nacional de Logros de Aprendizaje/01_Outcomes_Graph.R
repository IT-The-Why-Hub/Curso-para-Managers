library(ggplot2)
library(dplyr)
library(tidyr)

# Paleta de colores personalizada de TWH
custom_colors <- c("#06aed5", "#000000", "#b4b4b4")

# Ruta de tu archivo csv 
file_path <- "C:\\Users\\USUARIO\\Documents\\The Why Hub\\02_GitHub\\03_Curso\\1_Sesión\\learning-outcomes-1985-vs-2015.csv"

df <- read.csv(file_path)

# Seleccionar y renombrar las columnas relevante. Renombra manualmente el Average Outcome
df_long <- df %>%
  select(Entity, Year, Average.in.2015, 
         Year.1, Average.before.2000) %>%
  rename(Country = Entity, 
         Year_2015 = Year, 
         Score_2015 = Average.in.2015, 
         Year_Pre2000 = Year.1, 
         Score_Pre2000 = Average.before.2000)

# Modificando los datos para tener una mejor data 
df_long <- df_long %>%
  pivot_longer(cols = c(Score_2015, Score_Pre2000), 
               names_to = "Period", 
               values_to = "Score") %>%
  mutate(Year = ifelse(Period == "Score_2015", Year_2015, Year_Pre2000)) %>%
  select(Country, Year, Score)

# Filtrar los datos solo para los países de interés: Chile, Perú y Colombia
selected_countries <- c('Chile', 'Peru', 'Colombia')
df_selected_countries <- df_long %>%
  filter(Country %in% selected_countries) %>%
  drop_na(Score)

# Gráfico de líneas 
ggplot(df_selected_countries, aes(x = Year, y = Score, color = Country)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  labs(title = "Promedio Nacional de Logros de Aprendizaje: 1980-2015",
       x = "Año",
       y = "Puntaje de resultado de aprendizaje",
       color = "País",
       caption = "Source: Altinok, Angrist, and Patrinos (2018) - Elaboración propia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015, 5)) +
  scale_y_continuous(limits = c(300, 550)) +
  scale_color_manual(values = custom_colors)

# Exportar el gráfico a un archivo JPG
ggsave("C:\\Users\\USUARIO\\Documents\\The Why Hub\\02_GitHub\\03_Curso\\1_Sesión\\learning_outcomes_plot.jpg", width = 10, height = 8)