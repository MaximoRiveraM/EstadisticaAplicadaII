# Cargar librerías necesarias
library(dplyr)  # Para manipulación de datos
library(readr)  # Para leer datos
library(tibble) # Para trabajar con dataframes
library(ggplot2) # Cargar librería para gráficos



# Cargar el dataset
# Cambia la ruta del archivo a donde esté ubicado en tu máquina
olympic_data <- read_csv("baseFinal.csv")

olympic_data

# Revisar estructura del dataset
glimpse(olympic_data) # Similar a str() pero más legible

# Identificar valores faltantes por columna
missing_values <- sapply(olympic_data, function(x) sum(is.na(x))) # Cantidad de NA por columna
missing_percentage <- (missing_values / nrow(olympic_data)) * 100 # Porcentaje de NA por columna

# Crear un dataframe con los valores faltantes
missing_data <- tibble(
  Column = names(missing_values),
  `Missing Values` = missing_values,
  `Percentage Missing (%)` = missing_percentage
)

# Filtrar columnas con valores faltantes
missing_data <- missing_data %>%
  filter(`Missing Values` > 0)

# Mostrar las columnas con valores faltantes
print(missing_data)

# Filtrar datos de atletas que ganaron medalla de oro
gold_medalists <- olympic_data %>%
  filter(Medal == "Gold")

# Verificar la estructura del subconjunto
glimpse(gold_medalists)

# Distribución de medallistas por sexo
gold_sex_distribution <- gold_medalists %>%
  group_by(Sex) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Imprimir distribución por sexo
print(gold_sex_distribution)

# Distribución de medallistas por deporte
gold_sport_distribution <- gold_medalists %>%
  group_by(Sport) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Mostrar los 10 deportes con más medallas de oro
top_sports <- gold_sport_distribution %>%
  slice_max(Count, n = 10)

print(top_sports)

# Relación entre edad y ganar medalla de oro
gold_age_summary <- gold_medalists %>%
  summarise(
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE),
    Avg_Age = mean(Age, na.rm = TRUE)
  )

# Imprimir resumen de edad
print(gold_age_summary)

# Gráfico 1: Distribución de medallistas por sexo
ggplot(data = gold_sex_distribution, aes(x = Sex, y = Count, fill = Sex)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Distribución de Medallistas de Oro por Sexo",
    x = "Sexo",
    y = "Cantidad de Medallistas"
  )

# Gráfico 2: Top 10 deportes con más medallistas de oro
ggplot(data = top_sports, aes(x = reorder(Sport, -Count), y = Count, fill = Sport)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Top 10 Deportes con Más Medallas de Oro",
    x = "Deporte",
    y = "Cantidad de Medallas"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Resumen de edades de medallistas de oro
gold_age_summary_df <- as.data.frame(gold_age_summary) # Convertir a dataframe
ggplot(data = gold_age_summary_df, aes(x = c("Min_Age", "Max_Age", "Avg_Age"), y = c(Min_Age, Max_Age, Avg_Age))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(
    title = "Resumen de Edades de Medallistas de Oro",
    x = "Categoría",
    y = "Edad"
  )
