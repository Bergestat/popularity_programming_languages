# Carregar o pacote necessário
library(dplyr)

# Carregar os dados
data <- read.csv("stack_overflow_data.csv")

# Filtrar os dados para o ano de 2020 e para o tag 'r'
r_2020_data <- data %>%
  filter(year == 2020, tag == "r") %>%
  mutate(percentage = (num_questions / year_total) * 100)

# Selecionar as colunas desejadas
r_2020 <- r_2020_data %>%
  select(year, tag, num_questions, year_total, percentage)

# Exibir o resultado
print(r_2020)

# Filtrar os dados para o intervalo de 2015 a 2020
data_2015_2020 <- data %>%
  filter(year >= 2015, year <= 2020)

# Agrupar por tag e somar o número de perguntas para cada tag
total_questions_by_tag <- data_2015_2020 %>%
  group_by(tag) %>%
  summarise(total_questions = sum(num_questions)) %>%
  arrange(desc(total_questions))

# Selecionar as 5 tags com o maior número de perguntas
highest_tags <- head(total_questions_by_tag$tag, 5)

# Exibir o resultado
print(highest_tags)

# Carregar o pacote de visualização
library(ggplot2)

# Gráfico de barras para as 5 tags mais populares entre 2015 e 2020
ggplot(total_questions_by_tag[1:5, ], aes(x = reorder(tag, total_questions), y = total_questions)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 Tags Mais Populares (2015-2020)", x = "Tag", y = "Total de Perguntas") +
  theme_minimal()

