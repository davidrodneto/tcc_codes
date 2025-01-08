library(igraph)
library(ggplot2)
library(reshape2)

# Diretórios de entrada e saída
input_dir <- "C:/Users/USER/Desktop/csvs_normalizados_3dec"
output_dir <- "C:/Users/USER/Desktop/TCC_Resultados"

# Criar o diretório de saída, caso não exista
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Listar arquivos CSV no diretório de entrada
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# Loop sobre cada arquivo CSV
for (file_name in csv_files) {
  
  # Carregar os dados normalizados
  normalized_data <- read.csv(file_name, sep = ";", row.names = 1)
  
  # Converter para matriz
  normalized_matrix <- as.matrix(normalized_data)
  
  # Normalizar as linhas para garantir que somem 1
  normalized_matrix <- normalized_matrix / rowSums(normalized_matrix)
  
  # Ajustar os nomes dos estados (jogadores)
  states <- rownames(normalized_matrix)
  colnames(normalized_matrix) <- states
  rownames(normalized_matrix) <- states
  
  # Ordenar o eixo X (jogadores de destino) em ordem crescente
  states_sorted_x <- sort(states)
  
  # Ordenar o eixo Y (jogadores de origem) em ordem decrescente
  states_sorted_y <- rev(sort(states))
  
  # Reordenar a matriz com os estados ordenados
  normalized_matrix_sorted <- normalized_matrix[states_sorted_y, states_sorted_x]
  
  # Criar tabela de calor com os dados ordenados
  heatmap_data <- melt(normalized_matrix_sorted)
  colnames(heatmap_data) <- c("De", "Para", "Probabilidade")
  
  # Ajuste do eixo dos jogadores de origem (decrescente) e destino (crescente)
  heatmap_plot <- ggplot(heatmap_data, aes(x = Para, y = De, fill = Probabilidade)) +
    geom_tile(color = "gray") +  # Adiciona bordas cinzas às células
    scale_fill_gradient(low = "lightyellow", high = "red") +  # Gradiente branco-vermelho
    labs(title = paste("Tabela de Calor - Probabilidade de Passes\n", basename(file_name)),
         x = "Jogador de Destino", y = "Jogador de Origem", fill = "Probabilidade") +
    theme_minimal(base_size = 12) +  # Tema com fundo branco
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(color = "black", face = "bold", size = 14, hjust = 0.5),
          axis.title.x = element_text(color = "black", face = "bold", size = 12),
          axis.title.y = element_text(color = "black", face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"))  # Eixo Y com nomes em ordem decrescente
  
  # Definir o nome do arquivo de saída no diretório de saída
  heatmap_file <- file.path(output_dir, paste0(basename(file_name), "_heatmap.png"))
  
  # Salvar tabela de calor
  ggsave(heatmap_file, heatmap_plot, width = 10, height = 8, bg = "white")  # Salvar com fundo branco
  
  print(paste("Tabela de calor salva em:", heatmap_file))
}
