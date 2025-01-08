library(markovchain)
library(igraph)

# Diretórios de entrada e saída
input_dir <- "C:/Users/USER/Desktop/csvs_normalizados_3dec"
output_dir <- "C:/Users/USER/Desktop/TCC_Resultados/Simulacao"

# Criar diretório de saída, caso não exista
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Listar arquivos CSV no diretório de entrada
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# Tolerância para verificar convergência
epsilon <- 1e-5

# Função para simular passes até atingir o estado estacionário
simulate_to_stationary <- function(transition_matrix, initial_player, stationary_distribution, epsilon) {
  states <- rownames(transition_matrix)
  current_player <- initial_player
  sequence <- c(current_player)
  
  # Inicializar distribuição de probabilidades
  current_distribution <- numeric(length(states))
  names(current_distribution) <- states
  current_distribution[current_player] <- 1
  
  steps <- 0
  repeat {
    steps <- steps + 1
    next_probabilities <- transition_matrix[current_player, ]
    next_player <- sample(states, size = 1, prob = next_probabilities)
    sequence <- c(sequence, next_player)
    current_player <- next_player
    
    # Atualizar a distribuição atual
    current_distribution <- current_distribution %*% transition_matrix
    
    # Verificar convergência
    if (max(abs(current_distribution - stationary_distribution)) < epsilon) {
      break
    }
  }
  
  return(list(sequence = sequence, steps = steps))
}

# Loop sobre cada arquivo CSV
for (file_name in csv_files) {
  # Ler a matriz normalizada
  normalized_data <- read.csv(file_name, sep = ";", row.names = 1)
  normalized_matrix <- as.matrix(normalized_data)
  
  # Garantir que as linhas somam 1
  if (any(rowSums(normalized_matrix) == 0)) {
    stop("Erro: Existem linhas com soma zero na matriz de transição. Verifique os dados.")
  }
  normalized_matrix <- normalized_matrix / rowSums(normalized_matrix)
  
  # Calcular a distribuição estacionária
  stationary_distribution <- steadyStates(new("markovchain", transitionMatrix = normalized_matrix))
  stationary_distribution <- as.numeric(stationary_distribution)
  
  # Identificar o jogador inicial (mais influente)
  states <- rownames(normalized_matrix)
  initial_player <- states[which.max(stationary_distribution)]
  
  # Inicializar vetores para armazenar os números de passes dados e recebidos por jogador
  total_passes_dados <- numeric(length(states))
  total_passes_recebidos <- numeric(length(states))
  
  # Simulações de passes até o estado estacionário
  for (sim in 1:100) {
    result <- simulate_to_stationary(normalized_matrix, initial_player, stationary_distribution, epsilon)
    pass_sequence <- result$sequence
    total_steps <- result$steps
    
    # Criar data frame de transições
    transitions <- data.frame(
      De = head(pass_sequence, -1),
      Para = tail(pass_sequence, -1),
      Passo = seq_along(head(pass_sequence, -1)),  # Adicionar a ordem dos passes
      stringsAsFactors = FALSE
    )
    
    # Criar grafo a partir dos estados originais (todos os vértices)
    graph <- graph_from_data_frame(
      transitions,
      directed = TRUE,
      vertices = data.frame(name = states)
    )
    
    # Configurar atributos do grafo
    V(graph)$size <- stationary_distribution * 50  # Tamanho proporcional à influência
    V(graph)$label <- V(graph)$name  # Nome do jogador como rótulo
    V(graph)$label.cex <- 1.0  # Tamanho da fonte dos rótulos
    V(graph)$label.color <- "darkblue"
    V(graph)$color <- "yellow"
    V(graph)$label.dist <- -1  # Posicionar os nomes abaixo dos nós
    
    # Configuração das arestas
    E(graph)$label <- transitions$Passo  # Enumerar arestas conforme a sequência de passes
    E(graph)$label.cex <- 2.0  # Tamanho da fonte dos rótulos das arestas
    E(graph)$width <- 1  # Largura das arestas
    E(graph)$color <- "lightblue"  # Cor das arestas
    
    # Resolver passes bidirecionais com múltiplas arestas
    graph <- simplify(graph, remove.multiple = FALSE, remove.loops = TRUE)
    
    # Posicionar rótulos das arestas acima da linha da aresta
    edge_coords <- layout_as_tree(graph)  # Layout para os nós
    edge_coords <- data.frame(x = edge_coords[, 1], y = edge_coords[, 2])
    
    # Ajustar a posição dos rótulos das arestas
    edge_coords$label_x <- edge_coords$x + 0.05  # Ajustar posição X
    edge_coords$label_y <- edge_coords$y + 0.05  # Ajustar posição Y
    
    # Salvar gráfico de rede como PNG
    grafico_rede_file <- file.path(output_dir, sub("\\.csv$", paste0("_grafo_rede_simulacao_", sim, ".png"), basename(file_name)))
    png(grafico_rede_file, width = 800, height = 800)
    plot(
      graph,
      edge.arrow.size = 0.5,
      main = paste("Simulação de Passes iniciada por", initial_player,"\n", basename(file_name), "- Simulação", sim, "- Passos:", total_steps),
      edge.label = E(graph)$label,
      edge.label.cex = 0.8,
      edge.label.color = "black",
      edge.label.dist = 0.5
    )
    dev.off()
    
    # Contabilizar os passes dados e recebidos por jogador
    for (i in 1:(length(pass_sequence) - 1)) {
      player_from <- pass_sequence[i]
      player_to <- pass_sequence[i + 1]
      
      total_passes_dados[which(states == player_from)] <- total_passes_dados[which(states == player_from)] + 1
      total_passes_recebidos[which(states == player_to)] <- total_passes_recebidos[which(states == player_to)] + 1
    }
    
  }
  
  # Calcular as médias de passes dados e recebidos para cada jogador
  mean_passes_dados <- total_passes_dados / 100
  mean_passes_recebidos <- total_passes_recebidos / 100
  
  # Somar passes dados e recebidos para cada jogador
  total_passes <- mean_passes_dados + mean_passes_recebidos
  
  # Criar um data frame com as médias para os passes dados e recebidos
  df <- data.frame(
    Jogador = states,
    Passes = total_passes
  )
  
  # Gerar gráfico da distribuição estacionária
  grafico_file <- file.path(output_dir, sub("\\.csv$", "_barplot_passes_media.png", basename(file_name)))
  png(grafico_file,, width = 900, height = 600)
  
  par(mar = c(12, 5, 8, 3)) # Aumentar a margem inferior
  
  barplot(df$Passes, 
    names.arg = df$Jogador, 
    las = 2, 
    col = "gold",
    main = paste("Média do Total de Passes das simulações \n", basename(file_name)),
    ylab = "Número Médio de Passes",
    xlab = "",
    border = "blue",
    cex.names = 0.8,
    cex.lab = 1)
                             
                            
  # Adicionar o rótulo do eixo X abaixo do gráfico
  mtext("Jogadores", side = 1, line = 9, cex = 1)
  
  dev.off()
  
 
  cat("Gráfico de barras gerado para", basename(file_name), "\n")
}
