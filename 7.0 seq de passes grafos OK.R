library(markovchain)
library(igraph)

# Diretórios de entrada e saída
input_dir <- "C:/Users/USER/Desktop/RMADORtransicao"
output_dir <- "C:/Users/USER/Desktop/RMADORResultados/Simulacao"

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
  print(initial_player)
  
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
  }
}
