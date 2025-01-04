# Carregar os pacotes necessários
library(markovchain)
library(expm)

# Função para verificar irredutibilidade em k passos
is_irreducible_k_steps <- function(P, max_steps = 100) {
  n <- nrow(P)
  
  # Verificar a irredutibilidade nas primeiras max_steps potências da matriz
  for (k in 1:max_steps) {
    P_k <- P %^% k  # Potência k da matriz de transição
    if (all(P_k > 0)) {  # Se todos os elementos de P^k forem > 0
      return(TRUE)
    }
  }
  return(FALSE)  # Se não encontrou nenhum k tal que todos os elementos de P^k > 0
}

# Função para calcular o máximo divisor comum (GCD)
gcd <- function(a, b) {
  if (b == 0) return(a)
  gcd(b, a %% b)
}

# Função para verificar aperiodicidade
is_aperiodic <- function(P) {
  n <- nrow(P)
  reachable_times <- vector("list", n)
  
  # Testar várias potências da matriz para encontrar acessibilidade
  for (k in 1:n) {
    P_k <- P %^% k
    for (i in 1:n) {
      if (P_k[i, i] > 0) {
        reachable_times[[i]] <- c(reachable_times[[i]], k)
      }
    }
  }
  
  gcd_values <- sapply(reachable_times, function(times) {
    if (length(times) > 1) {
      return(Reduce(gcd, times))
    } else {
      return(times[1])
    }
  })
  
  return(all(gcd_values == 1))
}

# Diretórios de entrada e saída
input_dir <- "C:/Users/USER/Desktop/RMADORtransicao"
output_dir <- "C:/Users/USER/Desktop/RMADORResultados"

# Criar diretório de saída, caso não exista
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Listar arquivos CSV no diretório de entrada
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# Loop sobre cada arquivo CSV
for (file_name in csv_files) {
  normalized_data <- read.csv(file_name, sep = ";", row.names = 1)
  normalized_matrix <- as.matrix(normalized_data)
  
### Verificar e corrigir somas das linhas
  if (any(rowSums(normalized_matrix) == 0)) {
    stop("Erro: Existem linhas com soma zero na matriz de transição. Verifique os dados.")
  }
  normalized_matrix <- normalized_matrix / rowSums(normalized_matrix)
  
  states <- rownames(normalized_matrix)
  colnames(normalized_matrix) <- states
  rownames(normalized_matrix) <- states
  
  markov_model <- new("markovchain", 
                      transitionMatrix = normalized_matrix, 
                      states = states, 
                      name = "Modelo Normalizado")
  
  # Verificar irredutibilidade
  if (is_irreducible_k_steps(normalized_matrix)) {
    cat("A cadeia no arquivo", basename(file_name), "é irredutível.\n")
    
    # Verificar aperiodicidade
    if (is_aperiodic(normalized_matrix)) {
      cat("A cadeia no arquivo", basename(file_name), " também é aperiódica e portanto ergódica .\n")
    } else {
      cat("A cadeia no arquivo", basename(file_name), "não é aperiódica.\n")
      diag(normalized_matrix) <- diag(normalized_matrix) + 0.01
      normalized_matrix <- normalized_matrix / rowSums(normalized_matrix)
      markov_model <- new("markovchain", 
                          transitionMatrix = normalized_matrix, 
                          states = states, 
                          name = "Modelo Ajustado")
      cat("A cadeia foi ajustada para ser ergódica.\n")
    }
    
    # Calcular a distribuição estacionária
    stationary_distribution <- steadyStates(markov_model)
    stationary_distribution <- round(as.numeric(stationary_distribution), 3)
    stationary_df <- data.frame(Jogador = states, Vetor_Estacionario = stationary_distribution)
    stationary_df <- stationary_df[order(stationary_df$Vetor_Estacionario, decreasing = TRUE), ]
    
    # Extrair o nome do time a partir do nome do arquivo
    partida <- unlist(strsplit(basename(file_name), "_"))  # Dividir o nome do arquivo
    time <- partida[length(partida) - 1]  # O penúltimo elemento será o time da casa
    
    # Salvar distribuição estacionária
    nome_vetor_estacionario <- sub(".*/", "", file_name)
    output_file <- file.path(output_dir, sub("\\.csv$", "_distribuicao_estacionaria.csv", nome_vetor_estacionario))
    write.table(stationary_df, output_file, row.names = FALSE, sep = ";", dec = ".", quote = FALSE)
    
    # Identificar jogador mais fluente
    fluente_jogador <- states[which.max(stationary_distribution)]
    
    # Calcular a matriz de transição até a convergência
    epsilon <- 1e-5
    max_iterations <- 1000
    steps <- 0
    current_matrix <- normalized_matrix
    
    while (steps < max_iterations) {
      steps <- steps + 1
      next_matrix <- current_matrix %*% normalized_matrix
      
      if (max(abs(next_matrix - current_matrix)) < epsilon) {
        break
      }
      
      current_matrix <- next_matrix
    }
    
    # Imprimir a última matriz após a convergência e o número de passos
    cat("\nÚltima matriz P^k após a convergência:\n")
    print(current_matrix)
    cat("\nNúmero de passos até a convergência (k):", steps, "\n")
    
    # Salvar a última matriz P^k em um arquivo CSV com ponto e vírgula
    final_matrix_file <- file.path(output_dir, sub("\\.csv$", "_matriz_final.csv", nome_vetor_estacionario))
    
    # Salvar com os nomes dos jogadores nas linhas e colunas
    write.table(current_matrix, final_matrix_file, row.names = TRUE, col.names = NA, sep = ";", dec = ".", quote = FALSE)
    
    # Salvar jogador mais fluente e passos até a convergência
    fluent_player_file <- file.path(output_dir, sub("\\.csv$", "_jogador_fluente.txt", basename(file_name)))
    write(paste("Jogador mais fluente:", fluente_jogador), fluent_player_file)
    write(paste("Passos até a convergência:", steps), fluent_player_file, append = TRUE)
    
    # Gerar gráfico da distribuição estacionária
    grafico_file <- file.path(output_dir, sub("\\.csv$", "_barplot_distribuicao_estacionaria.png", basename(file_name)))
    png(grafico_file)
    
    par(mar = c(12, 5, 8, 3)) # Aumentar a margem inferior
    
    barplot_heights <- barplot(stationary_distribution, 
                               names.arg = states, 
                               las = 2, 
                               col = "gold",
                               main = paste("Distribuição Estacionária - Influência dos Jogadores\n do", time),
                               ylab = "Probabilidade",
                               xlab = "",
                               cex.names = 0.8,
                               cex.lab = 1)
    
    # Adicionar o rótulo do eixo X abaixo do gráfico
    mtext("Jogadores", side = 1, line = 9, cex = 1)
    
    text(barplot_heights, stationary_distribution, 
         labels = stationary_distribution, 
         pos = 3, cex = 0.7, col = "blue")
    dev.off()
  } else {
    cat("A cadeia no arquivo", basename(file_name), "não é irredutível.\n")
  }
}
