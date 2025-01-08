library(httr)
library(jsonlite)
library(stringi)
library(data.table)

# Headers da requisição
headers <- c(
  'Accept' = 'application/json',
  'Accept-Language' = 'en-US,en;q=0.9,pt;q=0.8',
  'Authorization' = 'Bearer 72fa6abf-408',
  'Connection' = 'keep-alive',
  'Content-Type' = 'application/json',
  'Origin' = 'https://www.footstats.com.br',
  'Referer' = 'https://www.footstats.com.br/',
  'Sec-Fetch-Dest' = 'empty',
  'Sec-Fetch-Mode' = 'cors',
  'Sec-Fetch-Site' = 'cross-site',
  'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36',
  'sec-ch-ua' = '"Not/A)Brand";v="8", "Chromium";v="126", "Google Chrome";v="126"',
  'sec-ch-ua-mobile' = '?0',
  'sec-ch-ua-platform' = '"macOS"'
)

# Função para criar diretórios
create_dir <- function(newpath) {
  if (!dir.exists(newpath)) {
    dir.create(newpath, recursive = TRUE)
  }
}

# Função para realizar requisição
request_interactions <- function(url) {
  response <- GET(url, add_headers(.headers = headers))
  if (status_code(response) == 200) {
    cat("Request 200 for:", url, "\n")
    return(fromJSON(content(response, "text", encoding = "UTF-8")))
  } else {
    cat("Bad request for:", url, "\n")
    return(NULL)
  }
}

# Função para salvar JSON
save_json_match <- function(round, id_match, json_interactions) {
  filepath <- sprintf("C:/Users/USER/Desktop/jsons/%s_%s.json", round, id_match)
  write_json(json_interactions, filepath)
}

# Função para ler JSON
read_json_match <- function(file_path) {
  return(fromJSON(file_path, simplifyVector = TRUE))
}

# Função para remover acentos
remove_accents <- function(string) {
  stri_trans_general(string, "Latin-ASCII")
}

generate_transition_matrix <- function(adj_matrix) {
  repeat {
    # Identificar linhas e colunas zeradas
    zero_rows <- which(rowSums(adj_matrix) == 0)
    zero_cols <- which(colSums(adj_matrix) == 0)
    
    # Combinar índices de linhas e colunas para remover
    remove_indices <- union(zero_rows, zero_cols)
    
    # Interromper o loop se nenhuma linha ou coluna precisar ser removida
    if (length(remove_indices) == 0) {
      break
    }
    
    # Capturar os nomes das linhas e colunas a serem removidas
    removed_names <- rownames(adj_matrix)[remove_indices]
    
    # Exibir informações sobre os elementos removidos
    cat("Removendo linhas e colunas:", paste(removed_names, collapse = ", "), "\n")
    
    # Remover linhas e colunas correspondentes
    adj_matrix <- adj_matrix[-remove_indices, -remove_indices, drop = FALSE]
  }
  
  # Soma dos elementos de cada linha
  row_sums <- rowSums(adj_matrix)
  
  # Normalizar as linhas (evitando divisão por zero)
  transition_matrix <- sweep(adj_matrix, 1, row_sums, FUN = "/")
  
  # Substituir NaNs por 0 (para linhas originalmente zeradas)
  transition_matrix[is.nan(transition_matrix)] <- 0
  
  # Arredondar os valores para 3 casas decimais
  transition_matrix <- round(transition_matrix, 3)
  
  return(transition_matrix)
}


# Função para criar as matrizes
generate_matrices <- function(json_interactions) {
  teams <- list()
  
  # Organizar jogadores por time e verificar duplicados
  all_players <- json_interactions$data$jogadores[[2]]  # Nomes dos jogadores
  normalized_names <- sapply(all_players, remove_accents)
  name_counts <- table(normalized_names)
  
  for (row in 1:length(json_interactions$data$jogadores[[1]])) {
    id_team <- json_interactions$data$jogadores[[3]][[row]]
    player_name <- json_interactions$data$jogadores[[2]][[row]]
    player_id <- json_interactions$data$jogadores[[1]][[row]]
    
    normalized_name <- remove_accents(player_name)
    if (name_counts[normalized_name] > 1) {
      final_name <- paste(normalized_name, player_id, sep="_")
    } else {
      final_name <- normalized_name
    }
    
    if (!is.null(teams[[as.character(id_team)]])) {
      teams[[as.character(id_team)]] <- c(teams[[as.character(id_team)]], final_name)
    } else {
      teams[[as.character(id_team)]] <- c(final_name)
    }
  }
  
  for (id_team in names(teams)) {
    teams[[id_team]] <- sort(teams[[id_team]])
  }
  
  keys <- names(teams)
  list_a <- teams[[keys[1]]]
  list_b <- teams[[keys[2]]]
  
  df_a <- matrix(0, nrow = length(list_a), ncol = length(list_a), dimnames = list(list_a, list_a))
  df_b <- matrix(0, nrow = length(list_b), ncol = length(list_b), dimnames = list(list_b, list_b))
  
  for (row in 1:length(json_interactions$data$jogadores[[1]])) {
    player_name <- json_interactions$data$jogadores[[2]][[row]]
    player_id <- json_interactions$data$jogadores[[1]][[row]]
    normalized_player_name <- ifelse(##############
      name_counts[remove_accents(player_name)] > 1, paste(normalized_name, player_id, sep="_"), remove_accents(player_name)
    )
    
    for (row_interaction in 1:length(json_interactions$data$jogadores[[4]][[row]][[1]])) {
      interaction_player_name <- json_interactions$data$jogadores[[4]][[row]][[2]][[row_interaction]]
      interaction_player_id <- json_interactions$data$jogadores[[4]][[row]][[1]][[row_interaction]]
      normalized_interaction_name <- ifelse(
        name_counts[remove_accents(interaction_player_name)] > 1, 
        paste(remove_accents(interaction_player_name), interaction_player_id, sep="_"),
        remove_accents(interaction_player_name)
      )
      
      # Quantidade de interações
      quantity_player_name <- json_interactions$data$jogadores[[4]][[row]][[3]][[row_interaction]]
      
      # Normalizar o nome do jogador original (também com id)
      normalized_player_name <- ifelse(
        name_counts[remove_accents(player_name)] > 1,
        paste(remove_accents(player_name), player_id, sep = "_"),
        remove_accents(player_name)
      )
      
      # Verificar e atualizar a matriz do time A
      if (normalized_player_name %in% rownames(df_a) && normalized_interaction_name %in% colnames(df_a)) {
        df_a[normalized_player_name, normalized_interaction_name] <- 
          df_a[normalized_player_name, normalized_interaction_name] + quantity_player_name
      }
      
      # Verificar e atualizar a matriz do time B
      if (normalized_player_name %in% rownames(df_b) && normalized_interaction_name %in% colnames(df_b)) {
        df_b[normalized_player_name, normalized_interaction_name] <- 
          df_b[normalized_player_name, normalized_interaction_name] + quantity_player_name
      }
      
    }
  }
  
  transition_a <- generate_transition_matrix(df_a)
  transition_b <- generate_transition_matrix(df_b)
  
  return(list(
    df_a = df_a, key_a = keys[1], transition_a = transition_a,
    df_b = df_b, key_b = keys[2], transition_b = transition_b
  ))
}

# Função para construir informações das partidas
construct_matches <- function(calendar_dict) {
  team_names <- list()
  all_matches <- list()
  
  for (round in calendar_dict$data$rodadas) {
    for (match in round$partidas) {
      match_info <- list(
        id = match$id,
        link = sprintf("https://footstatsapiapp.azurewebsites.net/partidas/%s/fundamento/2/campeonato/916/interacao", match$id),
        round = sprintf("%s_%s", round$rodada, round$fase),
        host = match$mandante$id,
        visitor = match$visitante$id
      )
      all_matches <- append(all_matches, list(match_info))
      team_names[[as.character(match$mandante$id)]] <- match$mandante$nome
      team_names[[as.character(match$visitante$id)]] <- match$visitante$nome
    }
  }
  
  return(list(team_names = team_names, all_matches = all_matches))
}

# Execução principal
# Diretório de acordo com sua escolha, ou seja, local em que os arquivos estão salvos
main <- function() {
  create_dir("C:/Users/USER/Desktop/csvs_normalizados_3dec")
  create_dir("C:/Users/USER/Desktop/csvs")
  create_dir("C:/Users/USER/Desktop/jsons")
  
  
  calendar_file_path <- "C:/Users/USER/Desktop/calendario_champions.json"
  
  if (!file.exists(calendar_file_path)) {
    cat("Arquivo calendario_champions.json não encontrado. Realizando a requisição...\n")
    
    url_calendario <- "https://footstatsapiapp.azurewebsites.net/campeonatos/916/calendario"
    
    response <- GET(url_calendario, add_headers(.headers = headers))
    
    if (status_code(response) == 200) {
      calendar_dict <- content(response, "parsed", encoding = "UTF-8")
      write_json(calendar_dict, calendar_file_path)
      cat("Dados do calendário obtidos com sucesso!\n")
    } else {
      stop(paste("Erro na requisição:", status_code(response)))
    }
  } else {
    cat("Arquivo calendario_champions.json já existe. Usando o arquivo existente...\n")
  }
  
  calendar_dict <- fromJSON(calendar_file_path, simplifyDataFrame = FALSE)
  match_info <- construct_matches(calendar_dict)
  
  for (match in match_info$all_matches) {
    json_interactions <- request_interactions(match$link)
    if (!is.null(json_interactions)) {
      save_json_match(match$round, match$id, json_interactions)
      json_interactions <- read_json_match(sprintf("C:/Users/USER/Desktop/jsons/%s_%s.json", match$round, match$id))
      
      matrices <- generate_matrices(json_interactions)
      
      df_a <- matrices$df_a
      transition_a <- matrices$transition_a
      key_a <- matrices$key_a
      
      df_b <- matrices$df_b
      transition_b <- matrices$transition_b
      key_b <- matrices$key_b
      
      team_a_name <- match_info$team_names[[key_a]]
      team_b_name <- match_info$team_names[[key_b]]
      
      write.table(df_a, file = sprintf("C:/Users/USER/Desktop/csvs/%s_%s_%s_%s.csv", match$round, match$id, key_a, team_a_name), sep = ";", row.names = TRUE, col.names = NA, fileEncoding = 'UTF-8')
      write.table(transition_a, file = sprintf("C:/Users/USER/Desktop/csvs_normalizados_3dec/%s_%s_%s_%s_normalizado.csv", match$round, match$id, key_a, team_a_name), sep = ";", row.names = TRUE, col.names = NA, fileEncoding = 'UTF-8')
      
      write.table(df_b, file = sprintf("C:/Users/USER/Desktop/csvs/%s_%s_%s_%s.csv", match$round, match$id, key_b, team_b_name), sep = ";", row.names = TRUE, col.names = NA, fileEncoding = 'UTF-8')
      write.table(transition_b, file = sprintf("C:/Users/USER/Desktop/csvs_normalizados_3dec/%s_%s_%s_%s_normalizado.csv", match$round, match$id, key_b, team_b_name), sep = ";", row.names = TRUE, col.names = NA, fileEncoding = 'UTF-8')
      
    }
  }
  cat("Processamento concluído!\n")
}

main()
