# Vamos assumir que os valores abaixo representem os sentidos cardinais
# N -> 1
# E -> 2
# S -> 3
# W -> 4
# A função abaixo recebe um int representando o sentido atual
# e uma letra representando para qual lado irá virar (Left ou Right)
alterar_sentido <- function(int_sentido, letra_sentido){
  if(letra_sentido == 'L'){
    if(int_sentido == 1){
      return(4)
    }
    else{
      return(int_sentido - 1)
    }
  }
  else{
    if(int_sentido == 4){
      return(1)
    }
    return(int_sentido + 1)
  }
}

taxicab_distance <- function(x1, y1, x2, y2){
  abs(x1 - x2) + abs(y1 - y2)
}

# função para mover as coordenadas. Retorna um vetor
move_coord <- function(direcao_atual, x, y, passos){
  x <- switch(direcao_atual,
              x,
              x + passos,
              x,
              x - passos)
  y <- switch(direcao_atual,
              y + passos,
              y,
              y - passos,
              y)
  c(x, y)
}

# PARTE 2: Função pra converter uma coordenada em string
coord_to_string <- function(coord){
  paste(coord[1], coord[2], sep=" ")
}

# dado uma coordenada que não varia, uma coordenada que varia
# em seu estado inicial e depois em seu estado final,
# retornar os estados intermediários dessa coordenada
#ex f(2, -3, 0) -> "2 -2" "2 -1" "2 0"
vector_intermed_coords <- function(eixo_invariante, invariante, estado_inicial,
                                   estado_final){
  #browser()
  resultado <- c()
  temp <- estado_inicial
  while(temp != estado_final){
    if(estado_inicial < estado_final){
      temp <- temp + 1
    }
    else{
      temp <- temp - 1
    }
    
    if(eixo_invariante == "x"){
      coord_intermed <- coord_to_string(c(invariante, temp))
    }
    else{
      coord_intermed <- coord_to_string(c(temp, invariante))
    }
    
    resultado <- append(resultado, coord_intermed)
  }
  
  resultado
}


# calcular as coordenadas caminhadas entre duas coordenadas
# coord2 sempre é a mais recente
 intermed_coord <- function(coord1, coord2){
  x <- abs(coord2[1] - coord1[1])
  y <- abs(coord2[2] - coord1[2])
  
  # se não caminhamos nada no eixo x (x = 0)
  if(!x){
    return(vector_intermed_coords("x", coord1[1], coord1[2], coord2[2]))
  }
  else{
    return(vector_intermed_coords("y", coord1[2], coord1[1], coord2[1]))
  }
}

main <- function(input){
  # transformamos num vetor que contém os passos
  movimentos <- strsplit(input, ", ")[[1]]
  # começamos no norte
  direcao_atual <- 1
  # até onde iremos iterar
  limite <- nchar(movimentos)
  # nossas coordenadas
  coord <- c(0, 0)
  # um vetor que vai agir como registrador das posições visitadas
  visited_coords <- c(coord_to_string(coord))
  # valor que usaremos como referência para separar as strings
  indice = 1
  while (indice <= length(movimentos)){
    # analisando as instruções atuais
    mov_atual = movimentos[indice]
    lado_a_virar <- substr(mov_atual, 1, 1)
    quant_passos <- substr(mov_atual, 2,
                           nchar(mov_atual)) |>
                    as.integer()
    # atualizamos a direção
    direcao_atual <- alterar_sentido(direcao_atual,
                                       lado_a_virar)
    
    temp_coord <- move_coord(direcao_atual, coord[1],
                             coord[2], quant_passos)
    
    intermediate_coords <- intermed_coord(coord, temp_coord)
    coord <- temp_coord
    # se no processo de movimento passarmos por um lugar já visitado,
    # retornamos da função
    for (intermed in intermediate_coords) {
      if(intermed %in% visited_coords){
        intermed <- strsplit(intermed, " ")[[1]] |> as.integer()
        return(taxicab_distance(0, 0, intermed[1], intermed[2]))
      }
      visited_coords <- append(visited_coords, intermed)
    }
    # o corte avança pro próximo passo 
    indice <- indice + 1
  }
}

input <- "R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2, R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4, R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3, L1, R1, R2, L4, L2, R1, L5, L4, L5"

# main(input)