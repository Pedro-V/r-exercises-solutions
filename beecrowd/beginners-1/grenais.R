#https://www.beecrowd.com.br/judge/en/problems/view/1131
# Escrever um programa que vai recebendo input de grenais
# e retorna um sumário simples

library(glue)
resultados <- c(0, 0, 0)
count_grenais <- 0

checa_vencedor <- function(gols){
  if(gols[1] > gols[2]){
    return(c(1, 0, 0))
  }
  else if(gols[1] == gols[2]){
    return(c(0, 0, 1))
  }
  else{
    return(c(0, 1, 0))
  }
}

repeat{
  resultados <- resultados + (readline() |> strsplit(" "))[[1]] |>
    as.integer() |> checa_vencedor()
  count_grenais <- count_grenais + 1
  print("Novo grenal? (1-sim 2-nao)")
  
  continuar <- readline() |> as.integer()
  if(continuar == 2){
    paste(count_grenais, "grenais") |> message()
    paste("Inter:", resultados[1], sep = "") |> message()
    paste("Gremio:", resultados[2], sep = "") |> message()
    paste("Empates:", resultados[3], sep = "") |> message()
    break
  }
}