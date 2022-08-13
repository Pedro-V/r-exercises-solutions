# https://www.codeabbey.com/index/task_view/neumanns-random-generator

# Dado um número, calcular quantas iterações até que os algoritmo retorne
# um número totalmente zerado

# recebe um número, retorna uma string formatada de tamanho len. Adiciona zeros
# à esquerda
complete_num <- function(num, len) {
    string_num <- as.character(num)
    while (nchar(string_num) < len) {
        string_num <- paste("0", string_num, sep = "")
    }

    string_num
}

# recebe um número em string, eleva ao quadrado e retorna truncado
exp_and_truncate <- function(num_string) {
    num <- as.integer(num_string) ** 2

    filled_num_str <- complete_num(num, 8)
    substr(filled_num_str, 3, 6)
}

neumanns_rng <- function(num_string) {
    val <- num_string
    previous_nums <- c(val)
    counter <- 0L

    repeat {
        val <- exp_and_truncate(val)
        # Basicamente, verificamos se um dado número já ocorreu no nosso loop
        # se sim, então retorna da função quantas iterações levaram
        # se não, continua o loop
        counter <- counter + 1
        if (val %in% previous_nums) {
            return(counter)
        }
        previous_nums <- c(previous_nums, val)
    }
}

neumanns_rng("4100")