not_quite_lisp <- function(parentheses) {
    counter <- 1L
    floor <- 0L
    while (!is.na(substr(parentheses, counter, counter))) {
        if (substr(parentheses, counter, counter) == "(") {
            floor <- floor + 1
        } else {
            floor <- floor - 1
        }
        counter <- counter + 1
    }
    floor
}

not_quite_lisp("(()")