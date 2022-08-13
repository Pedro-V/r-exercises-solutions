
# https://www.beecrowd.com.br/judge/en/problems/view/1028

collectable_cards <- function(amount1, amount2) {
    if (amount2 == 0) {
        return(amount1)
    }

    collectable_cards(amount2, amount1 %% amount2)
}
