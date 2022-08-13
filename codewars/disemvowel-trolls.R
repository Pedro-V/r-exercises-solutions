# https://www.codewars.com/kata/52fba66badcd10859f00097e/r
disemvowel <- function(phrase) {
    gsub("[aeiouAEIOU]", "", phrase)
}