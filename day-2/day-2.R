library(here)
library(tidyverse)

input <- read_lines(here('day-2','testinput.txt'))
input <- read_lines(here('day-2','input.txt'))

target <- c(red=12, green=13, blue=14)

## PART I ----
# nested df with game # and subsets as charvecs
input %>%
    str_split(fixed(':')) %>%
    enframe() %>%
    rename(game='name') %>%
    mutate(value=map_chr(value, ~pluck(., 2) %>%
                         trimws())) %>%
    mutate(value=str_split(value, fixed(';')) %>%
               map(~ trimws(.))) %>%
    mutate(subsets=map(value, ~ str_split(., fixed(',')) %>%
                           map(~trimws(.)))) %>%
    select(-value) -> tmp

# per-game listcols for blue, green, red extracted cube #s
df <- tmp %>%
    mutate(idx = map(subsets, ~ map(., ~ str_detect(., 'blue')))) %>%
    mutate(blue = map2(
        subsets, idx, ~ map2(
            .x, .y, function(.sub, .idx) {
        .sub[.idx] })) ) %>%
    mutate(blue = map(blue, ~ map(., function(x) {
        ifelse(length(x)==0, 0, readr::parse_number(x)) }) %>%
            unlist())) %>%
    mutate(idx = map(subsets, ~ map(., ~ str_detect(., 'green')))) %>%
    mutate(green = map2(
        subsets, idx, ~ map2(
            .x, .y, function(.sub, .idx) {
                .sub[.idx] })) ) %>%
    mutate(green = map(green, ~ map(., function(x) {
        ifelse(length(x)==0, 0, readr::parse_number(x)) }) %>%
            unlist())) %>%
    mutate(idx = map(subsets, ~ map(., ~ str_detect(., 'red')))) %>%
    mutate(red = map2(
        subsets, idx, ~ map2(
            .x, .y, function(.sub, .idx) {
                .sub[.idx] })) ) %>%
    mutate(red = map(red, ~ map(., function(x) {
        ifelse(length(x)==0, 0, readr::parse_number(x)) }) %>%
            unlist())) %>%
    select(-idx)

check <- function(target, bv, gv, rv) {
    b <- target['blue']; g <- target['green']; r <- target['red']
    # browser()
    # all pulls need to be less than target vector
    c1 <- all(bv<=b); c2 <- all(gv<=g); c3 <- all(rv<=r)
    if (c1 & c2 & c3) return(TRUE)
    else return(FALSE)
}

exnums <- df %>%
    select(-subsets) %>%
    mutate(possible = pmap_lgl(list(blue, green, red),
                               ~ check(target, ..1, ..2, ..3)))

exnums %>%
    filter(possible) %>%
    pull(game) %>%
    sum()

# PART II ----

max <- exnums %>%
    select(-possible) %>%
    mutate(blue=map_dbl(blue, max)) %>%
    mutate(green=map_dbl(green, max)) %>%
    mutate(red=map_dbl(red, max)) %>%
    mutate(power=blue*green*red)

max %>%
    pull(power) %>%
    sum()
