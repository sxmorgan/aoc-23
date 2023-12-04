library(here)
library(tidyverse)

input <- read_lines(here('day-4','testinput.txt'))
input <- read_lines(here('day-4','input.txt'))

# PART I ----
input %>%
    str_split(fixed(':')) %>%
    enframe() %>%
    mutate(value = map(value, ~ tail(., 1) %>%
                         str_split(fixed('|')) %>%
                           flatten() )) %>%
    mutate(winning = map(value, ~ head(., 1) %>%
                             trimws() %>% str_split(' ') %>%
                             flatten() %>%
                             stringi::stri_remove_empty() %>%
                             as.numeric() )) %>%
    mutate(actual = map(value, ~ tail(., 1) %>%
                             trimws() %>% str_split(' ') %>%
                             flatten() %>%
                             stringi::stri_remove_empty() %>%
                             as.numeric() )) %>%
    mutate(matches = map2_dbl(
        winning, actual, ~sum(.y %in% .x))) %>%
    select(-value) %>% rename(game='name') %>%
    mutate(game = as.character(game)) -> tidy

tidy %>%
    mutate(exponent = ifelse(matches!=0, matches-1, 0)) %>%
    mutate(score = ifelse(matches==1, 1,
                          ifelse(matches==0, 0, 2^exponent))) %>%
    pull(score) %>%
    sum() # 21959

# PART II ----
df.tally <- mutate(tidy, copies=1)
# vector to merge with df.tally every iteration
tally <- df.tally$copies
names(tally) <- df.tally$game   

for (x in seq_len(nrow(df.tally))) {
    # figure out indices and #copies won
    matches <- filter(df.tally, game==x) %>% pull(matches)
    copies <- filter(df.tally, game==x) %>% pull(copies)
    if (matches >= 1) {
        start <- x+1
        end <- x+matches }
    else next()

    # update tally vector according to copies
    for (i in seq_len(copies)) {
        tally[c(start:end)] <- tally[c(start:end)]+1 }
    # update df.tally
    tmp <- enframe(tally) %>%
        rename(game='name', copies='value')
    df.tally <- select(df.tally, -copies) %>%
        left_join(tmp)
}

df.tally %>%
    pull(copies) %>%
    sum() # 5132675
