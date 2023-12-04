library(here)
library(tidyverse)

input <- read_lines(here('day-3','testinput.txt'))
# input <- read_lines(here('day-3','input.txt'))

nrows <- length(input)
ncols <- nchar(input[1])
pars <- c(nrows, ncols)

# PART I ----
locs <- input %>%
    enframe() %>%
    mutate(digit.idx = map(
        value, ~ str_locate_all(., '[:digit:]') %>%
            map_dfr(~ as.data.frame(.) %>%
                        rename(pos='start') %>%
                        select(-end)) )) %>%
    mutate(punct.idx = map(
        value, ~ str_locate_all(., '[:punct:]') %>%
            map_dfr(~ as.data.frame(.) %>%
                        rename(pos='start') %>%
                        select(-end)) )) %>%
    mutate(dot.idx = map(
        value, ~ str_locate_all(., '[.]') %>%
            map_dfr(~ as.data.frame(.) %>%
                        rename(pos='start') %>%
                        select(-end)) )) %>%
    # mutate(sym.idx = map2(punct.idx, dot.idx, function(.p, .d) {
    #     idx <- setdiff(.p$start, .d$start)
    #     .p[idx,] }))
    mutate(sym.idx = map2(punct.idx, dot.idx, ~ setdiff(.x, .y))) %>%
    select(-punct.idx, -dot.idx)

makevec <- function(df) {
    tmp <- df$data
    names(tmp) <- df$name
    return(tmp)
}

nums <- locs %>%
    mutate(digits = map2(value, digit.idx, function(.v, .i) {
        tmp <- cumsum(c(TRUE, diff(.i$pos)!=1)) %>%
            str_c('N', .)
        str_sub(.v, .i$pos, .i$pos) %>% 
            as.numeric() %>%
            set_names(tmp) })) %>%
    mutate(tmp = map(digits, ~ enframe(.) %>%
                          nest(-name) %>%
                          mutate(data=map_dbl(
                              data, ~pull(., value) %>%
                                  as.character() %>%
                                  str_c(collapse='') %>%
                                  as.numeric()  )) )) %>%
    mutate(nums = map(tmp, ~ makevec(.))) %>%
    select(-tmp) %>%
    # df with digit, position of digit, and which # in row its part of
    mutate(data = map2(digits, digit.idx, function(d, d.idx) {
        d %>% enframe() %>% bind_cols(d.idx) })) %>%
    # add repeating col to indicate what value of # digits are part of
    mutate(all = pmap(list(digit.idx, sym.idx,
                           digits, nums, data), ))
    mutate(data = map2(data, nums, function(dat, num) {
        tmp <- num %>% enframe() %>%
            rename(n='value')
        left_join(dat, tmp) }))

# find symbol-adjacent nums in a given row
look.in.row <- function(d.idx, s.idx, dat) {
    browser()
    if (length(s.idx)==0) return(0)
    else {
        
    }
}

nums %>%
    mutate(tmp=pmap(list(digit.idx, sym.idx, data),
                    ~ look.in.row(..1, ..2, ..3) ))

# find symbol-adjacent nums above, below, or diagonally
lookaround <- function(row, dir, pars,
                       d.idx, s.idx, n) {
    # check edges of map
    if (row==1 & dir=='N') stop('north edge reached')
    if (row==pars[1] & dir=='S') stop('south edge reached')
}

