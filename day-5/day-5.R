library(here)
library(tidyverse)

input <- read_lines(here('day-5','testinput.txt'))
input <- read_lines(here('day-5','input.txt'))

# PART I ----
seeds <- input[1] %>%
    str_split(fixed(':')) %>%
    map_chr(~ tail(., 1) %>% trimws()) %>%
    str_split(' ') %>%
    pluck(1) %>%
    as.numeric()
input <- input[-c(1:2)]
map.names <- input[which(str_detect(input, 'map')==TRUE)] %>%
    str_remove_all(' map:') 

# indices of numbers to extract for each map
which(str_detect(input, 'map')==TRUE) +1 -> starts
c(which(input=='')-1, length(input)) -> ends

# from the input file
extract_nums <- function(char) {
    nums <- as.numeric(str_split(char, ' ')[[1]])
    tibble(src.idx=nums[2],
           dest.idx=nums[1],
           rng.len=nums[3])
}

# for a single row with source+dest idx and ranges
create_range <- function(start.idx, range.len) {
    tmp <- c(start.idx)
    for (i in seq_len(range.len-1)) {
        tmp <- c(tmp, start.idx+i) }
    return(tmp)
}

# don't fill in blanks yet
make_maps <- function(df.idx) {
    # list out indices specified by src/dest ranges
    df.rng <- df.idx %>%
        mutate(src.rng = map2(
            src.idx, rng.len, ~create_range(.x, .y) )) %>%
        mutate(dest.rng = map2(
            dest.idx, rng.len, ~create_range(.x, .y) )) %>%
        mutate(map = map2(
            src.rng, dest.rng, ~tibble(src = .x,
                                       dest = .y) )) #%>% 
        # pull(map) %>%
        # bind_rows() %>% 
        # arrange(src)
    return(df.rng)
}

df.tmp1 <- tibble(map=map.names,
                 line.start=starts,
                 line.end=ends) %>%
    mutate(entries = map2(
        line.start, line.end, function(.x, .y) {
            map_dfr(c(.x:.y), ~extract_nums(input[.])) }))
# slow
df.tmp <- df.tmp1 %>%
    mutate(maps = furrr::future_map(entries, ~make_maps(.) )) %>%
    select(map, maps)

nseedlocs <- df.tmp$maps[[1]]$map %>% 
    bind_rows() %>% pull(src) %>% max()

# fill in the simple src=dest entries
populate_ranges <- function(df.rng, map.name, target) {
    # browser()
    tmp <- bind_rows(df.rng$map) %>% arrange(src)
    tmp.src <- bind_rows(df.rng$map) %>% arrange(src) %>% pull(src)
    tmp.rng <- range(tmp.src)
    missing <- c()

    if (tmp.rng[1] != 0) missing <- setdiff(c(0:tmp.rng[2]), 
                                             tmp.src)
    if (tmp.rng[2] < (target-1)) missing <- c(missing,
                                          setdiff(c((tmp.rng[2]+1):target-1), 
                                                 tmp.src))
    for (n in missing) { tmp <- add_row(tmp, src=n, dest=n) }
    tmp.idx <- which(!c(0:(target-1)) %in% sort(tmp$src))
    if (length(tmp.idx) != 0) {
        for (i in tmp.idx) { tmp <- add_row(tmp, src=i, dest=i) }}
    arrange(tmp, src)
}
df.map <- mutate(df.tmp, items = map2(
    maps, map, ~populate_ranges(.x, .y, nseedlocs)))

get_dest <- function(map, map.entry) {
    filter(map, src==map.entry) %>%
        pull(dest)
}

# can't just purr need to do custom iteration over nested df
get_seed_locations <- function(seed.no, map) {
    # browser()
    src.idx <- seed.no
    v <- c(seed.no)
    for (i in seq_len(nrow(map))) {
        src.idx <- get_dest(map$items[[i]], src.idx)
        v <- c(v, src.idx) }
    # browser()
    return(v[length(v)])
}

map(seeds, ~ get_seed_locations(., df.map)) %>%
    unlist() %>%
    min()

