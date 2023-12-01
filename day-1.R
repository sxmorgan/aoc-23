library(tidyverse)
library(here)

infile <- read_lines(here('day-1','tmpinput2.txt'))
infile <- read_lines(here('day-1','input1.txt'))

# PART 1 -----
numlist <- gsub('\\D', '', infile)

# double single digits, extract FL from multiples
double.digits <- function(num) {
    n <- as.character(num)
    out <- case_when(
        nchar(n)==2 ~ n,
        nchar(n)==1 ~ paste0(n, n),
        TRUE ~ paste0(substr(n,1,1),
                      substr(n, nchar(n), nchar(n))))
    return(as.numeric(out))
}

ans <- map_dbl(numlist, ~ double.digits(.))
sum(ans) # 54927(!)

# PART 2 -----
spelled.nums <- c('one','two','three','four','five',
                  'six','seven','eight','nine') %>%
    enframe() %>%
    rename(num='name', char='value')
all.nums <- infile %>% 
    str_extract_all(paste0(c(spelled.nums$char, 
                             spelled.nums$num), 
                           collapse='|'))

converted <- all.nums %>% 
    # make everything digit-characters
    map(~ map_chr(., function(n) {
        if (n %in% spelled.nums$num) return(n)
        else return(as.character(
            spelled.nums$num[which(spelled.nums$char==n)])) }))

# extract first and last, paste, convert to numeric
final.nums <- converted %>%
    map_dbl(~ paste0(head(., 1),
                     tail(., 1)) %>% as.numeric())

sum(final.nums) #54607 WRONG

