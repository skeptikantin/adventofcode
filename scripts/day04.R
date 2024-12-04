# Dec 4 -------------------------------------------------------------------

# https://adventofcode.com/2024/day/3

## prelims ----

rm(list=ls(all=TRUE))
library(tidyverse)

puzzle <- read_fwf('day04/input_04.txt', col_positions = fwf_widths(rep(1, 140)))

## part 1 ----

row_words <- vector()

row_words <- sum(apply(puzzle, 1, function(x) {
  y <- paste(x, collapse = "")
  z <- str_count(y, "(XMAS|SAMX)")
  
}))

x <- puzzle

find_xmas <- function(x) {
  
  # rowwise:
  row_words <- sum(apply(x, 1, function(y) {
    z <- paste(y, collapse = "")
    z <- str_count(z, "(XMAS|SAMX)")
  }))
  
  # colwise
  col_words <- sum(apply(x, 2, function(y) {
    z <- paste(y, collapse = "")
    z <- str_count(z, "(XMAS|SAMX)")
  }))
  
  # forwards diagonal: take every value in row one and create a diagonal string:
  for (i in length(x[1,])) {
    iterant <- 0 + i
    paste(x[1,c(i:i+1)], collapse = "")
    print(z)
  }

  
}
