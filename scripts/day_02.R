# Dec 2 -------------------------------------------------------------------

# https://adventofcode.com/2024/day/2

## prelims ----

rm(list=ls(all=TRUE))
library(tidyverse)

reports <- read.csv('day02/input_02.txt', sep = '', header = F) |> 
  as_tibble()

## part 1 ----
# identify in-/decreasing reports; safe if each in-/decrease less than 4:

part1 <- reports
part1$safe <- apply(reports, 1, function(x) {
  y <- x[!is.na(x)]
  sorted <- (!is.unsorted(y, strictly = TRUE) | !is.unsorted(rev(y), strictly = TRUE))
  safe <- all(abs(diff(y)) < 4)
  (sorted & safe)
})

table(part1$safe)

## part 2 ----
# do vectors become 'safe' if (any) one element is removed?

part2 <- reports
part2$damped <- apply(reports, 1, function(x) {
  safe <- vector()
  sorted <- NULL
  for (i in 1:length(x)) {
    y <- x[-i]
    y <- y[!is.na(y)]
    sorted <- (!is.unsorted(y, strictly = TRUE) | !is.unsorted(rev(y), strictly = TRUE))
    if (sorted) {
      safe[i] <- all(abs(diff(y)) < 4)
    } else {
      safe[i] <- FALSE
    }
  }
  any(safe)
})

table(part2$damped)
