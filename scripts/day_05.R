# Dec 5 -------------------------------------------------------------------

# https://adventofcode.com/2024/day/5

## prelims ----

rm(list=ls(all=TRUE))
library(tidyverse)

# load input file
input <- read.table('input/input_05.txt', sep = '', header = F)

# split input into rules and updates:
rules <- input |> filter(grepl('\\|', V1)) |> 
  separate_wider_delim(V1, delim = '|', names = c('V1', 'V2'))
updates <- input |> filter(grepl(',', V1))

## part 1 ----

part1 <- vector()
for (update in 1:nrow(updates)) {
  
  # split update pages
  pages <- unlist(str_split(updates[update,], ','))
  # make all combinations:
  combos <- as_tibble(t(combn(pages, 2)))
  # check which of the combinations are **not** in rules:
  diffs <- combos |> anti_join(rules, join_by(V1, V2))
  
  # if there are no differences to the ordering rules, it's a correct rule:
  if (nrow(diffs) == 0) {
    part1[update] <- pages[ceiling(length(pages)/2)]
  } else {
    part1[update] <- NA
  }
  
}
sum(as.numeric(part1), na.rm = TRUE)

## part 2 ----

# part 1 and two can be done in one go:
part1 <- vector()
part2 <- vector()

for (update in 1:nrow(updates)) {
  
  # as above
  pages <- unlist(str_split(updates[update,], ','))
  combos <- as_tibble(t(combn(pages, 2)))
  diffs <- combos |> anti_join(rules, join_by(V1, V2))
  
  # if there are no differences to the ordering rules, it's a correct rule:
  if (nrow(diffs) == 0) {
    
    # if no differences, write to update for part 1:
    part1[update] <- pages[ceiling(length(pages)/2)]
    
  } else {
    
    # while differences remain, update and re-check until all diffs done:
    while (nrow(diffs) != 0) {
      
      diffs_update <- diffs[1,]
      swap1 <- which(pages == diffs_update$V1)
      swap2 <- which(pages == diffs_update$V2)
      pages <- replace(pages, c(swap1, swap2), pages[c(swap2, swap1)])
      diffs <- as_tibble(t(combn(pages, 2))) |> anti_join(rules, join_by(V1, V2))
    }

    part2[update] <- pages[ceiling(length(pages)/2)]
  }
  
}

sum(as.numeric(part1), na.rm = TRUE)
sum(as.numeric(part2), na.rm = TRUE)
