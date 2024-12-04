# Dec 1 -------------------------------------------------------------------

# https://adventofcode.com/2024/day/1

## prelims ----

rm(list=ls(all=TRUE))
library(tidyverse)

# read whitespace-delimited file and split to ls so each list can
# be sorted individually:
loc_ids <- read.table('day01/input_01.txt',
                      sep = '', header = F, col.names = c('id1', 'id2')) |> 
  as.list(spread(id1, id2)) |> 
  purrr::map(sort) |>
  bind_cols()

## part 1 ----
# find difference between each list pair

loc_ids |> 
  mutate(diff = abs(id1 - id2)) |> 
  summarize(sum(diff))

## part 2 ----
# add penalty to each id in col1 (mult by number of times it occurs in col2)

# fetch simscores to sum later:
sim_score <- vector()
for (i in 1:nrow(loc_ids)) {
  penalty = sum(loc_ids$id1[i] == loc_ids$id2)
  sim_score[i] <- penalty * loc_ids$id1[i]
}

sum(sim_score)

