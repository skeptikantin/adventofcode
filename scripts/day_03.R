# Dec 3 -------------------------------------------------------------------

# https://adventofcode.com/2024/day/3

## prelims ----

rm(list=ls(all=TRUE))
library(tidyverse)

input <- read_lines('day03/input_03.txt') |> 
  # input contains 4 lines -> collapse:
  paste0(collapse = '')

## part 1 ----
# identify all mul(x,y) vectors, multiply (x,y) and sum:

## old-school R:
mults <- unlist(str_extract_all(input, "mul\\([0-9]+,[0-9]+\\)"))
res <- vector()
sum(sapply(mults, function(x) {
  y <- gsub('mul\\(|\\)', '', x)
  y <- str_split(y, ',')
  y <- lapply(y, as.numeric)
  y <- sapply(y, function(x) {x[1] * x[2]})
}))

## tidy R:
unlist(str_extract_all(input, 'mul\\([0-9]+,[0-9]+\\)')) |> 
  as_tibble() |> 
  mutate(value = gsub('^mul\\((\\d+?,\\d+)\\).*$', '\\1', value)) |> 
  separate_wider_delim(value, delim = ',', names = c('mult1', 'mult2')) |> 
  mutate_at(vars(mult1, mult2), as.numeric) |> 
  mutate(res = mult1 * mult2) |> 
  summarize(res = sum(res))

## part 2 ----
# if mul(x,y) preceded by don't(), multiplication is disabled
# until a do() call

## old-school R:
mults <- unlist(str_extract_all(input, "(do(n't)?\\(\\)|mul\\([0-9]+,[0-9]+\\))"))
keep <- vector()
enable = TRUE
for (i in mults) {
  if (grepl("don't", i)) {
    enable = FALSE
  } else if (grepl("do\\(", i)) {
    enable = TRUE
  }
  if (enable & grepl('mul', i)) {
    keep = c(keep, i)
  }
}

enabled <- lapply(str_split(gsub("mul\\(|\\)", "", keep), ","), as.numeric)
sum(sapply(enabled, function(x) {x[1] * x[2]}))

## tidy R:
unlist(str_extract_all(input, "((don't|do)\\(\\))?(.*?)?mul\\([0-9]+,[0-9]+\\)")) |> 
  as_tibble() |>
  mutate(enabler = value,
         mults = gsub('^.+?(mul\\([0-9]+,[0-9]+\\))$', '\\1', value)) |> 
  mutate(enabler = case_when(grepl('don\'t', enabler) ~ 0, grepl('do[^n]', enabler) ~ 1, .default = NA),
         mults = gsub('mul\\(|\\)', '', mults)) |> 
  fill(enabler, .direction = 'down') |> 
  filter(enabler %in% c(1, NA)) |> 
  separate_wider_delim(mults, delim = ',', names = c('mult1', 'mult2')) |> 
  mutate_at(vars(mult1, mult2), as.numeric) |> 
  mutate(res = mult1 * mult2) |> 
  summarize(res = sum(res))
