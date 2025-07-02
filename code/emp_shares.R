library(tidyverse)
library(epiextractr)

basic_data <- load_basic(2023, year, emp, wbhao, basicwgt, age)

universe <- basic_data |>
    filter(age >= 16, emp == 1) |>
    mutate(wbhao = haven::as_factor(wbhao))

universe |> 
    summarize(n = sum(basicwgt / 12), .by = wbhao) |>
    mutate(share = n/sum(n)) |>
    write_csv("./output/shares_wbhao.csv")

