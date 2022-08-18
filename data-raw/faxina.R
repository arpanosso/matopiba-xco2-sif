# Carregando os dados
library(tidyverse)
uso_solo <- read.table("data-raw/land_use.txt",
           sep=",",
           header = TRUE) %>%
select(-ID)

write_rds(uso_solo,"data/land_use.rds")
