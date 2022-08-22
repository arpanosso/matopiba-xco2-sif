# Carregando os dados
library(tidyverse)
uso_solo <- read.table("data-raw/land_use.txt",
           sep=",",
           header = TRUE) %>%
select(-ID)

write_rds(uso_solo,"data/land_use.rds")


uso_solo2 <- read.table("data-raw/land_use2.txt",
                       sep=";",
                       header = TRUE) %>%
  select(-ID)
write_rds(uso_solo2,"data/land_use2.rds")


lst_dn <- read.table("data-raw/lst_dn.txt",
                        sep=",",
                        header = TRUE) %>%
  select(-ID)
write_rds(lst_dn,"data/lst_dn.rds")
