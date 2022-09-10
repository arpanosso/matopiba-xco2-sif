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


library(sf)
matopiba_shp <- st_read(
  "data-raw//matopiba_aptidao_20201110_v2//matopiba_aptidao_20201110_v2.shp")
st_geometry_type(matopiba_shp)
st_crs(matopiba_shp)
st_bbox(matopiba_shp)


modelos_amp <- list.files("img\\validacao_cruzada") %>% stringr::str_split(pattern = "_",
                                                        simplify = TRUE)

modelos_amp <- as.data.frame(modelos_amp[,3:4])
modelos_amp <- modelos_amp %>%
  mutate(
    V2 = str_remove(V2,".png")
  )

writexl::write_xlsx(modelos_amp,"data/modelos_amp.xlsx")



