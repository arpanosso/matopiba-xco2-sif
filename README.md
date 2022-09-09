
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Pacotes exigidos

``` r
library(patchwork)
library(tidyverse)
library(writexl)
library(ggpubr)
library(gstat)
library(geobr)
library(sp)
source("r/graficos.R")
source("r/funcoes.R")
```

## Correção do sinal de SIF

sif_757: 2.6250912\*10^-19

sif_771: 2.57743\*10^-19

``` r
lst_dn<- read_rds("data/lst_dn.rds")
uso_solo <- read_rds("data/land_use2.rds")
oco2_br <- read_rds("data/oco2_br.rds") %>% 
  mutate(
    sif_757 = fluorescence_radiance_757nm_idp_ph_sec_1_m_2_sr_1_um_1*2.6250912*10^(-19),
    sif_771 = fluorescence_radiance_771nm_idp_ph_sec_1_m_2_sr_1_um_1* 2.57743*10^(-19),
    SIF = (sif_757 + 1.5*sif_771)/2
  )
oco2_br %>% glimpse()
#> Rows: 37,387
#> Columns: 35
#> $ longitude                                                     <dbl> -70.5, -~
#> $ longitude_bnds                                                <chr> "-71.0:-~
#> $ latitude                                                      <dbl> -5.5, -4~
#> $ latitude_bnds                                                 <chr> "-6.0:-5~
#> $ time_yyyymmddhhmmss                                           <dbl> 2.014091~
#> $ time_bnds_yyyymmddhhmmss                                      <chr> "2014090~
#> $ altitude_km                                                   <dbl> 3307.8, ~
#> $ alt_bnds_km                                                   <chr> "0.0:661~
#> $ fluorescence_radiance_757nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 7.272876~
#> $ fluorescence_radiance_757nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 2.537127~
#> $ xco2_moles_mole_1                                             <dbl> 0.000394~
#> $ aerosol_total_aod                                             <dbl> 0.148579~
#> $ fluorescence_offset_relative_771nm_idp                        <dbl> 0.016753~
#> $ fluorescence_at_reference_ph_sec_1_m_2_sr_1_um_1              <dbl> 2.615319~
#> $ fluorescence_radiance_771nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 3.088582~
#> $ fluorescence_offset_relative_757nm_idp                        <dbl> 0.013969~
#> $ fluorescence_radiance_771nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 5.577878~
#> $ xco2                                                          <dbl> 394.3686~
#> $ data                                                          <dttm> 2014-09~
#> $ ano                                                           <dbl> 2014, 20~
#> $ mes                                                           <dbl> 9, 9, 9,~
#> $ dia                                                           <int> 6, 6, 6,~
#> $ dia_semana                                                    <dbl> 7, 7, 7,~
#> $ x                                                             <int> 7, 8, 11~
#> $ xco2_est                                                      <dbl> 392.7080~
#> $ delta                                                         <dbl> -1.66062~
#> $ XCO2                                                          <dbl> 387.2781~
#> $ flag_norte                                                    <lgl> TRUE, TR~
#> $ flag_nordeste                                                 <lgl> FALSE, F~
#> $ flag_sul                                                      <lgl> FALSE, F~
#> $ flag_sudeste                                                  <lgl> FALSE, F~
#> $ flag_centroeste                                               <lgl> FALSE, F~
#> $ sif_757                                                       <dbl> 0.666019~
#> $ sif_771                                                       <dbl> 0.796060~
#> $ SIF                                                           <dbl> 0.930054~
```

## Definição da região de trabalho

``` r
estados <- read_state(showProgress = FALSE) # carregando estados
matopiba_filtro <- estados$abbrev_state %in% c("MA","PI","TO","BA") # filtrando estados
matopiba <- estados$geom[matopiba_filtro] ## estados do matopiba

microregiao <- read_micro_region(showProgress = FALSE) # buscando as microregioes
micro_nomes <- read.table("data-raw/microregiao_nomes.txt",h=TRUE,sep="\t") ## dados retirados da wikipedia
microregiao_filtro <- microregiao$name_micro %in% micro_nomes$Microrregião ## filtro por microregiao

poli_micro <- read.table("data-raw/digit.dat",sep=",") %>% as.matrix() ## lendo o polígono editado manualmente pelo surfer
colnames(poli_micro) <- c("X","Y") ## renomaenado colunas

matopiba_micro <- microregiao$geom[microregiao_filtro]
matopiba_micro <- matopiba_micro[-15]
matopiba %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="white", color="black",
          size=.15, show.legend = FALSE) +
  tema_mapa()+
  geom_sf(data=matopiba_micro,fill="gray")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Dados de xCO2 e SIF

Inicialmente vamos extrair os polígonos com os limites dos estados do
objegio gerado pelo `{geobr}`.

``` r
pol_to <- estados$geom %>% purrr::pluck(7) %>% as.matrix()
pol_ma <- estados$geom %>% purrr::pluck(8) %>% as.matrix()
pol_ba <- estados$geom %>% purrr::pluck(16) %>% as.matrix()
pol_pi <- estados$geom %>% purrr::pluck(9) %>% as.matrix()
```

Utilizando a função `def_pol` para classificar se o ponto pertence, ou
não a um dos estados.

``` r
data_set <- oco2_br %>% 
  mutate(
    flag_to = def_pol(longitude, latitude, pol_to),
    flag_ma = def_pol(longitude, latitude, pol_ma),
    flag_ba = def_pol(longitude, latitude, pol_ba),
    flag_pi = def_pol(longitude, latitude, pol_pi),
    flag_matopiba = def_pol(longitude, latitude, poli_micro)
  )
```

## Mapeamento

Plot dos pontos do satélite e da região do matopiba.

``` r
matopiba %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  tema_mapa() +
  ggplot2::geom_point(data=data_set  %>%  dplyr::filter(flag_to |
                                                          flag_ma |
                                                          flag_pi |
                                                          flag_ba |
                                                          flag_matopiba, ano == 2014) ,
                      ggplot2::aes(x=longitude,y=latitude),
                      shape=3,
                      col="red",
                      alpha=0.2)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Tabela de médias de FCO2

``` r
tab_oco2_sif_media <- data_set  %>%  filter(SIF >= 0) %>% 
    tidyr::pivot_longer(
    dplyr::starts_with("flag"),
    names_to = "região",
    values_to = "flag"
  ) %>% 
  dplyr::filter(flag)  %>%  
  dplyr::mutate(região = stringr::str_remove(região,"flag_"))  %>% 
  dplyr::filter(região %in% c("ba","pi","to","ma")) %>% 
  dplyr::group_by(região, ano, mes, longitude, latitude) %>%  
  dplyr::summarise(media_sif = mean(SIF, na.rm=TRUE),
                   media_xco2 = mean(XCO2, na.rm=TRUE),
                   #latitude = mean(latitude, na.rm=TRUE),
                   #longitude = mean(longitude, na.rm=TRUE)
                   ) %>% 
    dplyr::mutate(
    mes_ano = lubridate::make_date(ano, mes, 1)
  )
write_xlsx(tab_oco2_sif_media, "data/medias_oco2_sif.xlsx")
```

## Faça o download da tabela de médias

[medias_oco2_sif.xlsx](https://github.com/arpanosso/matopiba-xco2-sif/raw/master/data/medias_oco2_sif.xlsx)

## Juntando os diferentes usos do solo para a região

``` r
uso_solo_uni <-uso_solo %>% 
  pivot_longer(LU_15:LU_19,names_to = "ano") %>% 
  arrange(ano) %>% 
  mutate(ano = as.numeric(str_remove(ano,"LU_"))+2000) %>% 
  rename(longitude = lon,latitude = lat)
uso_solo_uni %>% 
  ggplot(aes(longitude, latitude, color=value)) +
  geom_point() +
  facet_wrap(~ano)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
matopiba %>% 
  ggplot() +
  geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  tema_mapa() +
  geom_point(data=uso_solo_uni,
                      ggplot2::aes(x=longitude,y=latitude,color=value)) +
    geom_polygon(data=poli_micro %>% as.tibble(),
               aes(x=X,y=Y),color="red", fill="lightblue", alpha=.0,
               size=1)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
  facet_wrap(~ano)
#> <ggproto object: Class FacetWrap, Facet, gg>
#>     compute_layout: function
#>     draw_back: function
#>     draw_front: function
#>     draw_labels: function
#>     draw_panels: function
#>     finish_data: function
#>     init_scales: function
#>     map_data: function
#>     params: list
#>     setup_data: function
#>     setup_params: function
#>     shrink: TRUE
#>     train_scales: function
#>     vars: function
#>     super:  <ggproto object: Class FacetWrap, Facet, gg>
```

## Juntando as duas bases de dados

``` r
tab_oco2_sif_uso <- tab_oco2_sif_media %>%
  group_by(longitude, latitude, ano) %>% 
  summarise(
    media_sif = mean(media_sif),
    media_xco2 = mean(media_xco2)
  ) %>% 
  left_join(uso_solo_uni,c("longitude","latitude","ano")) %>% 
  drop_na()

tab_oco2_sif_media <- tab_oco2_sif_media %>%
  left_join(uso_solo_uni,c("longitude","latitude","ano")) %>% 
  drop_na()

names(lst_dn) <- c("longitude","latitude","ano","mes","LST_d","LST_n" )

tab_oco2_sif_media <- left_join(tab_oco2_sif_media, lst_dn,
          c("longitude","latitude","ano","mes"))

tab_oco2_sif_media <- tab_oco2_sif_media %>% 
  mutate(
    Amp_T = LST_d - LST_n
  )
  
write_xlsx(tab_oco2_sif_media, "data/medias_oco2_sif_uso.xlsx")
```

## Faça o download da tabela de médias e usos do solo de 2015 a 2019

[medias_oco2_sif.xlsx](https://github.com/arpanosso/matopiba-xco2-sif/raw/master/data/medias_oco2_sif_uso.xlsx)

# RESULTADOS - variabilidade temporal

``` r
tab_oco2_sif_media  %>%  
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  group_by(value, ano, mes) %>%  
  mutate(
    media_xco2 = mean(media_xco2)
  ) %>% 
  ggplot(aes(x = mes_ano, y = media_xco2,
                               color=value)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
tab_oco2_sif_media  %>%  
    mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  group_by(value, ano, mes) %>%  
  mutate(
    media_sif = mean(media_sif)
  ) %>% 
  ggplot(aes(x = mes_ano, y = media_sif,
                               color=value)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
tab_oco2_sif_media  %>%  
    mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  group_by(value, ano, mes) %>%  
  mutate(
    media_Amp_T = mean(Amp_T, na.rm=TRUE)
  ) %>% 
  ggplot(aes(x = mes_ano, y = media_Amp_T,
                               color=value)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
tab_oco2_sif_media  %>%  
    mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  group_by(value, ano, mes) %>%  
  mutate(
    media_LST_d = mean(LST_d, na.rm=TRUE)
  ) %>% 
  ggplot(aes(x = mes_ano, y = media_LST_d,
                               color=value)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
tab_oco2_sif_media  %>%  
    mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  group_by(value, ano, mes) %>%  
  mutate(
    media_LST_n = mean(LST_n,na.rm=TRUE)
  ) %>% 
  ggplot(aes(x = mes_ano, y = media_LST_n,
                               color=value)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Correlação

``` r
land_uses <- tab_oco2_sif_media %>% pull(value) %>%  unique()

for(i in seq_along(land_uses)){
  print(land_uses[i])
  mc <- tab_oco2_sif_media %>% ungroup() %>% 
      mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
    filter(value==land_uses[i]) %>% 
    select(media_sif, media_xco2, Amp_T, LST_d, LST_n) %>% 
    drop_na() %>% 
    cor()
  corrplot::corrplot.mixed(mc,lower = "number",lower.col = "black")
}
#> [1] "Agriculture"
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

    #> [1] "Herbaceus Veg."

![](README_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

    #> [1] "Shrubs"

![](README_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

    #> [1] "Forest"

![](README_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

``` r
sc_sif <- tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  summarise(media_sif = mean(media_sif, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_sif <= 2.5, media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  ggscatter(
    x = "media_sif", y = "media_xco2",
    add = "reg.line"
  ) + # coord_cartesian(ylim = c(382.5,392))+
  stat_cor(label.y = 395, label.x = .5) + 
  stat_regline_equation(label.y = 396.2, label.x = .5)+
  labs()

sc_amp <- tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>% 
  summarise(media_Amp_T = mean(Amp_T, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_xco2 <= 392.5 & media_xco2>=377.5) %>%
  ggscatter(
    x = "media_Amp_T", y = "media_xco2",
    add = "reg.line", color="red"
  ) + # coord_cartesian(ylim = c(382.5,392))+
  stat_cor(label.y = 395, label.x = 2) + 
  stat_regline_equation(label.y = 396.2, label.x = 2) +
  labs(y="")

sc_sif | sc_amp
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  group_by(value, ano, mes) %>%  
  summarise(media_sif = mean(media_sif, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_sif <= 2.5, media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  ggscatter(
    x = "media_sif", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  group_by(value, ano, mes) %>%  
  summarise(media_Amp_T = mean(Amp_T, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  ggscatter(
    x = "media_Amp_T", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Para período de seca

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  mutate(season = ifelse(mes >= 5 & mes <= 10, "dry","wet")) %>% 
  group_by(season, value, ano, mes) %>%  
  dplyr::summarise(media_sif = mean(media_sif, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_sif <= 2.5, media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  filter(season == "dry") %>% 
  ggscatter(
    x = "media_sif", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)+
  labs(color = "Dry: value")
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  mutate(season = ifelse(mes >= 5 & mes <= 10, "dry","wet")) %>% 
  group_by(season, value, ano, mes) %>%  
  dplyr::summarise(media_Amp_T = mean(Amp_T, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  filter(season == "dry") %>% 
  ggscatter(
    x = "media_Amp_T", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)+
  labs(color = "Dry: value")
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Para período úmido

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  mutate(season = ifelse(mes >= 5 & mes <= 10, "dry","wet")) %>% 
  group_by(season, value, ano, mes) %>%  
  dplyr::summarise(media_sif = mean(media_sif, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% 
  filter(season == "wet") %>% 
  ggscatter(
    x = "media_sif", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392)) +
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2) +
  labs(color = "Wet: value")
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
tab_oco2_sif_media %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  mutate(season = ifelse(mes >= 5 & mes <= 10, "dry","wet")) %>% 
  group_by(season, value, ano, mes) %>%  
  dplyr::summarise(media_Amp_T = mean(Amp_T, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% filter(media_xco2 <= 392.5 & media_xco2>=377.5) %>% 
  filter(season == "wet") %>% 
  ggscatter(
    x = "media_Amp_T", y = "media_xco2",
    color = "value", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~value) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)+
  labs(color = "Wet: value")
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- --> \##
Motivação, quais pontos apresentaram alteração do uso do solo?

``` r
tab_oco2_sif_uso <- tab_oco2_sif_uso %>% ungroup()
tab_oco2_sif_uso %>% 
  group_by(longitude, latitude, value) %>% 
  summarise(
   n = n()
  ) %>% 
  filter(n<5) %>% 
  ggplot(aes(x=longitude, y=latitude)) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Mapear os dados acima

``` r
matopiba %>% 
  ggplot() +
  geom_sf(fill="white", color="black",
          size=.15, show.legend = FALSE)+
  tema_mapa() +
  geom_point(data=tab_oco2_sif_uso %>% 
               group_by(longitude, latitude, value) %>% 
               summarise(
                 n = n()
               ) %>% 
               filter(n<5),
             aes(x=longitude,y=latitude),color="red")
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Ideal é identificar no banco de dados quais são esses pontos, por meio
da latitude e longitude

``` r
mudanca <- tab_oco2_sif_uso %>% 
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  group_by(longitude, latitude, value) %>% 
  summarise(
   n = n()
  ) %>% 
  filter(n<5) %>% 
  count()
tab_oco2_sif_media <- tab_oco2_sif_media %>% 
  mutate(
    mudança = 
      longitude %in% mudanca$longitude &
      latitude %in% mudanca$latitude
  )
#write_xlsx(tab_oco2_sif_media, "data/medias_oco2_sif_uso.xlsx")

tab_oco2_sif_media  %>% ungroup() %>%
  mutate(
        flag_matopiba = def_pol(longitude, latitude, poli_micro)
  ) %>% 
  filter(flag_matopiba) %>%
  mutate(season = ifelse(mes >= 5 & mes <= 10, "dry","wet")) %>% 
  group_by(mudança, season, ano, mes) %>%  
  dplyr::summarise(media_sif = mean(media_sif, na.rm=TRUE),
                   media_xco2 = mean(media_xco2, na.rm=TRUE)
  ) %>% 
  ggscatter(
    x = "media_sif", y = "media_xco2",
    color = "mudança", palette = "jco",
    add = "reg.line"
  ) + coord_cartesian(ylim = c(382.5,392))+
  facet_wrap(~mudança + season) +
  stat_cor(label.y = 390) + 
  stat_regline_equation(label.y = 391.2)
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

# RESULTADOS - Variabilidade espacial

Inicialmente, devemos criar o banco de dados com as amostras para a
geoestatística espaço-temporal.

``` r
dados_geo <- data_set  %>%  filter(SIF >= 0) %>% 
    tidyr::pivot_longer(
    dplyr::starts_with("flag"),
    names_to = "região",
    values_to = "flag"
  ) %>% 
  dplyr::filter(flag)  %>%  
  dplyr::mutate(região = stringr::str_remove(região,"flag_"))  %>% 
  dplyr::filter(região %in% c("ba","pi","to","ma")) %>% 
  mutate(
    mes_ano = lubridate::make_date(ano, mes, 1)
  ) %>% 
  select(longitude, latitude, mes_ano, XCO2, SIF)

dados_geo <- dados_geo %>% 
  left_join(tab_oco2_sif_media,c("longitude","latitude","mes_ano")) %>% 
  drop_na()

dados_geo %>% glimpse()
#> Rows: 6,433
#> Columns: 15
#> $ longitude  <dbl> -50.5, -50.5, -50.5, -48.5, -48.5, -47.5, -47.5, -47.5, -47~
#> $ latitude   <dbl> -12.5, -11.5, -10.5, -9.5, -8.5, -12.5, -11.5, -10.5, -9.5,~
#> $ mes_ano    <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01~
#> $ XCO2       <dbl> 382.4614, 382.7462, 382.8724, 384.4948, 383.7093, 384.5950,~
#> $ SIF        <dbl> 1.0275361, 0.8187736, 0.9326621, 0.8214164, 0.9543531, 1.17~
#> $ região     <chr> "to", "to", "to", "to", "to", "to", "to", "to", "to", "pi",~
#> $ ano        <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015,~
#> $ mes        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
#> $ media_sif  <dbl> 1.0275361, 0.8187736, 0.9326621, 0.8214164, 0.9543531, 1.17~
#> $ media_xco2 <dbl> 382.4614, 382.7462, 382.8724, 384.4948, 383.7093, 384.5950,~
#> $ value      <chr> "Herbaceus Veg.", "Forest", "Herbaceus Veg.", "Forest", "Fo~
#> $ LST_d      <dbl> 31.80200, 33.17500, 28.26221, 31.45272, 31.06999, 31.91999,~
#> $ LST_n      <dbl> 24.72000, 24.54999, 24.92667, 24.46666, 23.55500, 23.98666,~
#> $ Amp_T      <dbl> 7.081995, 8.625008, 3.335548, 6.986053, 7.514987, 7.933323,~
#> $ mudança    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE,~
```

Criando o grid de refinamento para a plotagem de pontos em locais não
amostrados

``` r
minX_pol <- min(pol_ma[,1],pol_to[,1],pol_pi[,1],pol_ba[,1])
maxX_pol <- max(pol_ma[,1],pol_to[,1],pol_pi[,1],pol_ba[,1])
minY_pol <- min(pol_ma[,2],pol_to[,2],pol_pi[,2],pol_ba[,2])
maxY_pol <- max(pol_ma[,2],pol_to[,2],pol_pi[,2],pol_ba[,2])
#x<-df_aux$x
#y<-df_aux$y
dis <- .1 #Distância entre pontos
grid <- expand.grid(X=seq(min(minX_pol),max(maxX_pol),dis), Y=seq(min(minY_pol),max(maxY_pol),dis))
gridded(grid) = ~ X + Y
plot(grid)
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Vamos filtrar para uma data específica e criar

``` r
lista_datas <- dados_geo$mes_ano %>% unique()
data_especifica <- "2015-03-01"
df_aux <- dados_geo %>% filter(mes_ano == data_especifica) %>% 
  mutate(x = longitude, y=latitude) %>% 
  select(x, y, LST_d) %>% 
  group_by(x,y) %>% 
  summarise(LST_d = mean(LST_d))
coordinates(df_aux)= ~ x+y
# form<-XCO2~1
form<-LST_d~1
```

Verificando o Variograma experimental

``` r
vario <- variogram(form, data=df_aux, cutoff=20, width=1.5,cressie=FALSE)
# vario  %>%
#   ggplot(aes(x=dist, y=gamma)) +
#   geom_point()
m_vario <- fit.variogram(vario,
                         fit.method = 7,
                         vgm(1, "Sph", 10, 0))

## validação Cruzada
m <- vgm(1, "Sph", 10, 0)
df_aux_g <- gstat(id=as.character(form)[2], formula = LST_d~1, data=df_aux)
df_aux_g <- gstat(df_aux_g, model =  m, fill.all = TRUE)
x <- variogram(df_aux_g, cutoff = 20)
df_fit = fit.lmc(x, df_aux_g)
out = gstat.cv(df_fit, nmax = 40, verbose = FALSE) 
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
#> [using ordinary kriging]
out %>% as.tibble() %>% 
  ggplot(aes(x=observed,LST_d.pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
  label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
sqr.f1<-round(attr(m_vario, "SSErr"),4); c0<-round(m_vario$psill[[1]],4); c0_c1<-round(sum(m_vario$psill),4);a<-round(m_vario$range[[2]],2)
r2<-round(r2findWLS(m_vario,vario),8)
texto_ajuste <- paste("Esf(C0= ",c0,"; C0+C1= ", c0_c1, "; a= ", a,"; SQR = ", sqr.f1,"; R² = ",r2,")",sep="")
preds = gstat::variogramLine(m_vario, maxdist = max(vario$dist))
semivar <- vario %>% 
  ggplot(aes(dist, gamma)) +
  geom_point() +
  geom_line(data = preds) + 
  theme_bw() +
  labs(x="Distância de separação", y="Semivariância",
       title=data_especifica,
       subtitle = texto_ajuste)+
  coord_cartesian(ylim = c(0,max(vario$gamma)))
ggsave(paste0("img/variograma/",data_especifica,"_modelo.png"),semivar)
```

### Krigragem ordinária (KO)

Utilizando o algoritmo de KO, vamos estimar xco2/sif nos locais não
amostrados.

``` r
ko_var<-krige(formula=form, df_aux, grid, model=m_vario, 
    block=c(0,0),
    nsim=0,
    na.action=na.pass,
    debug.level=-1,  
    )
#> [using ordinary kriging]
#>  67% done100% done
```

Mapa de padrões espaciais.

``` r
krigagem <- tibble::as.tibble(ko_var) %>%  
  dplyr::mutate(flag = def_pol(X,Y,pol_ma) | def_pol(X,Y,pol_to) | def_pol(X,Y,pol_pi) | def_pol(X,Y,pol_ba)
                ) %>% 
  dplyr::filter(flag) %>% 
  ggplot(aes(x=X, y=Y),color="black") + 
  geom_tile(aes(fill = var1.pred)) +
  scale_fill_gradient(low = "yellow", high = "blue") + 
  coord_equal()+
  tema_mapa()+
  ggplot2::labs(fill="xco2 (ppm)",title = data_especifica) +
  ggspatial::annotation_scale(
    location="bl",
    plot_unit="km",
    height = ggplot2::unit(0.2,"cm"))
ggsave(paste0("img/krig/",data_especifica,"_modelo.png"),krigagem)
print(krigagem)
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

[Yamamoto,
2007](https://link.springer.com/article/10.1007/s10596-007-9046-x)

[Journel,
1978](https://trove.nla.gov.au/work/23680388?q&versionId=44967529)

![y = k_0 \cdot exp \left\[ ln(\hat{y}\_{OK})+\frac{\sigma^2\_{OK}}{2} \right\]](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%20%3D%20k_0%20%5Ccdot%20exp%20%5Cleft%5B%20ln%28%5Chat%7By%7D_%7BOK%7D%29%2B%5Cfrac%7B%5Csigma%5E2_%7BOK%7D%7D%7B2%7D%20%5Cright%5D "y = k_0 \cdot exp \left[ ln(\hat{y}_{OK})+\frac{\sigma^2_{OK}}{2} \right]")

De acordo com Noel Cressie, preditor não viciado de
![Z](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Z "Z")
a qual sofreu transformação lognormal:

![\breve{p}\_Z(Z;S_0) = exp(\hat{p}\_Y(Z;S_0)+\sigma^2\_{Y}(S_0)/2-var(\hat{p}\_Y(Z;S_0))/2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbreve%7Bp%7D_Z%28Z%3BS_0%29%20%3D%20exp%28%5Chat%7Bp%7D_Y%28Z%3BS_0%29%2B%5Csigma%5E2_%7BY%7D%28S_0%29%2F2-var%28%5Chat%7Bp%7D_Y%28Z%3BS_0%29%29%2F2%29 "\breve{p}_Z(Z;S_0) = exp(\hat{p}_Y(Z;S_0)+\sigma^2_{Y}(S_0)/2-var(\hat{p}_Y(Z;S_0))/2)")

``` r
krigagem_bt <- tibble::as.tibble(ko_var) %>%  
  mutate(
    pz = exp(var1.pred),
    bt = exp(var1.pred + (var1.var/2)),
    bt_2 = bt*(mean(df_aux$XCO2)/mean(bt))
  ) %>% 
  dplyr::mutate(flag = def_pol(X,Y,pol_ma) | def_pol(X,Y,pol_to) | def_pol(X,Y,pol_pi) | def_pol(X,Y,pol_ba) | def_pol(X,Y,poli_micro)
                ) %>%
  dplyr::filter(flag) %>%
  ggplot(aes(x=X, y=Y),color="black") + 
  geom_tile(aes(fill = bt_2)) +
  scale_fill_gradient(low = "yellow", high = "blue") + 
  coord_equal()+
  tema_mapa()+
  ggplot2::labs(fill="xco2 (ppm)",title = data_especifica) +
  ggspatial::annotation_scale(
    location="bl",
    plot_unit="km",
    height = ggplot2::unit(0.2,"cm"))+
  geom_polygon(data=poli_micro %>% as.tibble(),
               aes(x=X,y=Y),color="red", fill="lightblue", alpha=.0,
               size=1)
print(krigagem_bt)
```

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

## Vamos criar um grid para testar todos os modelos

<https://gis.stackexchange.com/questions/237574/backtransformation-of-kriging-predictions-and-variances>

``` r
# mypar<-expand.grid(dia_ = lista_datas, 
#             modelo_ = c("Sph","Exp","Gau"),
#             variavel_ = c("LST_d", "LST_n"))
```

## Usando a `my_geo_stat` função para análise geoestatística

``` r
# my_geo_stat(df = dados_geo,
#                       modelo = "Gau",
#                         dia = "2015-01-01",
#                         variavel="LST_d")
# 
# for(i in 167:nrow(mypar)){
#   my_geo_stat(df = dados_geo,
#                         modelo = mypar$modelo_[i] %>% as.character(),
#                         dia = mypar$dia_[i] %>% as.character(),
#                         variavel=mypar$variavel_[i]%>% as.character()
#               )
#   print(paste0("---",i,"/",nrow(mypar),"----"))
# }
```

## Após a seleção de modelos, vamos fazer todos as figuras novamente.

``` r
modelos <- readxl::read_excel("data/modelos.xlsx")
# modelos <- modelos %>% 
#   mutate(
#     V1 = as.Date(V1)
#   )
# names(modelos) <- c("dia_","modelo_")
# modelos$variavel_ <- "LST_n"
# for(i in 1:nrow(modelos)){
#   my_geo_stat(df = dados_geo,
#                         modelo = modelos$modelo_[i] %>% as.character(),
#                         dia = modelos$dia_[i] %>% as.character(),
#                         variavel = modelos$variavel_[i] %>% as.character()
#               )
#   print(paste0("---",i,"/",nrow(modelos),"----"))
# }
```
