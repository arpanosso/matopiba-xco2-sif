
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}


r2findWLS <-function(fit, vario){
  SSErr<-attr(fit,"SSErr")
  weig<-vario$np/vario$dist^2
  SStot<- SStot <- sum((weig*(vario$gamma-mean(vario$gamma)))^2)
  R2<-1-SSErr/SStot
  return(R2)
}

my_geo_stat <- function(df = dados_geo,
                        modelo = "Sph",
                        dia = "2014-09-01",
                        variavel="XCO2"){

  # Preparo do banco de dados
  data_especifica <- dia
  df_aux <- df %>% filter(mes_ano == data_especifica) %>%
    mutate(x = longitude, y=latitude) %>%
    select("x", "y", variavel)
  names(df_aux) <- c("x","y","z")
  df_aux <- df_aux %>%
    group_by(x,y) %>%
    summarise(z = mean(z))
  coordinates(df_aux)= ~ x+y
  form<-z~1

  # Criando o variograma experimental
  vario <- variogram(form, data=df_aux, cutoff=20, width=1.5,cressie=FALSE)
  vario_exp <- vario  %>%
    ggplot(aes(x=dist, y=gamma)) +
    geom_point()+
    labs(title = paste0(variavel,"-",dia) )+
    theme_bw()
  ggsave(paste0("img/variograma_experimental/",variavel,"_",dia,".png"),vario_exp)

  # modelando o semivariograma
  m_vario <- fit.variogram(vario,fit.method = 7,
                           vgm(1, modelo, 10, 0)
  )
  sqr.f1<-round(attr(m_vario, "SSErr"),4)
  c0<-round(m_vario$psill[[1]],4)
  c0_c1<-round(sum(m_vario$psill),4)
  a<-round(m_vario$range[[2]],2)
  r2<-round(r2findWLS(m_vario,vario),8)
  texto_ajuste <- paste(modelo,"(C0= ",c0,"; C0+C1= ", c0_c1, "; a= ", a,"; SQR = ", sqr.f1,"; R² = ",r2,")",sep="")
  preds = gstat::variogramLine(m_vario, maxdist = max(vario$dist))
  semivar <- vario %>%
    ggplot(aes(dist, gamma)) +
    geom_point() +
    geom_line(data = preds) +
    theme_bw() +
    labs(x="Distância de separação", y="Semivariância",
         title=paste0(variavel,"-",dia),
         subtitle = texto_ajuste)+
    coord_cartesian(ylim = c(0,max(vario$gamma)))
  ggsave(paste0("img/variograma/",variavel,"_",dia,"_",modelo,".png"),semivar)

  # Krigagem ordinária
  ko_var<-krige(formula=form, df_aux, grid, model=m_vario,
                block=c(0,0),
                nsim=0,
                na.action=na.pass,
                debug.level=-1,
  )
  krigagem <- tibble::as.tibble(ko_var) %>%
    dplyr::mutate(flag = def_pol(X,Y,pol_ma) | def_pol(X,Y,pol_to) | def_pol(X,Y,pol_pi) | def_pol(X,Y,pol_ba)
    ) %>%
    dplyr::filter(flag) %>%
    ggplot(aes(x=X, y=Y),color="black") +
    geom_tile(aes(fill = var1.pred)) +
    scale_fill_gradient(low = "yellow", high = "blue") +
    coord_equal()+
    tema_mapa()+
    ggplot2::labs(fill=variavel,title = data_especifica) +
    ggspatial::annotation_scale(
      location="bl",
      plot_unit="km",
      height = ggplot2::unit(0.2,"cm"))
  ggsave(paste0("img/krig/",variavel,"_",dia,"_",modelo,".png"),krigagem)
}
