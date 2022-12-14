## Limites do outliers
limites_outlier <- function(x, coefi=1.5){
  med = median(x)
  li = med - coefi*IQR(x)
  ls = med + coefi*IQR(x)
  c(Lim_Inferior = li, Lim_Superior = ls)
}

# Função para o cálculo do CV
meu_cv <- function(x){
  100*sd(x)/mean(x)
}

# Função para o cálculo do erro padrão da média
meu_erro_padrao <- function(x){
  sd(x)/sqrt(length(x))
}

# Função para a estatística descritiva
est_descritiva <- function(x){
  n <- length(x)
  n_na <- sum(is.na(x)) # <<<<<<<------------
  x<- na.omit(x)
  m <- mean(x)
  dp <- sd(x)
  md <- median(x) # quantile(x, 0.50)
  cv <- meu_cv(x)
  mini <- min(x)
  maxi <- max(x)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  s2 <- var(x)
  g1 <- agricolae::skewness(x)
  g2 <- agricolae::kurtosis(x)
  epm <- meu_erro_padrao(x)
  pSW <- shapiro.test(x)$p.value
  return(c(N = n,
           N_perdidos = n_na, # <<<<<<<<<--------
           Media = m,Mediana = md,
           Min = mini,Max = maxi,
           Var = s2,DP = dp,
           Q1 = q1,Q3 = q3,
           CV = cv,EPM = epm,
           G1 = g1,G2 = g2,
           Norm = pSW))
}

## definição de um ponto dentro de um polígono
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

## calculando o r2 do modelo de semivriograma
r2findWLS <-function(fit, vario){
  SSErr<-attr(fit,"SSErr")
  weig<-vario$np/vario$dist^2
  SStot<- SStot <- sum((weig*(vario$gamma-mean(vario$gamma)))^2)
  R2<-1-SSErr/SStot
  return(R2)
}

# função para a modelagem geoestatística
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
  if(variavel == "XCO2"){
    df_aux <- df_aux %>%
      mutate(
        z=log(z)
      ) %>%
      group_by(x,y) %>%
      summarise(z = mean(z))
    }else{
      df_aux <- df_aux %>%
        group_by(x,y) %>%
        summarise(z = mean(z))
    }

  coordinates(df_aux)= ~ x + y
  form <- z ~ 1

  # Criando o variograma experimental
  vario <- variogram(form, data=df_aux, cutoff=20, width=1.5,cressie=FALSE)
  vario_exp <- vario  %>%
    ggplot(aes(x=dist, y=gamma)) +
    geom_point()+
    labs(title = paste0(variavel,"-",dia) )+
    theme_bw()
  ggsave(paste0("img/variograma_experimental/",variavel,"_",dia,".png"),vario_exp)

  #validação cruzada
  ## validação Cruzada
  m <- vgm(1, modelo, 10, 0)
  df_aux_g <- gstat(id=as.character(form)[2], formula = form, data=df_aux)
  df_aux_g <- gstat(df_aux_g, model =  m, fill.all = TRUE)
  x <- variogram(df_aux_g, cutoff = 20)
  df_fit = fit.lmc(x, df_aux_g)
  out = gstat.cv(df_fit, nmax = 16)
  cross_validate <- out %>% as.tibble() %>%
    ggplot(aes(x=observed,z.pred)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggpubr::stat_regline_equation(ggplot2::aes(
      label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
    theme_bw()
  ggsave(paste0("img/validacao_cruzada/",variavel,"_",dia,"_",modelo,".png"),cross_validate)



  # modelando o semivariograma
  m_vario <- fit.variogram(vario,fit.method = 7,
                           vgm(1, modelo, 10, 0)
  )
  sqr.f1<-round(attr(m_vario, "SSErr"),4)
  c0<-round(m_vario$psill[[1]],4)
  c0_c1<-round(sum(m_vario$psill),4)
  a<-round(m_vario$range[[2]],2)
  r2<-round(r2findWLS(m_vario,vario),8)
  texto_ajuste <- paste(modelo,"(C0= ",c0,"; C0+C1= ", c0_c1, "; a= ", a,"; SQR = ", sqr.f1,"; R2 = ",r2,")",sep="")
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

  if(variavel == "XCO2"){
    krigagem <- tibble::as.tibble(ko_var) %>%
      mutate(
        bt = exp(var1.pred + (var1.var/2)),
        var1.pred = bt * (mean(exp(df_aux$z))/mean(bt)),
        bt_c = (exp(mean(df_aux$z)+.5*var(df_aux$z))/exp(mean(var1.pred)+.5*var(var1.var)))*exp(var1.pred)
      ) %>%
      dplyr::mutate(flag = def_pol(X,Y,pol_ma) | def_pol(X,Y,pol_to)
                    | def_pol(X,Y,pol_pi) | def_pol(X,Y,pol_ba)
                    | def_pol(X,Y,poli_micro)
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
        height = ggplot2::unit(0.2,"cm"))+
      geom_polygon(data=poli_micro %>% as.tibble(),
                   aes(x=X,y=Y),color="red", fill="lightblue", alpha=.0,
                   size=1)

  }else{
    krigagem <- tibble::as.tibble(ko_var) %>%
      dplyr::mutate(flag = def_pol(X,Y,pol_ma) | def_pol(X,Y,pol_to)
                    | def_pol(X,Y,pol_pi) | def_pol(X,Y,pol_ba)
                    | def_pol(X,Y,poli_micro)
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
        height = ggplot2::unit(0.2,"cm"))+
      geom_polygon(data=poli_micro %>% as.tibble(),
                   aes(x=X,y=Y),color="red", fill="lightblue", alpha=.0,
                   size=1)
    }
  ggsave(paste0("img/krig/",variavel,"_",dia,"_",modelo,".png"),krigagem)
}
