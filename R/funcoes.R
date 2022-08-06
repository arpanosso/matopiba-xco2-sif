#' @title Define se postos pertence a um polígono
#'
#' @name def_pol
#'
#' @description Verifica para um ou mais pontos se eles se enquadram em um
#' determinado polígono.
#'
#' @param x Vetor com a coordenado x do ponto
#' @param y Vetor com a coordenada y do ponto
#' @param pol matriz do polígono
#'
#'
#' @details Função baseada na função point.in.pol do pacote sp, utilziada para
#' filtrar os pontos pertencentes aos polígonos dos estados, regiões e muicípios
#' do Brasil.

def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}
