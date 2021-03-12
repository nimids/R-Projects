if(!require("mosaic")){
  stop("Mosaic isn't loading for some reason! :(")
}
cgbinom <- function(p, size, prob,
                   label = TRUE,
                   legend = label,
                   digits = 5,
                   cSBegin = 0,
                   cSEnd = 0.97,
                   cSOption = c("C", "A", "B", "D", "E")){
  cSOption <- match.arg(cSOption)
  p = sort(unique(p))
  breaks = c(0, p, 1)
  if(length(p) == 1){
    labels = c(
      sprintf("p \u2264 %.5G", p),
      sprintf("p > %.5G", p)
    )
  }else{
    labels = sapply(
      1:(length(breaks)-1),
      function(i){
        sprintf(
          paste0("%.", digits, "G \u2264 p < %.", digits, "G"),
          breaks[i],
          breaks[i+1]
        )
      }
    )
  }
  breakPointsL = sort(c(pbinom((0:size)-1, size, prob), p))
  breakPointsU = sort(c(pbinom(0:size, size, prob), p))
  breakPointsA = (breakPointsL+breakPointsU)/2
  data <- dplyr::tibble(
    x = qbinom(breakPointsA,size,prob),
    density = sapply(1:(length(breakPointsL)),function(i){breakPointsU[i]-breakPointsL[i]}),
    Parts = cut(breakPointsA, breaks)
  )
  resultPlot <- data %>%
    gf_col(
      density ~ x,
      fill = ~Parts,
      group = ~Parts,
      position = position_stack(reverse = TRUE),
      show.legend = legend
    ) %>%
    gf_refine(
      scale_fill_viridis_d(begin = cSBegin, end = cSEnd, option = cSOption),
      scale_color_viridis_d(begin = cSBegin, end = cSEnd, option = cSOption)
    ) %>%
    gf_labs(
      x = NULL,
      y = "Density")
  return(resultPlot)
}
