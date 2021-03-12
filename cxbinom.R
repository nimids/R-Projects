if(!require("mosaic")){
  #if it didn't load we can try to install it
  install.packages("mosaic")
  if(!require("mosaic")){
    stop("Mosaic isn't loading for some reason! :(")
  }
}
if(!("knitr" %in% installed.packages()[,1])){
  #We only need knitr to nicely print tables, rmarkdown includes it but we don't
  #  need to require it. Later we may need rmarkdown so we will get it out of
  #  the way now
  install.packages("rmarkdown")
  if(!("knitr" %in% installed.packages()[,1])){
    stop("knitr isn't installed for some reason! :(")
  }
}

printf <- function(...) cat(sprintf(...))
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

print(dplyr::tibble(
  x = 0:16,
  density = dbinom(0:16, 16, 1/3)
) %>% gf_col(
  density ~ x
) %>%
  gf_labs(
    x = NULL,
    y = "Density"))
print(cgbinom(c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875), 16, 1/3, legend = FALSE))

print(cgbinom(0.5, 2000, 0.5))
