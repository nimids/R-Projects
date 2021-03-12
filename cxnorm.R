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
cgnorm <- function(q, mean = 0, sd = 1,
                   label = TRUE,
                   legend = label,
                   digits = 5,
                   xlim = mean + c(-4, 4) * sd,
                   ylim = c(0, 1.1 * dnorm(mean, mean, sd)),
                   cSBegin = 0,
                   cSEnd = 1,
                   cSOption = c("D", "A", "B", "C", "E")){
  cSOption <- match.arg(cSOption)
  q = sort(unique(q))
  breaks = c(-Inf, q, Inf)
  if(length(q) == 1){
    labels = c(
      sprintf(paste0("P(\U0001d54f \u2264 %.", digits, "G) = %.", digits, "G"), q, pnorm(breaks[1], mean, sd) - pnorm(breaks[2], mean, sd)),
      sprintf(paste0("P(\U0001d54f > %.", digits, "G) = %.", digits, "G"), q, pnorm(breaks[2], mean, sd) - pnorm(breaks[3], mean, sd))
    )
  }else{
    labels = sapply(
      1:(length(breaks)-1),
      function(i){
        sprintf(
          paste0("P(%.", digits, "G \u2264 \U0001d54f < %.", digits, "G) = %.", digits, "G"),
          breaks[i],
          breaks[i+1],
          pnorm(breaks[i+1], mean, sd) - pnorm(breaks[i], mean, sd)
        )
      }
    )
  }
  data <- dplyr::tibble(
    q = seq(xlim[1], xlim[2], length.out = 500),
    density = dnorm(q, mean, sd),
    Parts = cut(q, breaks, labels)
  )
  resultPlot <- data %>%
    gf_area(
      density ~ q,
      group = ~ Parts,
      fill = ~ Parts,
      show.legend = legend
    ) %>%
    gf_refine(
      coord_cartesian(ylim = ylim),
      scale_fill_viridis_d(begin = cSBegin, end = cSEnd, option = cSOption),
      scale_color_viridis_d(begin = cSBegin, end = cSEnd, option = cSOption)
    ) %>%
    gf_labs(
      x = NULL,
      y = "Density") 
  if(label){
    meanDensity <- dnorm(mean, mean, sd)
    labels <- dplyr::tibble(
      q = q,
      z = (q - mean)/sd,
      height = dnorm(q, mean, sd) + meanDensity * 0.05,
      label = sprintf(paste0("q = %." ,digits, "G\nz = %." ,digits, "G"), q, z)
    )
    resultPlot <- resultPlot %>%
      gf_text(
        height ~ q,
        label = ~label,
        data = labels, 
        vjust = -0.2,
        hjust = 0.5,
        inherit = FALSE,
        show.legend = FALSE
      ) %>%
      gf_segment(
        0 + height ~ (q) + (q),
        data = labels,
        inherit = FALSE)
  }
  return(resultPlot)
}
cdnorm <- function (x, mean = 0, sd = 1, lower.tail = TRUE, 
                    plot = TRUE,
                    label = TRUE,
                    verbose = TRUE,
                    invisible = FALSE,
                    legend = label,
                    digits = 5,
                    xlim = mean + c(-4, 4) * sd,
                    ylim = c(0, 1.1 * dnorm(mean, mean, sd)),
                    cSBegin = 0,
                    cSEnd = 1,
                    cSOption = c("D", "A", "B", "C", "E"),
                    ...,
                    return = c("value", "plot", "table")) 
{
  return <- match.arg(return)
  cSOption <- match.arg(cSOption)
  table <- dplyr::tibble(
    x = x,
    z = (x - mean)/sd,
    density = dnorm(x, mean, sd)
  )
  if (verbose) {
    printf(paste0("If \U0001d54f ~ Norm(%." ,digits, "G, %." ,digits, "G)\n"),
           mean, sd)
    if(length(x) == 1){
      printf(paste0("    Pr(\U0001d54f = %." ,digits, "G) = P(\u2124 = %.",
                    digits, "G) = %." ,digits, "G\n"), table$x, table$z,
             table$d)
    }else{
      printf("    P(\U0001d54f = x) = P(\u2124 = z) = density\n")
      if("knitr" %in% installed.packages()[,1]){
        cat(knitr::kable(table, digits = digits, format = "rst",
                         align = "rrcc"), sep = "\n")
      }else{
        printf("Install knitr for nicer tables\n")
        print(table)
      }
    }
  }
  resultPlot <- cgnorm(q = x, mean = mean, sd = sd,  label = FALSE,
                       legend = legend, digits = digits, xlim = xlim, ylim = ylim, 
                       cSBegin = cSBegin, cSEnd = cSEnd, cSOption = cSOption)
  if(label){
    eHeight <- dnorm(mean, mean, sd) * 0.05
    labels <- dplyr::tibble(
      x = x,
      z = (x - mean)/sd,
      d = dnorm(x, mean, sd),
      height = dnorm(x, mean, sd) + eHeight,
      label = sprintf(paste0("x = %." ,digits, "G\nz = %." ,digits, "G\nd = %." ,digits, "G"), x, z, d)
    )
    resultPlot <- resultPlot %>%
      gf_text(
        data = labels,
        height ~ x,
        label = ~label,
        vjust = -0.2,
        hjust = 0.5,
        inherit = FALSE,
        show.legend = FALSE
      ) %>%
      gf_segment(
        0 + height ~ x + x,
        data = labels,
        inherit = FALSE)
  }
  if(plot){
    print(resultPlot)
  }
  if (return == "plot") {
    toReturn = resultPlot
  }else if (return == "table") {
    toReturn = table
  }else{
    toReturn = dnorm(x, mean = mean, sd = sd)
  }
  if(invisible){
    return(invisible(toReturn))
  }else{
    return(toReturn)
  }
}
cpnorm <- function (q, mean = 0, sd = 1, lower.tail = TRUE, 
                    plot = TRUE,
                    label = TRUE,
                    verbose = TRUE,
                    invisible = FALSE,
                    legend = label,
                    digits = 5,
                    xlim = mean + c(-4, 4) * sd,
                    ylim = c(0, 1.1 * dnorm(mean, mean, sd)),
                    cSBegin = ifelse(lower.tail, 0, 1),
                    cSEnd = ifelse(lower.tail, 1, 0),
                    cSOption = c("D", "A", "B", "C", "E"),
                    ...,
                    return = c("value", "plot", "table")) 
{
  return <- match.arg(return)
  cSOption <- match.arg(cSOption)
  table <- dplyr::tibble(
    q = q,
    z = (q - mean)/sd,
    lowerTail = pnorm(q, mean, sd),
    upperTail = pnorm(q, mean, sd, lower.tail = FALSE)
  )
  if (verbose) {
    printf(paste0("If \U0001d54f ~ Norm(%." ,digits, "G, %." ,digits, "G)\n"),
           mean, sd)
    if(length(q) == 1){
      printf(paste0("    P(\U0001d54f \u2264 %." ,digits, "G) = P(\u2124 \u2264 %.",
                    digits, "G) = %." ,digits, "G\n"), table$q, table$z,
             table$lowerTail)
      printf(paste0("    P(\U0001d54f >  %." ,digits, "G) = P(\u2124 >  %.",
                    digits, "G) = %." ,digits, "G\n"), table$q, table$z,
             table$upperTail)
    }else{
      printf("    P(\U0001d54f \u2264 q) = P(\u2124 \u2264 z) = lowerTail\n")
      printf("    P(\U0001d54f >  q) = P(\u2124 >  z) = upperTail\n")
      if("knitr" %in% installed.packages()[,1]){
        cat(knitr::kable(table, digits = digits, format = "rst",
                         align = "rrcc"), sep = "\n")
      }else{
        printf("Install knitr for nicer tables\n")
        print(table)
      }
    }
  }
  resultPlot <- cgnorm(q = q, mean = mean, sd = sd,  label = label,
                       legend = legend, digits = digits, xlim = xlim, ylim = ylim, 
                       cSBegin = cSBegin, cSEnd = cSEnd, cSOption = cSOption)
  if(plot){
    print(resultPlot)
  }
  if (return == "plot") {
    toReturn = resultPlot
  }else if (return == "table") {
    toReturn = table
  }else{
    toReturn = pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail)
  }
  if(invisible){
    return(invisible(toReturn))
  }else{
    return(toReturn)
  }
}
cqnorm <- function (p, mean = 0, sd = 1, lower.tail = TRUE, 
                    plot = TRUE,
                    label = TRUE,
                    verbose = TRUE,
                    invisible = FALSE,
                    legend = label,
                    digits = 5,
                    xlim = mean + c(-4, 4) * sd,
                    ylim = c(0, 1.1 * dnorm(mean, mean, sd)),
                    cSBegin = ifelse(lower.tail, 0, 1),
                    cSEnd = ifelse(lower.tail, 1, 0),
                    cSOption = c("D", "A", "B", "C", "E"),
                    ...,
                    return = c("value", "plot", "table")) 
{
  return <- match.arg(return)
  cSOption <- match.arg(cSOption)
  table <- dplyr::tibble(
    p = p,
    lowerTailZ = qnorm(p),
    lowerTailQ = qnorm(p, mean, sd),
    upperTailZ = qnorm(p, lower.tail = FALSE),
    upperTailQ = qnorm(p, mean, sd, lower.tail = FALSE)
  )
  if (verbose) {
    printf(paste0("If \U0001d54f ~ Norm(%." ,digits, "G, %." ,digits, "G)\n"), mean, sd)
    if(length(p) == 1){
      printf(paste0("    P(\U0001d54f \u2264 %." ,digits, "G) = P(\u2124 \u2264 %." ,digits, "G) = %." ,digits, "G\n"), table$lowerTailQ, table$lowerTailZ, table$p)
      printf(paste0("    P(\U0001d54f >  %." ,digits, "G) = P(\u2124 >  %." ,digits, "G) = %." ,digits, "G\n"), table$upperTailQ, table$upperTailZ, table$p)
    }else{
      printf("    P(\U0001d54f \u2264 lowerTailQ) = P(\u2124 \u2264 lowerTailZ) = p\n")
      printf("    P(\U0001d54f >  upperTailQ) = P(\u2124 >  upperTailZ) = p\n")
      if("knitr" %in% installed.packages()[,1]){
        cat(knitr::kable(table, digits = digits, format = "rst", align = "rrcc"), sep = "\n")
      }else{
        printf("Install knitr for nicer tables\n")
        print(table)
      }
    }
  }
  q = qnorm(p, mean, sd)
  resultPlot <- cgnorm(q = q, mean = mean, sd = sd,  label = label,
                       legend = legend, digits = digits, xlim = xlim, ylim = ylim,
                       cSBegin = cSBegin, cSEnd = cSEnd, cSOption = cSOption)
  if(plot){
    print(resultPlot)
  }
  if (return == "plot") {
    toReturn = resultPlot
  }else if (return == "table") {
    toReturn = table
  }else{
    toReturn = qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail)
  }
  if(invisible){
    return(invisible(toReturn))
  }else{
    return(toReturn)
  }
}
#What colors do you like best?
gridExtra::grid.arrange(grobs = lapply(c("D", "A", "B", "C", "E"), function(cSOption){
  plot = cpnorm(c(-1.15035,-0.6745, -0.31864, 0, 0.31864, 0.6745, 1.15035),
                cSOption = cSOption, plot = F, label = F, verbose = F, return = "plot") %>%
    gf_labs(title = paste0("Color Scale Option: ", cSOption))
  }), nrow=3, ncol=2)
#What theme?
gridExtra::grid.arrange(grobs = lapply(c(theme_gray, theme_bw, theme_linedraw, theme_light, theme_dark, theme_minimal, theme_classic, theme_void), function(theme){
  plot = cpnorm(c(-1.15035,-0.6745, -0.31864, 0, 0.31864, 0.6745, 1.15035),
                plot = F, label = F, verbose = F, return = "plot") %>%
    gf_theme(theme())
  }), nrow=3, ncol=3)

#demo:
cdnorm(c(2.699301, 3.651020, 4.362721, 5.000000, 5.637279, 6.348980, 7.300699), 5, 2)
cpnorm(c(2.699301, 3.651020, 4.362721, 5.000000, 5.637279, 6.348980, 7.300699), 5, 2)
cpnorm(c(2.699301, 3.651020, 4.362721, 5.000000, 5.637279, 6.348980, 7.300699), 5, 2, lower.tail = F)
cqnorm(c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875), 5, 2)
cqnorm(c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875), 5, 2, lower.tail = F)
