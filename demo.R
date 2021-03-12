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

print(dplyr::tibble(
  x = 0:16,
  density = dbinom(x, 16, 1/3)
) %>% gf_col(
  density ~ x
) %>%
  gf_labs(
    x = NULL,
    y = "Density"))

print(cgbinom(c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875), 16, 1/3, legend = FALSE))

print(cgbinom(0.5, 2000, 0.5))
