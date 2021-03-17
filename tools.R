GCD <- function(vector = NULL, gformula = NULL, data = NULL){
  things <- list(a = vector, b = gformula, c = data)
  things <- Filter(Negate(is.null), things)
  if(length(things) == 2){
    if(inherits(vector, "formula")){
      gformula <- vector
      vector <- eval(gformula[[2]], bootstrap)
    }
    if(inherits(vector, "data.frame")){
      data <- vector
      vector <- eval(gformula[[2]], bootstrap)
    }
  }else if(length(things) != 1){
    stop("incorrect arguments")
  }
  vector <- sort(unique(vector[vector != 0]))
  while(length(vector) != 0){
    lowest <- vector[1]
    vector <- vector %% lowest
    vector <- sort(unique(vector[vector != 0]))
  }
  return(lowest)
}


#The arguments should be self explanatory see Chris during office hour for a detailed explanation.
buildHistogram <- function(data, gformula, title = "",
                           addQuantLines = FALSE, addSDLines = FALSE,
                           addNormalCurve = FALSE,
                           limits = NULL, label = TRUE){
  #3 color blind safe colors on the viridis color scale
  colors = viridis::viridis(3)
  binwidth <- GCD(data, gformula)
  histogram <-
    gf_histogram(
      data = data, gformula,
      binwidth = binwidth
    ) %>%
    gf_labs(
      title = title,
      y = NULL
    )
  if(label){
    histogram <-
      histogram + scale_x_continuous(
        minor_breaks = seq(0, 200, 0.5),
        breaks = seq(0, 200, 1),
        limits = limits
      )
  }else{
    histogram <- histogram +
      scale_x_continuous(limits = limits)
  }
  if(addQuantLines){
    histogram <-
      histogram +
      geom_vline(
        data = data.frame(
          quantLines <- quantile(
            data = data, gformula,
            probs = c(0.025,0.50,0.975)
          )
        ),
        aes(xintercept = quantLines, color = "quant"),
        lwd = 1.5,
        lty = c(3,1,3)
      )
  }
  if(addSDLines){
    SDLines <- mean(data = data, gformula) +
      c(-2,0,2) * sd(data = data, gformula)
    histogram <-
      histogram +
      geom_vline(
        data = data.frame(
          SDLines <- mean(data = data, gformula) +
            c(-2,0,2) * sd(data = data, gformula)
        ),
        aes(xintercept = SDLines, color = "SD"),
        lwd = 1.5,
        lty = c(3,1,3)
      )
  }
  if(addNormalCurve){
    histogram <-
      histogram +
      stat_function(
        fun = function(x)
          dnorm(x, mean(data = data, gformula), sd(data = data, gformula)) *
          binwidth * nrow(data),
        lwd = 1.5,
        aes(colour = "Curve"),
        show.legend = ifelse(addQuantLines || addSDLines, FALSE, NA)
      )
  }
  histogram <-
    histogram +
    scale_colour_manual(
      name = "Legend",
      values = c("quant" = colors[1],
                 "SD" = colors[2],
                 "Curve" = colors[3]),
      labels = c("quant" = "2.5, 50, and 97.5\nPercentile",
                 "SD" = "Mean and SD",
                 "Curve" = "Normal")
    )
  if(!label)histogram <- histogram + theme(legend.position = "none")
  histogram
}
