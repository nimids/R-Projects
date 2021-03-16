if(!suppressMessages(require("mosaic", warn.conflicts = FALSE, quietly = TRUE))){
  stop("Mosaic isn't loading for some reason! :(")
}

#' Random Pick
#'
#' Picks n items from an urn filled with choices, with or without replacement.
#'
#' @rdname rpick
#' @return  for `rpick`, a pickresult object
#'
#' @param n number of draws
#' @param choices vector of choices
#' @param replace if TRUE, draws are put back before the next pick
#' @param verbose if TRUE, forces rpick to print out it's self
#'
#' @examples
#' rpick(10)
#' rpick(10, c("H","T"))
#' do(5) * rpick(10, c(1,2,3))
#' @export

rpick <- function (
  n = 1,
  choices = c("A","B","C"),
  replace = TRUE,
  verbose = FALSE
){
  result <- sample(choices, n, replace = replace)
  pick <- paste(result, collapse = ", ")
  attr(pick, "n") <- n
  attr(pick, "choices") <- choices
  attr(pick, "replace") <- replace
  attr(pick, "result") <- result
  class(pick) <- "pickresult"
  if(verbose){
    print(pick, past = FALSE)
    return(invisible(pick))
  }else{
    return(pick)
  }
}

print.pickresult <- function(object, ...) {
  details <- attributes(object)
  cat(strwrap(
    paste0(
      "Pulled ", details$n, " thing", ifelse(details$n > 1, "s", ""), " from an ",
      "urn with {", paste(details$choices, collapse = ", "), "} inside, ",
      ifelse(details$replace, "with", "without")," replacement.\n"
    ),
    width = getOption("width") - 8,
    exdent = 2
  ), "", sep = "\n")
  cat(strwrap(
    paste0("Result: ", as.character(object)),
    width = getOption("width") - 8,
    exdent = 4
  ), "", sep = "\n")
  cat("Summary:", sep = "\n")
  choices <- unique(details$choices)
  print(data.frame(
    choice = choices,
    numOf = sapply(choices, function(choice)sum(details$result == choice)),
    propOf = sapply(choices, function(choice)sum(details$result == choice))/details$n
  ), row.names = FALSE)
}
.S3method("print", "pickresult")

value.pickresult <- function(object, asList = TRUE, asDataFrame = !asList, ...) {
  details <- attributes(object)
  if(asDataFrame){
    value <- data.frame(n = details$n)
    value[["choices"]] = list(details$choices)
    value[["replace"]] = details$replace
    value[["result"]] = list(details$result)
  }else{
    value <- list(
      n = details$n,
      choices = details$choices,
      replace = details$replace,
      result = details$result
    )
  }
  for(choice in unique(details$choices)){
    value[[paste0("numOf", choice)]] = sum(details$result == choice)
  }
  return(value)
}
.S3method("value", "pickresult")

cull_for_do.pickresult <- function(object, ...) {
  return(value(object, asDataFrame = TRUE))
}
.S3method("cull_for_do", "pickresult")
