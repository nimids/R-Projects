if(!exists("chrissTools") || !is.environment(chrissTools)){
  chrissTools <- new.env()
  chrissTools$source <- function(f, encoding = 'UTF-8', local = .GlobalEnv) {
    eval(parse(f, encoding = encoding), envir = local)
  }
  attach(chrissTools, warn.conflicts = FALSE)
  chrissTools$printf <- function(...) cat(sprintf(...))
}
if(exists("chrissTools") && all(is.na(match(search(),"chrissTools")))){
  attach(chrissTools, warn.conflicts = FALSE)
}
if(!("mosaic" %in% installed.packages()[,1])){
  #We only need knitr to nicely print tables, rmarkdown includes it but we don't
  #  need to require it. Later we may need rmarkdown so we will get it out of
  #  the way now
  install.packages("mosaic")
  if(!("mosaic" %in% installed.packages()[,1])){
    stop("mosaic isn't installed for some reason! :(")
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
source("https://raw.githubusercontent.com/nimids/R-Projects/main/cxnorm.R", local = chrissTools)
source("https://raw.githubusercontent.com/nimids/R-Projects/main/cxbinom.R", local = chrissTools)
