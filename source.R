if(!exists("chrissTools") || !is.environment(chrissTools)){
  chrissTools <- new.env()
  chrissTools$source <- function(f, encoding = 'UTF-8', local = .GlobalEnv) {
    eval(parse(f, encoding = encoding), envir = local)
  }
  chrissTools$printf <- function(...) cat(sprintf(...))
  attach(chrissTools, warn.conflicts = FALSE)
}
if(exists("chrissTools") && all(is.na(match(search(),"chrissTools")))){
  attach(chrissTools, warn.conflicts = FALSE)
}
if(!("mosaic" %in% installed.packages()[,1])){
  install.packages("mosaic")
  if(!("mosaic" %in% installed.packages()[,1])){
    stop("mosaic isn't installed for some reason! :(")
  }
}
if(!("rmarkdown" %in% installed.packages()[,1])){
  install.packages("rmarkdown")
  if(!("rmarkdown" %in% installed.packages()[,1])){
    stop("rmarkdown isn't installed for some reason! :(")
  }
}
source("tools.R", local = chrissTools)
source("cxnorm.R", local = chrissTools)
source("cxbinom.R", local = chrissTools)
source("rpick.R", local = chrissTools)
detach(chrissTools)
attach(chrissTools, warn.conflicts = FALSE)
