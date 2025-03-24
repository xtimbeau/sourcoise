
data <- tibble::tibble( a = rep("attention", args$size),
                b = 1:args$size,
                r = runif(args$size))
return(data)
