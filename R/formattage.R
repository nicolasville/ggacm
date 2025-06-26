


as_pvalue <- function(x) scales::label_pvalue(decimal.mark = ",")(x)


as_0.01 <- function(x) scales::label_number(accuracy = 0.01, decimal.mark = ",")(x)
