bind_rows2 <- function(l) {

  #checks
  stopifnot(is.list(l))

  #get vars
  v <- unlist(
    lapply(unname(l), \(df) vapply(df, typeof, character(1)))
  )
  nm <- names(v)
  nm0 <- unique(nm[duplicated(nm)])

  #get list of columns with diff types in diff datasets
  #to do use reduce with intersection hwere might make this all easier to read.
  x <- stats::setNames(lapply(nm0, \(x) unique(v[nm == x])), nm0)
  x0 <- x[!sapply(x, \(x) length(unique(x)) == 1)]

  # Convert to highest in type hierarchy
  type_hierarchy <- c("logical" = 1, "integer" = 2, "double" = 3, "character"= 4)
  conv_funs <- lapply(x0, \(x)
                      switch(max(type_hierarchy[x]),
                             match.fun(as.logical),
                             match.fun(as.integer),
                             match.fun(as.double),
                             match.fun(as.character)
                      )
  )
  l1 <- lapply(l, \(df) {
    f <- conv_funs[names(conv_funs) %in% names(df)]
    for(i in 1:length(f)) {
      df[[names(f[i])]] <- f[[i]](df[[names(f[i])]])
    }
    df
  })

  # bind rows and return
  dplyr::bind_rows(l1)
}
