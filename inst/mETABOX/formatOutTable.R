#http://stackoverflow.com/questions/13545547/how-to-write-a-data-frame-with-one-column-a-list-to-a-file
formatOutTable <- function(df){
  df[sapply(df, is.list)] = apply(df[sapply(df, is.list)], 1, function(x) paste(unlist(x), sep=", ", collapse=", "))
  return(df)
}