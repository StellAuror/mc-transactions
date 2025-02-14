lcs <- function(X, Y) {
  m <- nchar(X)
  n <- nchar(Y)
  
  L <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  for (i in 1:m) {
    for (j in 1:n) {
      if (substr(X, i, i) == substr(Y, j, j)) {
        L[i + 1, j + 1] <- L[i, j] + 1
      } else {
        L[i + 1, j + 1] <- max(L[i + 1, j], L[i, j + 1])
      }
    }
  }
  
  lcs_length <- L[m + 1, n + 1]
  lcs_str <- character(lcs_length)
  
  i <- m
  j <- n
  while (i > 0 && j > 0) {
    if (substr(X, i, i) == substr(Y, j, j)) {
      lcs_str[lcs_length] <- substr(X, i, i)
      i <- i - 1
      j <- j - 1
      lcs_length <- lcs_length - 1
    } else if (L[i, j + 1] > L[i + 1, j]) {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }
  
  return(paste(lcs_str, collapse = ""))
}

find_subs <- function(descriptions) {
  if (length(descriptions) == 1) return(descriptions)
  
  common_seq <- descriptions[1]
  for (desc in descriptions[-1]) {
    common_seq <- lcs(common_seq, desc)
    if (nchar(common_seq) == 0) break
  }
  
  return(common_seq)
}

posix_diff <- function(time1, time2) {
  time1 + as.numeric(difftime(time2, time1, units = "secs")) / 2
}

bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}