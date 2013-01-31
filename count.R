count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) stop("error!!! cause is null")
  
  ## Check that specific "cause" is allowed; else throw error
  x <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  
  if (!any(x == as.character(cause))) stop("error!!! cause is not is valid items")
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Using regular expression to extract causes of death
  asphyx <- regexpr("<dd>Cause: [Aa][Ss][Pp][Hh][Yy][Xx][Ii][Aa][Tt][Ii][Oo][Nn]</dd>", homicides)
  blunt <- regexpr("<dd>Cause: [Bb][Ll][Uu][Nn][Tt] [Ff][Oo][Rr][Cc][Ee]</dd>", homicides)
  shooting <- regexpr("<dd>Cause: [Ss][Hh][Oo][Oo][Tt][Ii][Nn][Gg]</dd>", homicides)
  stab <- regexpr("<dd>Cause: [Ss][Tt][Aa][Bb][Bb][Ii][Nn][Gg]</dd>", homicides)
  other <- regexpr("<dd>Cause: [Oo][Tt][Hh][Ee][Rr]</dd>", homicides)
  unknown <- regexpr("<dd>Cause: [Uu][Nn][Kk][Nn][Oo][Ww][Nn]</dd>", homicides)
  
  if (cause == "asphyxiation")
    cnt <- regmatches(homicides, asphyx)
  else if (cause == "blunt force")
    cnt <- regmatches(homicides, blunt)
  else if (cause == "shooting")
    cnt <- regmatches(homicides, shooting)
  else if (cause == "other")
    cnt <- regmatches(homicides, other)
  else if (cause == "stabbing")
    cnt <- regmatches(homicides, stab)
  else
    cnt <- regmatches(homicides, unknown)
  
  ## Return integer containing count of homicides for that cause
  length(cnt)
}
