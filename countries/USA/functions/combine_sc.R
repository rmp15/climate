combine_sc <- function(dat,scloc.df.sc) {
  for (nr in 1:nrow(scloc.df.sc)) {
    fipsfind <- scloc.df.sc[nr,-1]
    fipsfind <- as.vector(fipsfind[fipsfind!=""])
    fipsrepl <- unlist(scloc.df.sc[nr,1])
    dat$fips[dat$fips %in% fipsfind] <- fipsrepl
  }
  return(dat)
}