combine_mc <- function(dat,scloc.df) {
  for (nr in 1:nrow(scloc.df)) {
    fipsfind <- scloc.df[nr,-c(1,ncol(scloc.df))]
    fipsfind <- as.vector(fipsfind[is.na(fipsfind)==FALSE])
    fipsrepl <- unlist(scloc.df[nr,1])
    dat$fips[dat$fips %in% fipsfind] <- fipsrepl
  }
  return(dat)
}