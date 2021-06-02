meta.to.rma <- function(meta){
  require(metafor)
  
  subset <- ".subset" %in% colnames(meta$data)
  exclude <- ".exclude" %in% colnames(meta$data)
  ## include only studies in which OR (TE) is not NA
  TE.NA <- !is.na(meta$TE)
  ## define the dfframe (df) and indices which needs to be included (inc)
  ## before computing sample sizes
  if(subset == TRUE & exclude == FALSE){
    df <- meta$data[meta$data$.subset == TRUE,]
    inc <- TE.NA
  } else 
    if(subset == TRUE & exclude == TRUE){
      df <- meta$data[meta$data$.subset == TRUE,]
      inc <- TE.NA & !(meta$exclude)
    } else
      if(subset == FALSE & exclude == FALSE){
        df <- meta$data
        inc = TE.NA
      } else
        if(subset == FALSE & exclude == TRUE){
          df <- meta$data
          inc <- !(meta$exclude)
        }  
  df <- df[inc,]
  
  df$TE <- meta$TE[TE.NA]
  df$seTE <- meta$seTE[TE.NA]
  
  df$nSample <- with(df, nCases+nControls)
  
  ## z-test or hakn adjustment
  if(meta$hakn == TRUE){
    test <- "knha"
  } else {test <- "z"}
  
  rma <- rma(yi = TE, sei = seTE, slab = df$.studlab, method = "DL", data = df, test = test)
  
  output <- list(rma, df)
  return(output)
  
  # the function returns a list of length two:
     ##1: List[[1]]: is an rma object
     ##2: List[[2]]: is a dataframe
}