#  A function to search an environment for objects based on a pattern in their names and/or specific class/calsses 
# the output is a list of all object matching the pattern in that environment. The class of objects may also be set

find.objects <- function(pattern, envir = .GlobalEnv, class=NULL){
  require(tibble)
  
  ## the following code gives a list of all matched objects: desired_list
  ### finding and getting object matching a pattern in their names
  query <- grep(pattern, names(envir), value = TRUE)
  pattern_match <- do.call("list", mget(query, envir = envir))
  
  ### filter the identified objects based on a given class
  if(!is.null(class) & length(class) == 1){
    class_filter <- unlist(sapply(pattern_match, function(x) class %in% class(x) , simplify = TRUE))
    desired_list <- pattern_match[class_filter]
  
  ### filter the identified objects based on given classes  
  } else if (!is.null(class) & length(class) > 1) {
    class_filter <- unlist(sapply(pattern_match, function(x) as.logical(sum(class(x) %in% class))))
    desired_list <- pattern_match[class_filter]
  
  ### get the pattern match as final results regardless of class  
  } else if (is.null(class)){
    desired_list <- pattern_match
  }}

## call the function within the RmarkDown file to retrieve all meta objects (set envir = knitr::knit_global())
desired_objs <- find.objects('pattern', class = 'meta', envir = knitr::knit_global())

## save retrieved meta objects in a readable format 
library(tibble) # for rownames_to_column()
df <- data.frame(nStudies = NA, OR.FE = NA, OR.FE.L = NA, OR.FE.U = NA, Pz.FE = NA, OR.RE = NA, OR.RE.L = NA, OR.RE.U = NA, Pz.RE=NA, I2=NA, I2.L=NA, I2.U=NA,  Tau=NA, Phet= NA)

for (i in 1:length(desired_objs)){
    meta <- desired_objs[[i]] 
    df[i,"nStudies"] <- length(meta$studlab)
    df[i,"OR.FE"] <- exp(meta$TE.fixed)
    df[i,"OR.FE.L"] <- exp(meta$lower.fixed)
    df[i,"OR.FE.U"] <- exp(meta$upper.fixed)
    df[i,"Pz.FE"] <- meta$pval.fixed
    df[i,"OR.RE"] <- exp(meta$TE.random)
    df[i,"OR.RE.L"] <- exp(meta$lower.random)
    df[i,"OR.RE.U"] <- exp(meta$upper.random)
    df[i,"Pz.RE"] <- meta$pval.random
    df[i,"I2"] <- meta$I2
    df[i,"I2.L"] <- meta$lower.I2
    df[i,"I2.U"] <- meta$upper.I2
    df[i,"Tau"] <- meta$tau
    df[i,"Phet"] <- meta$pval.Q
    row.names(df)[i] <- names(desired_objs)[i] 
}

## save object names as a column to keep track of different analyses
df <- df %>% rownames_to_column("Analysis")
 
## format the numbers for publishing & rearrange columns 
df <- within(df, {
    Fixed.Effect <- paste0(round(OR.FE, digits = 2), " (", round(OR.FE.L, digits = 2), "-", round(OR.FE.U, digits = 2), ")")
    Fixed.Pvalue <- round(Pz.FE, digits = 2)
    Random.Effect <- paste0(round(OR.RE, digits = 2), " (", round(OR.RE.L, digits = 2), "-", round(OR.RE.U, digits = 2), ")")
    random.Pvalue <- round(Pz.RE, digits = 2)
    Isqr = round(I2*100, digits = 1)
    Heterogeneity.Pvalue = round(Phet, digits = 2)
    Tau.round = round(Tau, digits = 2)
    Isqr.CI <- paste0(round(I2*100, digits = 1), " (", round(I2.L*100, digits = 1), "-", round(I2.U*100, digits = 1), ")")
})

## the final results: results of each meta-analysis (i.e. a meta object) in a row 
write.table(df, file = 'Meta.objects.txt', row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

