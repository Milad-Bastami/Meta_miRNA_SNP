# Function to save a number of meta objects.
# meta objects for each miRNA polymorphism (e.g. miR.146a) are like (miR.146a.{name of model} or miR.146a.{subgroup})
# This function is useful for concatenating, formatting and saving a large number of meta objects
# (combination of various SNPs, subgroups, and/or models) 
meta.save <- function(vec1, vec2 = c("Homo", "Hetro", "Dom", "Rec", "All"), filename = "df.txt"){
  require(dplyr)
 # vec1 is the vector of meta objects e.g. c("miR.146a", "miR.196a.ov")  
 # vec2 is the vector of genetic models or subgroups
  
  df <- data.frame(nStudies=NA, OR.FE=NA, OR.FE.L=NA, OR.FE.U=NA, Pz.FE=NA, OR.RE=NA, OR.RE.L=NA, OR.RE.U=NA, Pz.RE=NA, I2=NA, I2.L=NA, I2.U=NA,  Tau=NA, Phet= NA)
  
  # with envir argument, mget() will look for the objects in the chunk environment. 
  # By default it looks in globalenv.
  # this is useful for sourcing and implementing save.meta() in Rmarkdown.
  obj <- mget(apply(expand.grid(vec1, vec2), 1, paste, collapse ="."), envir = knitr::knit_global())

  for (i in 1:length(obj)){
   meta <- obj[[i]] 
   df[i,"nStudies"] <- meta$k
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
   row.names(df)[i] <- names(obj)[i] 
  }
  df <- df %>% rownames_to_column("Model")
  
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
  
  write.table(df, file = filename, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}




