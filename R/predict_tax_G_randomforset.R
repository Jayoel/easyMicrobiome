##属水平的的随机森林模型预测

predict_rand_tax <- function(ps7,ps6,model) {

  library(randomForest)
  library(stats)

  vegan_otu <-  function(physeq){
    OTU <-  otu_table(physeq)
    if(taxa_are_rows(OTU)){
      OTU <-  t(OTU)
    }
    return(as(OTU,"matrix"))
  }

  vegan_tax <-  function(physeq){
    tax <-  tax_table(physeq)

    return(as(tax,"matrix"))
  }
  library(tidyverse)
  psX <- ps6 %>%
    subset_taxa(
      row.names(tax_table(ps6)) %in% row.names(tax_table(ps7))
    )
  psX

  otu  = as.data.frame(t(vegan_otu(psX)))

  head(otu)

  otu_add = matrix(0,nrow = length(setdiff(row.names(tax_table(ps7)), row.names(tax_table(ps6)))),
                   ncol = dim(otu)[2])
  dim(otu_add)
  otu_add = as.data.frame(otu_add)
  row.names(otu_add) = setdiff(row.names(tax_table(ps7)), row.names(tax_table(ps6)))
  colnames(otu_add) = colnames(otu)
  dim(otu)
  otu_fil = rbind(otu,otu_add)
  dim(otu_fil)
  ps8 <- psX
  taxGlomRank = "Genus"
  ps8 = tax_glom(ps8, taxrank = taxGlomRank)


  otutab = otu_fil
  dim(otutab)
  mapping8 = as.data.frame(sample_data(ps8))
  # mapping8 $SampleType = mapping8$fianl_SampleType
  # sample_data(ps6)=  mapping8
  # saveRDS(ps6,"./add_sample_for_predict/No_166//ps_NCBI10.rds")
  # install.packages("randomForest")
  library(randomForest)

  # otutab need transposition for randomForest function
  otutab_t = as.data.frame(t(otutab))

  # as.factor(mapping8$SampleType)
  # Set classification info.
  otutab_t$group = factor(mapping8$SampleType)
  # otutab_t$group = factor(mapping8$SampleType)
  colnames(otutab_t) = paste("OTU",colnames(otutab_t),sep = "")

  otutab.pred = stats::predict(model, otutab_t )
  pre_tab = table(observed=otutab_t[,"OTUgroup"], predicted=otutab.pred)
  pre_tab
  # save prediction result
  predict = data.frame(group = otutab_t[,"OTUgroup"], predicted=otutab.pred)



  summary(predict)

  return(list(pre_tab,summary(predict)))
}
