# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#合并ps对象
#有两种模式，model =1 是取共有部分进行合并
#model =2 是保留x中的全部OTU，y中如果有其他OTU，全部舍弃，如果没有x中的这些otu使用0填充。
#注意进化树可不能合并，必须重新运算，这里没有必要花费很长的时间来运算这个。
#model = 3,xy全部OTU进行合并


merge_ps = function(ps1,ps2,model = 1){
  library("phyloseq")
  library(tidyverse)

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


  #otu_table
  if(model == 1){
    otu_table1 = as.data.frame(t(vegan_otu(ps1)))
    dim(otu_table1)
    otu_table2 = as.data.frame(t(vegan_otu(ps2)))
    dim(otu_table2)
    otu_table3 = merge(otu_table1,otu_table2,by = "row.names",all = T)
    dim(otu_table3)
    row.names(otu_table3) = otu_table3$Row.names
    otu_table3$Row.names = NULL
    otu_table3 = as.matrix(otu_table3)
    otu_table3[is.na(otu_table3)] <- 0
    #tax_table

    tax_table1 = as.data.frame((vegan_tax(ps1)))
    dim(tax_table1)
    tax_table2 = as.data.frame((vegan_tax(ps2)))
    dim(tax_table2)
    head(tax_table1)
    head(tax_table2)
    tax_table3 = rbind(tax_table1,tax_table2)
    dim(tax_table3)

  }
  if(model == 2){
    otu_table1 = as.data.frame(t(vegan_otu(ps1)))
    dim(otu_table1)
    otu_table2 = as.data.frame(t(vegan_otu(ps2)))
    dim(otu_table2)
    otu_table3 = merge(otu_table1,otu_table2,by = "row.names",all.x = T)
    dim(otu_table3)
    row.names(otu_table3) = otu_table3$Row.names
    otu_table3$Row.names = NULL
    otu_table3 = as.matrix(otu_table3)
    otu_table3[is.na(otu_table3)] <- 0
    #tax_table

    tax_table1 = as.data.frame((vegan_tax(ps1)))
    dim(tax_table1)
    tax_table3 = tax_table1
  }
  if(model == 3){
    otu_table1 = as.data.frame(t(vegan_otu(ps1)))
    dim(otu_table1)
    otu_table2 = as.data.frame(t(vegan_otu(ps2)))
    dim(otu_table2)
    otu_table3 = merge(otu_table1,otu_table2,by = "row.names",all = T)
    dim(otu_table3)
    row.names(otu_table3) = otu_table3$Row.names
    otu_table3$Row.names = NULL
    otu_table3 = as.matrix(otu_table3)
    otu_table3[is.na(otu_table3)] <- 0
    #tax_table
    tax_table1 = as.data.frame((vegan_tax(ps1)))
    dim(tax_table1)
    tax_table2 = as.data.frame((vegan_tax(ps2)))
    dim(tax_table2)
    head(tax_table1)
    head(tax_table2)
    tax_table3 = rbind(tax_table1,tax_table2)
    dim(tax_table3)
  }


  mapping1 = as.data.frame(sample_data(ps1))
  head(mapping1)
  mapping1 = mapping1[,c("SampleType","Description")]
  mapping2 = as.data.frame(sample_data(ps2))
  head(mapping2)
  # mapping2$BarcodeSequence = NULL
  # mapping2$LinkerPrimerSequence = NULL
  mapping2$SampleType = paste(mapping2$fianl_SampleType,mapping2$zone,sep = "_")
  mapping2 = mapping2[,c("SampleType","Description")]
  mapping3 = rbind(mapping1,mapping2)
  head(mapping3)


  ps_add_out =phyloseq(otu_table(as.matrix(otu_table3),taxa_are_rows = T),
                       sample_data(mapping3),
                       tax_table(as.matrix(tax_table3)))
  return(ps_add_out)
}

