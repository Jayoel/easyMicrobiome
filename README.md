# easyMicrobiome
R
运行下面命令安装和测试R包

对微生物群落OTU表格的多种方式合并，具体参见帮助文件
```{R}
library(devtools)
install_github("taowenmicro/easyMicrobiome")
library("easyMicrobiome")
?merge_ps
merge_ps(ps1,ps2,model = 1)
```
