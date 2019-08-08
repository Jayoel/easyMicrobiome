single_diff_and_plot <- function(data_wt = data_wt,plotname = "wentao_",plot = "bar")  {

  for (i in 3:ncol(data_wt)) {
    #构造待分析的子数据框
    ss <- data_wt[i]
    colnames(ss) <- c("count")
    ss$group = data_wt$group

    #正态检验
    xx <-  shapiro.test(as.vector(as.matrix(data_wt[i])))
    (p1 <- xx[[2]])#得要p值
    p1 <- round(p1,3)#保留三为小数
    #方差齐性检验
    xc <- bartlett.test(count~group,data=ss)
    (p2 <- xc[[3]])
    p2 <- round(p2,3)

    if ( plot == "bar") {
      name_i = colnames(data_wt[i])
      #求取均值和方差
      wen1 = as.data.frame(tapply(as.vector(as.matrix(data_wt[i])),data_wt$group,mean,na.rm=TRUE))
      wen2 = as.data.frame(tapply(as.vector(as.matrix(data_wt[i])),data_wt$group,sd,na.rm=TRUE))
      went = cbind(wen1,wen2)
      went
      #p1 >=.05& p2 >=.05:数据符合正态分布，方差齐心
      if (p1 >=.05& p2 >=.05) {
        #进行方差检验 下面wtx3为提取的p值
        model<-aov(count ~ group, data= ss)#方差分析
        wtx1 = summary(model)
        wtx2 = wtx1[[1]]
        wtx3 = wtx2[5]#

        #wtx3$`Pr(>F)`[1]< 0.05：当P值小于0.05时，进行多重比较
        if ( wtx3$`Pr(>F)`[1]< 0.05) {

          out <- LSD.test(model,"group", p.adj="none")#进行多重比较，不矫正P值
          aa = out$group#结果显示：标记字母法
          aa$group = row.names(aa)

          wentao = merge(aa,went, by="row.names",all=F)
          colnames(wentao) = c(colnames(wentao[1:4]),"mean" ,"SD")
          #使用的tidyverse函数，对数据框添加两列，目的为柱状图添加bar
          aa = mutate(wentao, ymin = mean - SD, ymax =  mean + SD)
          a = max(aa$mean)*1.5##用于设置y轴最大值

          ### 出图柱状图
          p = ggplot(aa , aes(x = group, y = mean,colour= group)) +
            geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +
            geom_text(aes(label = groups,y=ymax, x = group,vjust = -0.3,size = 6))+
            geom_errorbar(aes(ymin=ymin,
                              ymax=ymax),
                          colour="black",width=0.1,size = 1)+
            geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            scale_y_continuous(expand = c(0,0),limits = c(0,a))+
            labs(x=paste(name_i,"of all group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,sep = ":"))
          p
          #as.vector(as.matrix(data_wt[i]))为进行差异分析的一组数据
          p=p+Mytheme
          p

          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          p
          FileName <- paste(plotname ,name_i,"_aov_LSD_bar", ".pdf", sep = "_")
          ggsave(FileName, p, width = 8, height = 8)

          #wtx3$`Pr(>F)`[1]>= 0.05当p值大于0.05那么就不能做多重比较了
        }else if ( wtx3$`Pr(>F)`[1]>= 0.05)  {

          colnames(went) = c("mean" ,"SD")
          aa = mutate(went, ymin = mean - SD, ymax =  mean + SD)
          aa$group = levels(ss$group)
          aa
          a = max(aa$mean)*2
          p = ggplot(aa , aes(x = group, y = mean,colour= group)) +
            geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +
            # geom_text(aes(label = groups,y=ymax, x = group,vjust = -0.3,size = 6))+
            geom_errorbar(aes(ymin=ymin,
                              ymax=ymax),
                          colour="black",width=0.1,size = 1)+
            scale_y_continuous(expand = c(0,0),limits = c(0,a))+
            labs(x=paste(name_i,"of all group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,"aov",round(wtx3$`Pr(>F)`[1],3),sep = ":"))
          p
          p=p+Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"aov_nosig"," bar", ".pdf", sep = "_")
          ggsave(FileName, p, width = 8, height = 8)
        }
        #   p1 <.05| p2 <.05：不符合正态检验或者方差不齐性
      }else if (p1 <.05| p2 <.05){

        krusk=compare_means(count ~ group, data=ss, method = "kruskal.test")
        sumkrusk=as.data.frame(krusk)
        sumkrusk
        #多组比较小于0.05，表明多组之间具有差异，可以进行两两非参数检验，并标记字母
        #但是这里没有做，个人认为挺难的，还
        if ( sumkrusk[3]< 0.05) {

          colnames(went) = c("mean" ,"SD")
          aa = mutate(went, ymin = mean - SD, ymax =  mean + SD)
          aa$group = levels(ss$group)
          aa
          a = max(aa$mean)*1.5
          p = ggplot(aa , aes(x = group, y = mean,colour= group)) +
            geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +
            # geom_text(aes(label = groups,y=ymax, x = group,vjust = -0.3,size = 6))+
            geom_errorbar(aes(ymin=ymin,
                              ymax=ymax),
                          colour="black",width=0.1,size = 1)+
            geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            scale_y_continuous(expand = c(0,0),limits = c(0,a))+
            labs(x=paste(name_i,"of all group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,"kruskal.test",sumkrusk[3],sep = ":"))
          p
          p=p+ Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_kruskal.test_YES_bar", ".pdf", sep = "_")
          ggsave(FileName, p, width = 8, height = 8)
          # 如果多组比较大于0.05表明多组之间不具有差异，那么直接出图
        }else if ( sumkrusk[3] >= 0.05)  {
          colnames(went) = c("mean" ,"SD")
          aa = mutate(went, ymin = mean - SD, ymax =  mean + SD)
          aa$group = levels(ss$group)
          aa
          a = max(aa$mean)*1.5

          p = ggplot(aa , aes(x = group, y = mean,colour= group)) +
            geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +
            geom_errorbar(aes(ymin=ymin,
                              ymax=ymax),
                          colour="black",width=0.1,size = 1)+
            scale_y_continuous(expand = c(0,0),limits = c(0,a))+
            geom_hline(aes(yintercept=mean(mean)), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            labs(x=paste(name_i,"of all group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,"kruskal.test",sumkrusk[3],sep = ":"))
          p
          p=p + Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_kruskal.test_nosig_bar", ".pdf", sep = "")
          ggsave(FileName, p, width = 8, height = 8)
        }


      }



    }
    if( plot == "box") {
      name_i = colnames(data_wt[i])

      if (p1 >.05& p2 >.05) {

        #进行方差检验 下面wtx3为提取的p值
        model<-aov(count ~ group, data= ss)#方差分析
        wtx1 = summary(model)
        wtx2 = wtx1[[1]]
        wtx3 = wtx2[5]#
        #p1 >=.05& p2 >=.05:数据符合正态分布，方差齐心
        if ( wtx3$`Pr(>F)`[1]< 0.05) {

          data_box = data_wt[c(1,2,i)]
          colnames(data_box) = c("ID" , "group","dd" )
          out = LSD.test(model,"group", p.adj="none") # alternative fdr
          stat = out$groups
          data_box$stat=stat[as.character(data_box$group),]$groups
          max=max(data_box[,c("dd")])
          min=min(data_box[,c("dd")])
          x = data_box[,c("group","dd")]
          y = x %>% group_by(group) %>% summarise_(Max=paste('max(',"dd",')',sep=""))
          y=as.data.frame(y)
          y
          rownames(y)=y$group
          data_box$y=y[as.character(data_box$group),]$Max + (max-min)*0.05

          # mi=c("#1B9E77" ,"#D95F02", "#7570B3","#E7298A")
          p = ggplot(data_box, aes(x=group, y=data_box[["dd"]], color=group)) +
            geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
            labs(x=paste(name_i," group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,sep = ":"))+
            geom_text(data=data_box, aes(x=group, y=y, color=group, label= stat)) +
            geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+theme(legend.position="none")
          p
          p=p+Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_aov_LSD_box", ".pdf", sep = "")
          ggsave(FileName, p, width = 8, height = 8)

          #wtx3$`Pr(>F)`[1]>= 0.05当p值大于0.05那么就不能做多重比较了
        }else if ( wtx3$`Pr(>F)`[1]>= 0.05)  {
          # out <- LSD.test(model,"group", p.adj="none")#进行多重比较，不矫正P值
          # aa = out$group#结果显示：标记字母法
          # aa$group = row.names(aa)
          # a = max(aa$dd)*1.2

          data_box = data_wt[c(1,2,i)]
          colnames(data_box) = c("ID" , "group","dd" )
          max=max(data_box[,c("dd")])
          min=min(data_box[,c("dd")])
          x = data_box[,c("group","dd")]
          y = x %>% group_by(group) %>% summarise_(Max=paste('max(',"dd",')',sep=""))
          y=as.data.frame(y)
          rownames(y)=y$group
          data_box$y=y[as.character(data_box$group),]$Max + (max-min)*0.05

          p = ggplot(data_box, aes(x=group, y=data_box[["dd"]], color=group)) +
            geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
            geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            labs(x=paste(name_i,"box", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,"aov",round(wtx3$`Pr(>F)`[1],3),sep = ":"))+
            geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+theme(legend.position="none")
          p
          p=p+Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_aov_nosig_box", ".pdf", sep = "")
          ggsave(FileName, p, width = 8, height = 8)
        }


        #   p1 <.05| p2 <.05：不符合正态检验或者方差不齐性
      }else if (p1 <.05| p2 <.05){

        krusk=compare_means(count ~ group, data=ss, method = "kruskal.test")
        sumkrusk=as.data.frame(krusk)
        sumkrusk
        #多组比较小于0.05，表明多组之间具有差异，可以进行两两非参数检验，并标记字母
        #但是这里没有做，个人认为挺难的，还
        if ( sumkrusk[3]< 0.05) {
          data_box = data_wt[c(1,2,i)]
          colnames(data_box) = c("ID" , "group","dd" )
          max=max(data_box[,c("dd")])
          min=min(data_box[,c("dd")])
          x = data_box[,c("group","dd")]
          y = x %>% group_by(group) %>% summarise_(Max=paste('max(',"dd",')',sep=""))
          y=as.data.frame(y)
          rownames(y)=y$group
          data_box$y=y[as.character(data_box$group),]$Max + (max-min)*0.05
          wtq = levels(data_wt$group)
          lis = combn(levels(data_wt$group), 2)
          x <-lis
          my_comparisons <- tapply(x,rep(1:ncol(x),each=nrow(x)),function(i)i)

          p = ggplot(data_box, aes(x=group, y=data_box[["dd"]], color=group)) +
            geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
            labs(x=paste(name_i,"of all group", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,sep = ":"))+
            geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+theme(legend.position="none")+
            stat_compare_means()+
            stat_compare_means(comparisons=my_comparisons,label = "p.signif",hide.ns = F) # Add pairwise

          p
          p=p+Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_kruskal.test_wlc_box_", ".pdf", sep = "")
          ggsave(FileName, p, width = 8, height = 8)
        }else if ( sumkrusk[3] >= 0.05)  {
          data_box = data_wt[c(1,2,i)]
          colnames(data_box) = c("ID" , "group","dd" )

          max=max(data_box[,c("dd")])
          min=min(data_box[,c("dd")])
          x = data_box[,c("group","dd")]
          y = x %>% group_by(group) %>% summarise_(Max=paste('max(',"dd",')',sep=""))
          y=as.data.frame(y)
          rownames(y)=y$group
          data_box$y=y[as.character(data_box$group),]$Max + (max-min)*0.05
          data_box

          p = ggplot(data_box, aes(x=group, y=data_box[["dd"]], color=group)) +
            # geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
            # geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
            geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
            labs(x=paste(name_i,"box", sep = "_"),
                 y="group",
                 title = paste("Normality test",p1,"Homogeneity of variance",p2,"kruskal.test",round(wtx3$`Pr(>F)`[1],3),sep = ":"))+
            geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+theme(legend.position="none")+
            stat_compare_means()
          p
          p=p + Mytheme
          p
          if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
          FileName <- paste(plotname ,name_i,"_kruskal.test_nosig_box", ".pdf", sep = "")
          ggsave(FileName, p, width = 8, height = 8)
        }


      }

    }



  }
}




