\name{merge_ps}
\alias{merge_ps}
\title{merge_ps}
\usage{
    merge_ps (ps1,ps2,model = 1)
}
\arguments{
\item{ps1}{两个合并phyloseq对象中的一个，这个对象至少包含OTU表格，mapping文件，注释TAX文件;}

\item{ps2}{两个合并phyloseq对象中的第二个，注意有顺序，这个对象至少包含OTU表格，mapping文件，注释TAX文件;}

\item{model}{这是合并模式的选择，可以选择1，2，3，这三种模式;}

}
\value{
model 1,2,3.分别代表了三种合并方式，model =1 是取共有部分进行合并
；model =2 是保留x中的全部OTU，y中如果有其他OTU，全部舍弃，如果没有x中的这些otu使用0填充。
；model = 3,xy全部OTU进行合并。注意进化树可不能合并，必须重新运算，这里没有必要花费很长的时间来运算这个。
}

\description{
当前我的工作集中搭建在phyloeq文件上，为了将不同的OTU表格进行合并分析，我手动写了这个功能。基于phyloseq对象合并OTU表格，tax表格和mapping表格。

共有三种模式
有s三种模式，model =1 是取共有部分进行合并
model =2 是保留x中的全部OTU，y中如果有其他OTU，全部舍弃，如果没有x中的这些otu使用0填充。
注意进化树可不能合并，必须重新运算，这里没有必要花费很长的时间来运算这个。
model = 3,xy全部OTU进行合并

mapping文件：两个ps对象mapping文件必须包含SampleTypeeh和 Desciption列，其他内容随便。

}
\examples{
#example1
merge_ps(ps1,ps2,model = 1)
#example2
merge_ps(ps1,ps2,model = 2)

#example3
merge_ps(ps1,ps2,model = 3)

}

