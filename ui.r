library(shinydashboard)
library(shiny)
library(wordcloud2)

dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(),
  dashboardBody()
)
ui <- dashboardPage(
  dashboardHeader(title = "大数据分析实训平台"),
  dashboardSidebar( 
    sidebarMenu( 
    menuItem("基础分析",icon = icon("dashboard"),
             menuSubItem("关联分析", tabName = "associationAnalysis"),
             menuSubItem("线性回归分析", tabName = "linearRegressionAnalysis")
             ), 
    menuItem("多元统计分析", icon =icon("dashboard"),
             menuItem("聚类分析", icon =icon("dashboard"),
                      menuSubItem("K-means", tabName = "K-means"),
                      menuSubItem("密度聚类", tabName = "dbscan"),
                      menuItem("层次聚类", icon =icon("dashboard"),
                               menuSubItem("凝聚聚类", tabName = "hclust"),
                               menuSubItem("分裂聚类", tabName = "diana")
                               )
                      ),
             menuItem("分类分析", icon =icon("dashboard"),
                      menuSubItem("朴素贝叶斯", tabName = "naiveBayes"),
                      menuSubItem("bp神经网络", tabName = "nnet"),
                      menuSubItem("k邻近(KNN)", tabName = "knn"),
                      menuSubItem("决策树(id3 和 cart)", tabName = "rpart")
                      
             ),
             menuSubItem("主成分分析", tabName = "princomp")
             ),
    menuItem("其他",icon = icon("dashboard"),
             menuSubItem("文本处理",tabName="jiebaR")
             )
  )),
  dashboardBody(
    tabItems(
      # First tab content 关联分析
      tabItem(tabName = "associationAnalysis",
        fluidRow(
          shinydashboard::box(title="数据准备区",fileInput("associationAnalysisFile", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),width=4),
          shinydashboard::box(title="公式展示区",htmlOutput("associationAnalysisFomula", height = 100))
        ),
        fluidRow(
          shinydashboard::box(title="参数调整区",
             shinydashboard::box(
                sliderInput("support", "支持度:", 0, 1, 0.006,step=0.001),width=4
              ),
             shinydashboard::box(
                sliderInput("confidence", "置信度:", 0, 1, 0.4,step=0.001),width=4
              ),
             shinydashboard::box(

                sliderInput("minlen", "集合最小数:", 0, 10, 2,step=1),width=4
              ),width=12
          )
          ),
      fluidRow(
        shinydashboard::box(title="结果展示区",
          shinydashboard::box(downloadLink('associationAnalysisDownloadData', 'DownloadResult')
               ,width = 3
            ),
            shinydashboard::box(plotOutput("associationAnalysisResult1"),width = 12
                )
            ,width=12)
      ),
      fluidRow(
        shinydashboard::box(title="核心代码展示区",
           shinydashboard::box(htmlOutput('associationAnalysisResource'),width = 12
            )
            ,width=12)
      )
    ),
    #second tab context 线性回归
    tabItem(tabName = "linearRegressionAnalysis",
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="数据准备区",fileInput("linearRegressionAnalysisFile", "Choose CSV File",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                ".csv")),width=12),
                shinydashboard::box(title="自定义公式", a("查看帮助",
                                     href="lmFomulaHelp.jpg",target="black"),
                    textInput("linearRegressionAnalysisFomula",label="输入公式",value="y~x"),
                    width=12)

              ,width=5),
              shinydashboard::box(title="公式展示区",htmlOutput("linearRegressionAnalysisFomula", width=7))
            ),
            fluidRow(
              shinydashboard::box(title="结果展示区",
                 shinydashboard::box(downloadLink('linearRegressionAnalysisDownloadData', 'DownloadResult')
                      ,width = 3
                  ),
                  shinydashboard::box(plotOutput("linearRegressionAnalysisResult1"),width = 12
                  ),
                  shinydashboard::box(verbatimTextOutput("linearRegressionAnalysisResult2"),width = 12
                  )
                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="验证结果",
                 shinydashboard::box(plotOutput("linearRegressionAnalysisValidResult"),width = 12
                  )
                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                shinydashboard::box(htmlOutput('linearRegressionAnalysisResource'),width = 12
                  )
                  ,width=12)
            )
     ),
    
    #third tab context k-means
    tabItem(tabName = "K-means",
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="数据准备区",fileInput("kmeansFile", "Choose CSV File 第一行为名称或序列",
                                                            multiple = TRUE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),width=12)
                ,width=5),
              shinydashboard::box(title="公式展示区",htmlOutput("kmeansFomula", width=7))
            ),
            
            fluidRow(
            shinydashboard::box(title="参数调整区",
                                shinydashboard::box(
                                  sliderInput("kmeans.iter.max", "最大迭代数:", 1, 100, 10,step=1),width=6
                                ),
                                shinydashboard::box(
                                  sliderInput("kmeans.nstart", "起始聚类个数:", 1, 100, 1,step=1),width=6
                                ),
                                shinydashboard::box(
                                  textInput("kmeans.centers",label="期望聚类个数",value="3"),width=6
                                ),
                                shinydashboard::box(title="选择迭代算法", a("查看帮助",
                                                                     href="kmeansHelp.png",target="black"),
                                                    selectInput("kmeans.algorithm",label="选择迭代算法",c("Hartigan-Wong", "Lloyd", "Forgy",
                                                                                             "MacQueen"),selected = "Hartigan-Wong"),
                                                    width=6),
                                width=12)
            ),
            fluidRow(
              shinydashboard::box(title="结果展示区",
                                  shinydashboard::box(downloadLink('kmeansDownloadData', 'DownloadResult')
                                                      ,width = 3
                                  ),
                                  shinydashboard::box(plotOutput("kmeansResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('kmeansResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    # 4th tab  dbscan 
    tabItem(tabName = "dbscan",
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="数据准备区",fileInput("dbscanFile", "Choose CSV File 第一行为名称或序列",
                                                            multiple = TRUE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),width=12)
                ,width=5)
            ),
            fluidRow( 
              shinydashboard::box(title="参数调整区",
                                  shinydashboard::box(
                                    textInput("dbscan.eps", label="领域半径:",value="1"),width=6
                                  ),
                                  shinydashboard::box(
                                    textInput("dbscan.MinPts", label="最小包含点数:",value="5"),width=6
                                  ),
                                  shinydashboard::box(
                                    radioButtons("dbscan.scale",label="是否标准化",c("Y","N"),selected="N"),width=6
                                  ),
                                  shinydashboard::box(title="选择method", a("查看帮助",
                                                                        href="dbscanHelp.png",target="black"),
                                                      selectInput("dbscan.method",label="选择method",c("hybrid", "raw","dist")
                                                                                                     ,selected = "raw"),
                                                      width=6),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="结果展示区",
                                  shinydashboard::box(downloadLink('dbscanDownloadData', 'DownloadResult')
                                                      ,width = 3
                                  ),
                                  shinydashboard::box(plotOutput("dbscanResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="公式展示区",htmlOutput("dbscanFomula", width=12),width=12
                ),width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('dbscanResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    # 5th tab hclust 凝聚聚类分析
    tabItem(tabName = "hclust",
            fluidRow(
              
                shinydashboard::box(title="数据准备区",fileInput("hclustFile", "Choose CSV File 第一行为名称或序列",
                                                            multiple = TRUE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),width=5),
                  shinydashboard::box(
                    shinydashboard::box(title="公式展示区",a("点击查看",href="hclust.png",target="black"),width=12
                    ),width=7)
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区",
                                  
                                  shinydashboard::box(title="选择距离计算方法", a("查看帮助",
                                                                          href="hclustHelp1.png",target="black"),
                                                      selectInput("hclust.dist.method",label="选择距离计算方法",c("euclidean", "maximum",
                                                                                                                  "manhattan","canberra","minkowski")
                                                                  ,selected = "euclidean"),
                                                      width=6),
                                  shinydashboard::box(title="选择簇距离度量方法", a("查看帮助",
                                                                           href="hclustHelp2.png",target="black"),
                                                      selectInput("hclust.method",label="选择簇距离度量方法",c("ward.D", "ward.D2","single"
                                                                                                      ,"complete","average","mcquitty"
                                                                                                      ,"median","centroid")
                                                                  ,selected = "complete"),
                                                      width=6),
                                  shinydashboard::box(title="计算距离的p值(只有明式距离有效)",
                                    textInput("hclust.dist.p",label="计算距离的p值(为2的时候是欧式距离)",value="2"),width=6
                                  ),
                                  shinydashboard::box(
                                    radioButtons("hclust.scale",label="是否标准化",c("Y","N"),selected="N"),width=6
                                  ),
                                  shinydashboard::box(
                                    sliderInput("hclust.centers", "期望聚类个数:", 2, 149, 3,step=1),width=12
                                  ),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="结果展示区",
                                  shinydashboard::box(downloadLink('hclustDownloadData', 'DownloadResult')
                                                      ,width = 3
                                  ),
                                  shinydashboard::box(plotOutput("hclustResult1"),width = 12
                                  )
                                  ,width=12)
            ),
           
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('hclustResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    # 6th tab diana 分裂聚类分析
    tabItem(tabName = "diana",
            fluidRow(
              
              shinydashboard::box(title="数据准备区",fileInput("dianaFile", "Choose CSV File 第一行为名称或序列",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=5),
              shinydashboard::box(
                shinydashboard::box(title="公式展示区",a("点击查看",href="hclust.png",target="black"),width=12
                ),width=7)
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区",
                                  
                                  shinydashboard::box(title="选择距离计算方法", a("查看帮助",
                                                                          href="hclustHelp1.png",target="black"),
                                                      selectInput("diana.metric",label="选择距离计算方法",c("euclidean","manhattan"),
                                                                  selected = "euclidean"),
                                                      width=6),
                      
                                  shinydashboard::box(
                                    radioButtons("diana.scale",label="是否标准化",c("Y","N"),selected="N"),width=6
                                  ),
                                  shinydashboard::box(
                                    sliderInput("diana.centers", "期望聚类个数:", 2, 149, 3,step=1),width=12
                                  ),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="结果展示区",
                                  shinydashboard::box(downloadLink('dianaDownloadData', 'DownloadResult')
                                                      ,width = 3
                                  ),
                                  shinydashboard::box(plotOutput("dianaResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('dianaResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    
    # 7th tab naiveBayes 朴素贝叶斯算法分析
    tabItem(tabName = "naiveBayes",
            fluidRow(
              
              shinydashboard::box(title="数据准备区",fileInput("naiveBayesFile", "Choose CSV File 这是训练集，最后一行为分类的类别",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=5),
              shinydashboard::box(
                
                 title="公式展示区",htmlOutput("naiveBayesFomula", width=12)
                ,width=7)
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区-默认十折交叉验证",
                          
                                  shinydashboard::box(a("查看帮助",href="naiveBayesHelp.png",target="black"),
                                    radioButtons("naiveBayes.usekernel",label="是否核密度估计",c("Y","N"),selected="N"),width=6
                                  ),
                                  shinydashboard::box(
                                    radioButtons("naiveBayes.laplace",label="是否拉普拉斯修正",c("Y","N"),selected="N"),width=6
                                  ),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="分类器效果展示",
                                
                                  shinydashboard::box(plotOutput("naiveBayesResult1"),width = 12
                                  ),
                                  shinydashboard::box(verbatimTextOutput("naiveBayesResult2"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="预测区",
                                  
                                  shinydashboard::box(title="上传待预测数据",fileInput("naiveBayesFile1", "Choose CSV File 没有分类的数据",
                                                                              multiple = TRUE,
                                                                              accept = c("text/csv",
                                                                                         "text/comma-separated-values,text/plain",
                                                                                         ".csv")),width=12),
                                  shinydashboard::box(title="下载预测结果",downloadLink('naiveBayesDownloadData','DownloadResult')
                                                      ,width = 6)
              ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('naiveBayesResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    
    # 8th tab nnet bp神经网络
    tabItem(tabName = "nnet",
            fluidRow(

              shinydashboard::box(title="数据准备区",fileInput("nnetFile", "Choose CSV File 最后一行为类别",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=5),
              shinydashboard::box(
                shinydashboard::box(title="公式展示区",a("点击查看",href="nnet-formula.png",target="black"),width=12
                ),width=7)
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区",
                                   shinydashboard::box(title="隐节点的个数",
                                     textInput("nnet.size",label="隐节点的个数",value="2"),width=4
                                  ),
                                  shinydashboard::box(title="权重衰减参数", 
                                                      textInput("nnet.decay",label="权重衰减参数",value="0"),width=4
                                  ),
                                  shinydashboard::box(title="迭代次数",
                                                      textInput("nnet.maxit",label="迭代次数",value="100"),width=4
                                  ),
                                  shinydashboard::box(title="权重最小值", 
                                                      textInput("nnet.abstol",label="权重最小值",value="0.0001"),width=4
                                  ),
                                  shinydashboard::box(title="初始权值设置",
                                                      textInput("nnet.rang",label="初始权值设置",value="0.7"),width=4
                                  ),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="分类器效果展示",

                                  shinydashboard::box(verbatimTextOutput("nnetResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="预测区",

                                  shinydashboard::box(title="上传待预测数据",fileInput("nnetFile1", "Choose CSV File 没有分类的数据",
                                                                                multiple = TRUE,
                                                                                accept = c("text/csv",
                                                                                           "text/comma-separated-values,text/plain",
                                                                                           ".csv")),width=12),
                                  shinydashboard::box(title="下载预测结果",downloadLink('nnetDownloadData','DownloadResult')
                                                      ,width = 6)
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('nnetResource'),width = 12
                                  )
                                  ,width=12)
            )
     ),
    
    # 9th tab knn knn(k邻近)
    tabItem(tabName = "knn",
            fluidRow(
              shinydashboard::box(title="数据准备区",
              shinydashboard::box(title="训练数据准备区",fileInput("knnFile", "Choose CSV File 最后一行为类别",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=6),
              shinydashboard::box(title="上传待预测数据",fileInput("knnFile1", "Choose CSV File 没有分类的数据",
                                                            multiple = TRUE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),width=6),
              width=12)
            ),
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="公式展示区",a("点击查看",href="knnFormula.png",target="black"),width=12
                ),width=12)
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区",
                                  shinydashboard::box(title="k的值",
                                                      textInput("knn.k",label="k的值",value="7"),width=6
                                  ),
                                  shinydashboard::box(title="选择距离计算方法", a("查看帮助",
                                                                          href="hclustHelp1.png",target="black"),
                                                      selectInput("knn.dist",label="选择距离计算方法",c("euclidean", "maximum",
                                                                                                "manhattan","canberra","minkowski")
                                                                  ,selected = "euclidean"),
                                                      width=6),
                                  shinydashboard::box(
                                    radioButtons("knn.scale",label="是否标准化",c("Y","N"),selected="Y"),width=6
                                  ),
                                 
                                  shinydashboard::box(title="计算距离的p值(只有明式距离有效)",
                                                      textInput("knn.dist.p",label="计算距离的p值",value="2"),width=6
                                  ),
                                  
                                    width=12)
            ),
            fluidRow(
              shinydashboard::box(title="分类器效果展示",
                                  shinydashboard::box(title="下载预测结果",downloadLink('knnDownloadData','DownloadResult')
                                                      ,width = 3),
                                  shinydashboard::box(htmlOutput("knnResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('knnResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    
    # 10th tab raprt 决策树
    tabItem(tabName = "rpart",
            fluidRow(
              
              shinydashboard::box(title="数据准备区",fileInput("rpartFile", "Choose CSV File 这是训练集，最后一行为分类的类别",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=5),
              shinydashboard::box(
                title="公式展示区",a("点击查看",href="rpartFormula.png",target="black"),width=7)
            
            ),
            fluidRow(
              shinydashboard::box(title="参数调整区-默认十折交叉验证",

                                  shinydashboard::box(a("查看帮助",href="rpartHelp.png",target="black"),
                                                      selectInput("rpart.split",label="选择分类标准",c("information","gini")
                                                                  ,selected = "euclidean"),width=3
                                  ),
                                  shinydashboard::box(
                                    textInput("rpart.minsplit",label="每个节点中所含样本最小数",value="10"),width=3
                                  ),
                                  shinydashboard::box(
                                    textInput("rpart.maxdepth",label="决策树最大深度 ",value="30"),width=3
                                  ),
                                  shinydashboard::box(
                                    textInput("rpart.cp",label="complexity parameter ",value="0.01"),width=3
                                  ),
                                  width=12)
            ),
            fluidRow(
              shinydashboard::box(title="分类器效果展示",
                                  
                                  shinydashboard::box(plotOutput("rpartResult1"),width = 12
                                  ),
                                  shinydashboard::box(verbatimTextOutput("rpartResult2"),width = 12
                                  ),
                                  shinydashboard::box(verbatimTextOutput("rpartResult3"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="预测区",
                                  
                                  shinydashboard::box(title="上传待预测数据",fileInput("rpartFile1", "Choose CSV File 没有分类的数据",
                                                                                multiple = TRUE,
                                                                                accept = c("text/csv",
                                                                                           "text/comma-separated-values,text/plain",
                                                                                           ".csv")),width=12),
                                  shinydashboard::box(title="下载预测结果",downloadLink('rpartDownloadData','DownloadResult')
                                                      ,width = 6)
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('rpartResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    
    
    #11th 主成分分析
    tabItem(tabName = "princomp",
            fluidRow(
              
              shinydashboard::box(title="数据准备区",fileInput("princompFile", "Choose CSV File",
                                                          multiple = TRUE,
                                                          accept = c("text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv")),width=5),
              shinydashboard::box(
                title="公式展示区",a("点击查看",href="princompFormula.png",target="black"),width=7)
              
            ),
            fluidRow(
              shinydashboard::box(title="结果展示",
                                  shinydashboard::box(title="下载分析结果",downloadLink('princompDownloadData','DownloadResult')
                                                      ,width = 3),
                                  shinydashboard::box(title="下载预测结果",downloadLink('princompDownloadData1','DownloadResult')
                                                      ,width = 3),
                                  shinydashboard::box(verbatimTextOutput("princompResult1"),width = 12
                                  ),
                                  shinydashboard::box(plotOutput("princompResult2"),width = 12
                                  ),
                                  shinydashboard::box(plotOutput("princompResult3"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('princompResource'),width = 12
                                  )
                                  ,width=12)
            )
    ),
    
    #12th 文本处理 jiebaR
    tabItem(tabName = "jiebaR",
            fluidRow(
              shinydashboard::box(
                shinydashboard::box(title="上传待分词文本",fileInput("jiebaRFile", "Choose utf8 File",
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".txt")),width=4),
                shinydashboard::box(title="上传自定义词典",fileInput("jiebaRDictFile", "Choose utf8 File",
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".txt")),width=4),
                shinydashboard::box(title="上传停用词词典",fileInput("jiebaRStopwordFile", "Choose utf8 File",
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".txt")),width=4)
                ,width=12)
            ),

            fluidRow(
              shinydashboard::box(title="结果展示区",
                                  shinydashboard::box(downloadLink('jiebaRDownloadData', 'DownloadResult')
                                                      ,width = 3
                                  ),
                                  shinydashboard::box(wordcloud2Output("jiebaRResult1"),width = 12
                                  )
                                  ,width=12)
            ),
            fluidRow(
              shinydashboard::box(title="核心代码展示区",
                                  shinydashboard::box(htmlOutput('jiebaRResource'),width = 12
                                  )
                                  ,width=12)
            )
    )
    
    
    
    
    
    
    
    
    
    
  )
  
)
)


