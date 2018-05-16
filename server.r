library(arules)
library(arulesViz)
library(RColorBrewer)
library(broom)
library(shiny)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2)
data(Groceries)
data(cars)
server <- function(input, output){
  # 关联分析
  aprioriResult <- reactive({
    if(!is.null(input$associationAnalysisFile)){
      
      tempData <- read.csv(input$associationAnalysisFile$datapath)
      
      dlist <- apply(tempData,1,function(x) colnames(tempData)[unlist(x,use.names=F)])
      apioriData <- as(dlist,"transactions")
      
    }
    else{
      apioriData=Groceries
    }
    aprioriRules <- apriori(data = apioriData,
                            parameter = list(support = input$support, 
                                             confidence = input$confidence,
                                             minlen =input$minlen,target="rules"))
  })
  
  output$associationAnalysisFomula <- renderUI({
    # ex <- expression("confidence(A=>B)=P(A|B)")
    # ex1<-expression("=support_count(A U B)/support_count(A)")
    # par(mar = rep(0.1,4),cex=1)
    # plot.new()
    # title<-"计算置信度公式"
    # plot.window(c(0, 2), c(0, 1))
    # text(0.2,1,title)
    # text(0.4, 0.8, ex) 
    # text(1.33, 0.6, ex1)
    # context<-"<img src=gongshi.jpg />"
    # HTML(context)
    img(src="apriori.png")
    
  })
  output$associationAnalysisResult1 <- renderPlot({
    
     plot(aprioriResult(), measure="confidence", method="graph",
               control=list(type="items"),   
              shading = "lift",max=500)
   
  })
  output$associationAnalysisDownloadData <- downloadHandler(

    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
     
      data<-as( aprioriResult(),"data.frame")
      write.csv(data,file)
    }
  )
  output$associationAnalysisResource <- renderUI({
    context<-"
    #加载数据<br>
    data(Groceries)<br>
    #获取关联规则  连接步和剪枝步<br>
    aprioriRules <- apriori(data = Groceries,<br>
    parameter = list(support = input$support, <br>
    confidence = input$confidence,<br>
    minlen =input$minlen,target=\"rules\"))<br>
    #作图<br>
    plot(aprioriRules, measure=\"confidence\", method=\"graph\",<br>
    control=list(type=\"items\"),   <br>
    shading = \"lift\")"
    HTML(context)
  })
  
  # 线性回归分析
  lmResult <- reactive({
  
    yy=lm(lmFormula(),data=lmData())
    })
   lmData<- reactive({
     if(!is.null(input$linearRegressionAnalysisFile)){
       lmData <- read.csv(input$linearRegressionAnalysisFile$datapath)
     }
     else{
       lmData=cars
     }
   })
   
   lmFormula<-reactive({
     if(!is.null(input$linearRegressionAnalysisFile)){
       linearRegressionAnalysisFomula<-input$linearRegressionAnalysisFomula
       Fomula<-as.formula(linearRegressionAnalysisFomula)
     }
     else{
       Fomula<-as.formula("dist~speed")
     }
     
     })
   
  output$linearRegressionAnalysisFomula <- renderUI({
  
    context<-"<img src=lm-1.png/>
              <br><img src=lm-2.png/>
              <br><img src=lm-3.png/>"
    HTML(context)
    
  })
  output$linearRegressionAnalysisResult1 <- renderPlot({
       plot(data=lmData(),lmFormula())
       abline(lmResult())
  })
  
  output$linearRegressionAnalysisResult2 <- renderPrint({
    summary(lmResult())
  })
  output$linearRegressionAnalysisDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      write.csv(tidy(summary(lmResult())),file)
    }
  )
  #valid result
  output$linearRegressionAnalysisValidResult <- renderPlot({
    par(mfrow=c(2,2))
    plot(lmResult(),which=c(1:4))
  })
  
  output$linearRegressionAnalysisResource <- renderUI({
    context<-"
    #加载数据<br>
    data(car)<br>
    #生成回归模型<br>
    model=lm(dist~speed,data=cars)<br>
    #作图<br>
    plot(cars$speed,cars$dist)<br>
    abline(model)<br>
    #验证 残差图 QQ图 cook距离图 标准化残差方根散点图<br>
    par(mfrow=c(2,2))<br>
    plot(model,which=c(1:4))<br>
    "
    HTML(context)
  })
  
  
  # k-means
  library(fpc)
  data(iris)
  kmeansResult <- reactive({
    set.seed(252964)
    kmeansResult<-kmeans(na.omit(kmeansData()),input$kmeans.centers,input$kmeans.iter.max,
                         input$kmeans.nstart,input$kmeans.algorithm)
  })
  
  kmeansData <- reactive({
    if(!is.null(input$kmeansFile)){
      kmeansData <- read.csv(input$kmeansFile$datapath)
      rownames(kmeansData) <- kmeansData[,1]
      
      kmeansData<-kmeansData[,c(2:length(kmeansData))]
    }
    else{
      kmeansData=iris[,c(1:4)]
    }
  })
  output$kmeansFomula <- renderUI({
    
    context<-"<img src=kmeans-1.png/>"
    HTML(context)
    
  })
  output$kmeansResult1 <- renderPlot({
   
    plotcluster(na.omit(kmeansData()),(kmeansResult())$cluster) 
  })
  
  output$kmeansDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv((kmeansResult())$cluster,file)
    }
  )
  
  output$kmeansResource <- renderUI({
    context<-"
    #加载R包<br>
    library(fpc)<br>
    #加载数据<br>
    data(iris)<br>
    #处理数据<br>
    kmeansData=iris[,c(1:4)]<br>
    # 设置随机数种子，保证每次的结果一致<br>
     set.seed(252964)<br>
    # 使用R语言内置函数聚类<br>
     kmeansResult<-kmeans(na.omit(kmeansData))<br>
    #作图<br>
     plotcluster(na.omit(kmeansData),kmeansResult$cluster)
    "
    HTML(context)
  })

  # dbscan
  dbscanResult <- reactive({
    if(input$dbscan.scale=="Y")
      dbscan.scale=T
    else
      dbscan.scale=F
    
    dbscanResult<-dbscan(dbscanData(),input$dbscan.eps,input$dbscan.MinPts,scale=dbscan.scale,method=input$dbscan.method)
   
  })
  
  dbscanData <- reactive({
    set.seed(664554)
    if(!is.null(input$dbscanFile)){
      dbscanData <- read.csv(input$dbscanFile$datapath)
      rownames(dbscanData) <- dbscanData[,1]
      dbscanData<-dbscanData[,c(2:length(dbscanData))]
    }
    else{
      dbscanData=iris[,c(1:4)]
    }
  })
  output$dbscanFomula <- renderUI({
    
    context<-"<img src=dbscan-1.png/>"
    HTML(context)
    
  })
  output$dbscanResult1 <- renderPlot({
    
    plotcluster(na.omit(dbscanData()),(dbscanResult())$cluster) 
  })
  
  output$dbscanDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv((dbscanResult())$cluster,file)
    }
  )
  
  output$dbscanResource <- renderUI({
    context<-"
    #加载数据<br>
    data(iris)<br>
    #处理数据<br>
    dbscanData=iris[,c(1:4)]<br>
    # 设置随机数种子，保证每次的结果一致<br>
    set.seed(252964)<br>
    # 使用R语言内置函数聚类<br>
   dbscanResult<-dbscan(dbscanData,1,5,scale=F,method=\"raw\")<br>
    #作图<br>
    plotcluster(na.omit(dbscanData),dbscanResult$cluster)
    "
    HTML(context)
  })
  
  # hclust
  hclustResult <- reactive({
    hclustData=hclustData()
    if(input$hclust.scale=="Y"){
      hclustData<-scale(hclustData)
    }
    if(input$hclust.dist.method=="minkowski")
    d<-dist(hclustData,method = input$hclust.dist.method,p=input$hclust.dist.p)
    else
      d<-dist(hclustData,method = input$hclust.dist.method)
    
    hclustResult<-hclust(d, method = input$hclust.method)
  })
  
  hclustData <- reactive({
    set.seed(664554)
    if(!is.null(input$hclustFile)){
      hclustData <- read.csv(input$hclustFile$datapath)
      rownames(hclustData) <- hclustData[,1]
      hclustData<-hclustData[,-1]
    }
    else{
      hclustData=iris[,c(1:4)]
    }
  })
  output$hclustResult1 <- renderPlot({
    plot(hclustResult())                           #对结果画图 如图1
    rect.hclust(hclustResult(),k=input$hclust.centers)                   #用矩形画出分为3类的区域
  })
  
  output$hclustDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      result<-as.data.frame(cutree(hclustResult(),k=input$hclust.centers))
      write.csv(result,file)
    }
  )
  
  output$hclustResource <- renderUI({
    context<-"
    #加载数据<br>
    data(iris)<br>
    #处理数据<br>
    hclustData=iris[,c(1:4)]<br>
    # 设置随机数种子，保证每次的结果一致<br>
    set.seed(252964)<br>
    # 使用R语言内置函数聚类<br>
    hclustResult<-hclust(d, method = \"single\")<br>
    #作图<br>
    plot(hclustResult)<br>
    rect.hclust(hclustResult,k=3)  
    "
    HTML(context)
  })
  
  
  # diana
  library(cluster)
  dianaResult <- reactive({
    dianaData=dianaData()
    if(input$diana.scale=="Y"){
      dianaData<-scale(dianaData)
    }
    
    dianaResult<-diana(dianaData, metric= input$diana.metric)
  })
  
  dianaData <- reactive({
    set.seed(664554)
    if(!is.null(input$dianaFile)){
      dianaData <- read.csv(input$dianaFile$datapath)
      rownames(dianaData) <- dianaData[,1]
      dianaData<-dianaData[,-1]
    }
    else{
      dianaData=iris[,c(1:4)]
    }
  })
  output$dianaResult1 <- renderPlot({
    plot(dianaResult())                           #对结果画图
    rect.hclust(dianaResult(),k=input$diana.centers)#用矩形画出分为3类的区域
  })
  
  output$dianaDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      result<-as.data.frame(cutree(dianaResult(),k=input$diana.centers))
      write.csv(result,file)
    }
  )
  
  output$dianaResource <- renderUI({
    context<-"
    #加载R包<br>
    library(cluster)<br>
    #加载数据<br>
    data(iris)<br>
    #处理数据<br>
    dianaData=iris[,c(1:4)]<br>
    # 设置随机数种子，保证每次的结果一致<br>
    set.seed(252964)<br>
    # 使用R语言内置函数聚类<br>
    dianaResult<-diana(dianaData, metric= \"euclidean\")<br>
    #作图<br>
    plot(dianaResult)<br>
    rect.hclust(dianaResult,k=3)  
    "
    HTML(context)
  })
  
  # 贝叶斯分类
    library(caret)
  naiveBayesResult <- reactive({
    
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    laplace<-0
    usekernel<-FALSE
    if(input$naiveBayes.usekernel=="Y")
      usekernel<-TRUE
    if(input$naiveBayes.laplace=="Y")
      laplace<-1
    nbmodel <- train(x =naiveBayesData()[,-length(naiveBayesData())], y = naiveBayesData()[, length(naiveBayesData())],
                     method = "nb", trControl = fitControl,
                     tuneGrid = data.frame(.fL =laplace,.usekernel =usekernel,.adjust=FALSE))
  })
  
  naiveBayesData <- reactive({
    set.seed(664554)
    if(!is.null(input$naiveBayesFile)){
      naiveBayesData <- read.csv(input$naiveBayesFile$datapath)
      
    }
    else{
      naiveBayesData=iris
    }
  })
  output$naiveBayesFomula <- renderUI({
    
    context<-"<img src=naiveBayes-3.png/>"
    HTML(context)
    
  })
  output$naiveBayesResult1 <- renderPlot({
   
    # predicted_outcome <- predict(naiveBayesResult(), naiveBayesData()[,-length(naiveBayesData())])
     resampleHist(naiveBayesResult())
  })
  output$naiveBayesResult2 <- renderPrint({
    
     predicted_outcome <- predict(naiveBayesResult())
     confusionMatrix(predicted_outcome,naiveBayesData()[,length(naiveBayesData())])
  })

  output$naiveBayesDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      predicted_outcome<-NULL
      if(!is.null(input$naiveBayesFile1)){
        
        naiveBayesData <- read.csv(input$naiveBayesFile1$datapath)
        predicted_outcome <- predict(naiveBayesResult(), naiveBayesData)
        write.csv(predicted_outcome,file)
      }
      write.csv(predicted_outcome,file)
    }
  )
  
  output$naiveBayesResource <- renderUI({
    context<-"
    #加载R数据包<br>
    library(caret)<br>
    #加载数据<br>
    data(iris)<br>
    #k折交叉验证设置<br>
    fitControl <- trainControl(method = \"repeatedcv\", number = 10, repeats = 3)<br>
    #训练模型<br>
    nbmodel <- train(x =iris[,-5], y =iris[,5],method = \"nb\", trControl = fitControl,<br>
    tuneGrid = data.frame(.fL =laplace,.usekernel =usekernel,.adjust=FALSE))<br>
   #画出模型统计的采样分布<br>
   resampleHist(nbmodel)<br>
   #预测结果<br>
   predicted_outcome <- predict(nbmodel,iris[-5])<br>
   #展示混淆矩阵<br>
   confusionMatrix(predicted_outcome,iris[,5])
    "
    HTML(context)
  })
  # bp神经网络分类
  library(nnet)
  nnetResult <- reactive({
    nnetData<-nnetData()
    nnetData[,length(nnetData())]<-as.factor(nnetData[,length(nnetData())])
    formulaStr<-paste(names(nnetData)[length(nnetData)],"~.")
    nnet(as.formula(formulaStr), nnetData, size=as.numeric(input$nnet.size),maxit =as.numeric(input$nnet.maxit), 
         abstol = as.numeric(input$nnet.abstol),decay=as.numeric(input$nnet.decay))
  })
  
  nnetData <- reactive({
    set.seed(1234)
    if(!is.null(input$nnetFile)){
      nnetData <- read.csv(input$nnetFile$datapath)
      n<-nrow(nnetData)
      trainindex <- sample(1:n,0.8*n)
      trainset <- nnetData[trainindex,]
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      trainset <- iris[trainindex,]
    }
  })
  nnetTestData<-reactive({
    set.seed(1234)
    if(!is.null(input$nnetFile)){
      nnetData <- read.csv(input$nnetFile$datapath)
      n<-nrow(nnetData)
      trainindex <- sample(1:n,0.8*n)
      testset <- nnetData[-trainindex,]
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      testset <- iris[-trainindex,]
    }
  })
  
  output$nnetResult1 <- renderPrint({
    predicted_outcome <- predict(nnetResult(),nnetTestData(),type="class")
    predict.table = table(nnetTestData()[,length(nnetTestData())],predicted_outcome)
    confusionMatrix(predict.table)
  })
  
  output$nnetDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      predicted_outcome<-NULL
      if(!is.null(input$nnetFile1)){
        nnetData <- read.csv(input$nnetFile1$datapath)
        predicted_outcome <- predict(nnetResult(), nnetData,type="class")
        write.csv(predicted_outcome,file)
      }
      write.csv(predicted_outcome,file)
    }
  )
  
  output$nnetResource <- renderUI({
    context<-"
    #加载R数据包<br>
    library(nnet)<br>
    #加载数据<br>
    data(iris)<br>
    #分出测试集和训练集<br>
    n<-nrow(iris)<br>
    trainindex <- sample(1:n,0.9*n)<br>
    trainset <- data[trainindex,]<br>
    testset<-data[-trainindex,]<br>
    #训练模型<br>
     nn<-nnet(Species~.,trainset,size=3,entropy=FALSE,maxit =100, <br>
         abstol = 0.0001,decay=0)<br>
   #预测结果<br>
   predicted_outcome <- predict(nn,testset)<br>
   #展示混淆矩阵<br>
   predict.table = table(testset$Species,predicted_outcome)<br>
   confusionMatrix(predict.table)
    "
    HTML(context)
  })
  
  # knn k邻近算法
  library(kknn)
  knnResult <- reactive({
    knnData<-knnData()
    formulaStr<-paste(names(knnData)[length(knnData)],"~.")
    distance<-2
    if(input$knn.dist=="maximum"){
      distance<-Inf
    }
    else if(input$knn.dist=="manhattan"){
      distance<-1
    }
    else if(input$knn.dist=="euclidean"){
      distance<-2
    }
    else{
      distance<-input$knn.dist.p
    }
    scale<-TRUE
    if(input$knn.scale=="N")
      scale<-FALSE
    result<-kknn(as.formula(formulaStr),knnData,knnTestData(),k=as.numeric(input$knn.k),distance=distance,scale=scale)
    })
  
  knnData <- reactive({
    set.seed(12345)
    if(!is.null(input$knnFile)){
      knnData <- read.csv(input$knnFile$datapath)
     
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      trainset <- iris[trainindex,]
    }
  })
  knnTestData<-reactive({
    set.seed(12345)
    if(!is.null(input$knnFile1)){
      knnData <- read.csv(input$knnFile1$datapath)
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      testset <- iris[-trainindex,]
    }
  })
  
  output$knnResult1 <- renderUI({
    context<-"<strong><p style=\"color:red\">由于没有涉及抽象过程，kNN实际上并没有创建一个模型，所以无法评价模型</P></Strong>"
    HTML(context)
  })
  
  output$knnDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
        
        write.csv(fitted(knnResult()),file)
     
    }
    
  )
  
  output$knnResource <- renderUI({
    context<-"
    #加载R数据包<br>
    library(kknn)<br>
    #加载数据<br>
    data(iris)<br>
    #分出测试集和训练集<br>
    n<-nrow(iris)<br>
    trainindex <- sample(1:n,0.8*n)<br>
    trainset <- data[trainindex,]<br>
    testset<-data[-trainindex,]<br>
    #训练模型<br>
    knn<-kknn(Species.~,trainset,testset,k=7,distance=2)<br>
    #展示混淆矩阵<br>
    predict.table = table(testset$Species,fitted(knn))<br>
    confusionMatrix(predict.table)
    "
    HTML(context)
  })
  
  
  # 决策树 rpart
  library(rpart)
  library(rpart.plot)
  rpartResult <- reactive({
    rpartData<-rpartData()
    formulaStr<-paste(names(rpartData)[length(rpartData)],"~.")
    rpart.minsplit<-as.numeric(input$rpart.minsplit)   
    rpart.cp<-as.numeric(input$rpart.cp)
    rpart.maxdepth<-as.numeric(input$rpart.maxdepth)
    tc <- rpart.control(minsplit=rpart.minsplit,xval=3,cp=rpart.cp,maxdepth=rpart.maxdepth)
    rpart(as.formula(formulaStr),data=rpartData,method="class",parms=list(split=input$rpart.split),control=tc)
  })
  
  rpartData <- reactive({
    set.seed(664554)
    if(!is.null(input$rpartFile)){
      rpartData <- read.csv(input$rpartFile$datapath)
      n<-nrow(rpartData)
      trainindex <- sample(1:n,0.8*n)
      trainset <- rpartData[trainindex,]
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      trainset <- iris[trainindex,]
    }
  })
  rpartTestData <- reactive({
    set.seed(1234567)
    if(!is.null(input$rpartFile)){
      rpartData <- read.csv(input$rpartFile$datapath)
      n<-nrow(rpartData)
      trainindex <- sample(1:n,0.8*n)
      testset <- rpartData[-trainindex,]
    }
    else{
      n<-nrow(iris)
      trainindex <- sample(1:n,0.8*n)
      testset<- iris[-trainindex,]
    }
  })
  output$rpartResult1 <- renderPlot({
    
    rpart.plot(rpartResult())
  })
  output$rpartResult2 <- renderPrint({
    rpartResult()$cptable
  })
  
  output$rpartResult3 <- renderPrint({
    rpartTestData<-rpartTestData()
    predtree<-predict(rpartResult(),newdata=rpartTestData,type="class")   #利用预测集进行预测
    predict.table = table(rpartTestData[,length(rpartTestData)],predtree)
    confusionMatrix(predict.table)
    
  })
  
  output$rpartDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      predicted_outcome<-NULL
      if(!is.null(input$rpartFile1)){
        
        rpartData <- read.csv(input$rpartFile1$datapath)
        predicted_outcome <- predict(rpartResult(), rpartData,type="class")
        write.csv(predicted_outcome,file)
      }
      write.csv(predicted_outcome,file)
    }
  )
  
  output$rpartResource <- renderUI({
    context<-"
    #加载R数据包<br>
    library(rpart)<br>
    library(rpart.plot)<br>
    #加载数据<br>
    data(iris)<br>
    #分出测试集和训练集<br>
    n<-nrow(iris)<br>
    trainindex <- sample(1:n,0.8*n)<br>
    trainset <- data[trainindex,]<br>
    testset<-data[-trainindex,]<br>
    # 设置树的节点最少样本数 交叉验证次数 复杂系数 树的高度<br>
    tc <- rpart.control(minsplit=10,xval=3,cp=0.01,maxdepth=30)<br>
    #训练模型<br>    
    model<-rpart(Species~.,data=trainset,method=\"class\",parms=list(split=\"information\"),control=tc)<br>
    #画出决策树<br>
    rpart.plot(model)<br>
    #预测结果<br>
    predicted_outcome <- predict(model,testset,type=\"class\")<br>
    #展示混淆矩阵<br>
    confusionMatrix(predicted_outcome,testset[,5])
    "
    HTML(context)
  })
  
  
  
  # 文本出路 分词 统计词频 词云绘制
  library(jiebaR)
  library(wordcloud2)
  jiebaRResult <- reactive({
     browser()
    if(!is.null(input$jiebaRDictFile)){
      jiebaRDict <- input$jiebaRDictFile$datapath
    }
    else{
      jiebaRDict<-"data/金庸武侠招式.txt"
    }
    if(!is.null(input$jiebaRStopwordFile)){
      jiebaRStopword <- input$jiebaRStopwordFile$datapath
    }
    else{
      jiebaRStopword<- "data/stop_word1.txt"
    }
      #指定词语词频统计
      #user 自定义词典 
      wk=worker(stop_word=jiebaRStopword,user=jiebaRDict,write=FALSE)
      test1<-segment(jiebaRData(),wk)
      #关键词提取
      #keys= worker("keywords", topn = 10)
      #write.table(vector_keywords(test1,keys),file=paste("关键词",i,".txt"))
      #将结果变成数据框
      
      data <- as.data.frame(table(test1))
      #写列名
      colnames(data) = c("Word","freq")
      #排序结果
      ordFreq <- data[order(data$freq,decreasing = T),]
      shuaixuan<-ordFreq[which(ordFreq$freq >1),]
      
  })
  
  
  jiebaRData <- reactive({
    if(!is.null(input$jiebaRFile)){
      jiebaRData <- input$jiebaRFile$datapath
    }
    else{
      jiebaRData<-"data/test.txt"
    }
   
  })
  
  output$jiebaRResult1<- renderWordcloud2({
    
    wordcloud2(jiebaRResult())
  })

  output$jiebaRDownloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
       
        write.csv(jiebaRResult(),file)
    }
  )
  
  output$jiebaRResource <- renderUI({
    context<-"
      #加载R包<br/>
      library(jiebaR)<br/>
      library(wordcloud2)<br/>
      #指定词语词频统计<br/>
      #user 自定义词典   stop_word 停用词 type=mix 分词引擎<br/>
      wk=worker(stop_word=jiebaRStopword,user=jiebaRDict,write=FALSE)<br/>
      #分词<br/>
      words<-segment(jiebaRData,wk)<br/>
      #关键词提取<br/>
      #keys= worker(\"keywords\", topn = 10)<br/>
      #将结果变成数据框<br/>
      data <- as.data.frame(table(words))<br/>
      #写列名<br/>
      colnames(data) = c(\"Word\",\"freq\")<br/>
      #排序结果<br/>
      ordFreq <- data[order(data$freq,decreasing = T),]<br/>
      shuaixuan<-ordFreq[which(ordFreq$freq >1),]<br/>
      #画词云<br/>
      wordcloud2(shuaixuan)<br/>
    "
    HTML(context)
  })
  
  
  
  
  
  
  
  
}