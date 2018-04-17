shinyServer(function(input,output)
{options(shiny.maxRequestSize=1024*1024^2)
  
ACC<-fluidPage(
    renderUI(
    sidebarPanel(selectInput("Y","Target Variable",choices =c("Select Target Variable",
                                                                     colnames(Data2())),
                      selected = "select Target Variable",selectize =F,size = ".005%"),
                 
     selectInput("M","Select Model",
         c("Please select Model","Logistic Regression","Bayesian Network","CHAID"),selected = "None"),

checkboxGroupInput("iv","Select Input Variables",c(colnames(Data2())))
                      )),
       renderUI(conditionalPanel(condition = "input.M=='Logistic Regression'",renderText(LogIT()),renderPlot(ROCC()))),
       renderUI(conditionalPanel(condition = "input.M=='Bayesian Network'",renderPlot(ROCC()))),
       renderUI(conditionalPanel(condition = "input.M=='CHAID'",renderPrint(CH1())))
       
       )

Dnload<-fluidPage(renderUI(
  sidebarPanel(checkboxGroupInput("MDLS", "Select Model ",c("Logistic regression","Bayesian network","CHAID"),inline = TRUE),
               checkboxGroupInput("RSLTS"," Select Results to download ",c("Reports","Visualizations","Anamolies"),inline = TRUE),
               radioButtons("Dformat","select the format",c("PDF","HTML","Word"))
  )))

S1<-fluidPage(renderPrint(str(Data())))
ST2<-fluidPage(renderUI(
  selectInput("var","Select Variable",c(paste(1:ncol(Data()),".",colnames(Data()))),width = "10%")),
  sidebarPanel(renderDataTable(SUM1())),
  mainPanel(renderPlot(SUMM()))
)
sampte<-reactiveTable(sample_n(Data(),(nrow(Data())*.30)))
samptr<-reactiveTable(sample_n(Data(),(nrow(Data())*.70)))

 InputVariables<-reactive({
  if(input$Y=="Select Target Variable"){return(NULL)}
  colnames(Data2())[!colnames(Data2())==colnames(Data2()[input$Y])]})
 TVar<-reactive({
    
  })


  
 Data<-reactive({
    Data1<-input$file;FILEPATH<-Data1$datapath;
    FILENAME<-Data1$name;H<-input$Header;S<-input$sep;
    if(is.null(input$file)){return(NULL)}
    if(endsWith(FILENAME,'.txt')==TRUE)
    {D<-read.csv(FILEPATH,header=H,sep=S);
    for(i in 1:ncol(D) )
    {if(is.factor(D[,i]))
    { D[,i]<-as.numeric(factor(D[,i]))}}
    return(D)}
    else if(endsWith(FILENAME,'.csv')==TRUE)
    {D<-read.csv(FILEPATH,header=H,sep=S);return(D)}
    else if(endsWith(FILENAME,'.xlsx')==TRUE)
    {D<-xlsx::read.xlsx(FILEPATH,1);return(D)}
    else if(endsWith(FILENAME,'.xls')==TRUE)
    {D<-import(FILEPATH,1);return(D)} 
    else if(endsWith(FILENAME,'.json')==TRUE)
    {D<- fromJSON(FILEPATH,header=H,sep=S);return(D)}
    else{print("NO FORMAT")}
  })
 
 accumulate_by<-function(dat,var){
   var<-lazyeval::f_eval(var,dat)
   lvls<-plotly:::getLevels(var)
   dats<-lapply(seq_along(lvls),function(x){
     cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
   })
   dplyr::bind_rows(dats)
 }
 
library(plotly)
 
fpl1<-reactive({
  
  d<-plt1%>%
    accumulate_by(~ds)
  fp1<-d %>%
    plot_ly(
      x=~ds,
      y=~Price,
      color=~class,
      frame=~frame,
      split=~ds1,
      type="scatter",
      mode="lines",
      #hoverinfo=text,
      #text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(fp1)
  
  
  
  
}) 
 
   
pl1<-reactive({
  accumulate_by<-function(dat,var){
    var<-lazyeval::f_eval(var,dat)
    lvls<-plotly:::getLevels(var)
    dats<-lapply(seq_along(lvls),function(x){
      cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  data<-Data()
  str(data)
  sapply(data,function(x) sum(is.na(x)))
  D<-strptime(data$Date,format = "%m/%d/%Y")
  
  D1<-as.numeric(as.character(data$Close.SBI))
  D2<-as.numeric(as.character(data$Close.Rcom))
  D3<-as.numeric(as.character(data$Close.kcp))
  D4<-as.numeric(as.character(data$Close.HDFC))
  D5<-as.numeric(as.character(data$Close.HATH))
  
  
  Data1<-data.frame(D,D1,D2,D3,D4,D5)
  #Data1<-Data1[-1,]
  names(Data1)<-c("timeStamp","close.SBI","close.Rcom","close.kcp","close.HDFC","close.HATH")
  #Data1$timeStamp
  TS1<-ts(Data1$close.SBI)
  TS2<-ts(Data1$close.Rcom)
  TS3<-ts(Data1$close.kcp)
  TS4<-ts(Data1$close.HDFC)
  TS5<-ts(Data1$close.HATH)
  
  
  library(forecast)
  fore1<-arima(TS1,order = c(4,1,4))
  fore2<-arima(TS2,order = c(4,1,4))
  fore3<-arima(TS3,order = c(4,1,4))
  fore4<-arima(TS4,order = c(4,1,4))
  fore5<-arima(TS5,order = c(2,1,2))
  
  PR1<-forecast(fore1,h=31)
  PR2<-forecast(fore2,h=31)
  PR3<-forecast(fore3,h=31)
  PR4<-forecast(fore4,h=31)
  PR5<-forecast(fore5,h=31)
  
  
  A<-cbind(unclass(PR1$mean),unclass(PR2$mean),unclass(PR3$mean),unclass(PR4$mean),unclass(PR5$mean))
  head(A)
  
  change<-NULL
  change1<-NULL
  for(i in 1:ncol(A)){
    for(j in 1:(nrow(A)-1)){
      change<-paste(round((((A[j+1,i]-A[j,i])/A[j,i])*100),digits = 2),"%")
      change1<-rbind(change1,change)
    }
  }
  ind<-1:(nrow(change1)/ncol(A))
  changeSBI<-change1[ind]
  changeRcom<-change1[ind+30]
  changekcp<-change1[ind+60]
  changeHDFC<-change1[ind+90]
  changeHATH<-change1[ind+120]
  
  library(prophet)
  Pr1<-Data1[,c(1,2)]
  names(Pr1)<-c("ds","y")
  prop<-prophet(df=Pr1)
  B<-make_future_dataframe(prop,periods = 31)
  
  Dtabel<-data.frame(as.Date(B[250:279,1]),changeSBI,changeRcom,changekcp,changeHDFC,changeHATH)
  names(Dtabel)<-c("Date","SBI","Rcom","kcp","HDFC","HATH")
  #View(t(Dtabel))
  Finaldata<<-t(Dtabel)
  Finaldata1<<-data.frame(Company=rownames(Finaldata),Finaldata)
 
  
  
  V1<-append(unclass(TS1),unclass(PR1$mean))
  V2<-append(unclass(TS2),unclass(PR2$mean))
  V3<-append(unclass(TS3),unclass(PR3$mean))
  V4<-append(unclass(TS4),unclass(PR4$mean))
  V5<-append(unclass(TS5),unclass(PR5$mean))
  class<-c(rep(1,length(TS1)),rep(2,length(PR1$mean)))
  B<-as.Date(B[,1])
  
  Ap1<-rep(1:length(V1),5)
  Ap2<-c(rep(1,280),rep(2,280),rep(3,280),rep(4,280),rep(5,280))
  class1<-c(c(rep(1,length(TS1)),rep(2,length(PR1$mean))),c(rep(1,length(TS1)),rep(2,length(PR1$mean))),
            c(rep(1,length(TS1)),rep(2,length(PR1$mean))),c(rep(1,length(TS1)),rep(2,length(PR1$mean))),
            c(rep(1,length(TS1)),rep(2,length(PR1$mean))))
  
  VV<-c(V1,V2,V3,V4,V5)
  
  plt1<<-data.frame(ds=Ap1,ds1=as.character(Ap2),Price=VV,frame=(1:length(Ap1)),class=class1)
  
  
  plt<<-data.frame(ds=1:length(V1),TS=as.character(B),
                   PriceSBI=V1,
                   PriceRcom=V2,
                   PriceKcp=V3,
                   PriceHDFC=V4,
                   PriceHath=V5,
                   class=class,frame=(1:length(V1)),
                   changeSBI=append(rep(0,250),changeSBI),
                   changeRcom=append(rep(0,250),changeRcom),
                   changekcp=append(rep(0,250),changekcp),
                   changeHDFC=append(rep(0,250),changeHDFC),
                   changeHATH=append(rep(0,250),changeHATH)
  )
  
  plt$TS<-as.character(plt$TS)
  
  
  
  library(plotly)
  
  d<-plt%>%
    accumulate_by(~ds)
  p1<-d %>%
    plot_ly(
      x=~ds,
      y=~PriceSBI,
      frame=~frame,
      color=~class,
      #type="scatter",
      mode="lines",
      hoverinfo=text,
      text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(p1)
  
  })


pl2<-reactive({
  
  accumulate_by<-function(dat,var){
    var<-lazyeval::f_eval(var,dat)
    lvls<-plotly:::getLevels(var)
    dats<-lapply(seq_along(lvls),function(x){
      cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  d<-plt%>%
    accumulate_by(~ds)
  p2<-d %>%
    plot_ly(
      x=~ds,
      y=~PriceRcom,
      frame=~frame,
      color=~class,
      #type="scatter",
      mode="lines",
      hoverinfo=text,
      text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(p2)
  
})

pl3<-reactive({
  
  accumulate_by<-function(dat,var){
    var<-lazyeval::f_eval(var,dat)
    lvls<-plotly:::getLevels(var)
    dats<-lapply(seq_along(lvls),function(x){
      cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  d<-plt%>%
    accumulate_by(~ds)
  p3<-d %>%
    plot_ly(
      x=~ds,
      y=~PriceKcp,
      frame=~frame,
      color=~class,
      #type="scatter",
      mode="lines",
      hoverinfo=text,
      text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(p3)
})


pl4<-reactive({
  
  accumulate_by<-function(dat,var){
    var<-lazyeval::f_eval(var,dat)
    lvls<-plotly:::getLevels(var)
    dats<-lapply(seq_along(lvls),function(x){
      cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  d<-plt%>%
    accumulate_by(~ds)
  p4<-d %>%
    plot_ly(
      x=~ds,
      y=~PriceHDFC,
      frame=~frame,
      color=~class,
      #type="scatter",
      mode="lines",
      hoverinfo=text,
      text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(p4)
  
})

pl5<-reactive({
  
  accumulate_by<-function(dat,var){
    var<-lazyeval::f_eval(var,dat)
    lvls<-plotly:::getLevels(var)
    dats<-lapply(seq_along(lvls),function(x){
      cbind(dat[var %in% lvls[seq(1,x)],],frame=lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  d<-plt%>%
    accumulate_by(~ds)
  p5<-d %>%
    plot_ly(
      x=~ds,
      y=~PriceHath,
      frame=~frame,
      color=~class,
      #type="scatter",
      mode="lines",
      hoverinfo=text,
      text=~paste("Date:",TS,"<br>","Price: Rs",PriceSBI,"<br>"),
      line=list(simplyfy=F))%>%
    animation_opts(
      frame=100,
      transition=10,
      redraw=FALSE
    )%>%
    animation_slider(hide=F
    )%>%
    animation_button(
      x=1,xanchor="right",y=0,yanchor="bottom")
  
  
  return(p5)
  
  
})
 















 

 
output$std.dev<-renderUI({fluidPage(mainPanel(
        fluidRow(column(6,renderPlotly(pl1())),  
                 column(6,renderPlotly(pl2()))),br(),
        fluidRow(column(6,renderPlotly(pl3())),  
                 column(6,renderPlotly(pl4()))),br(),
        fluidRow(column(6,renderPlotly(pl5()))))  
                       
      )
        
        #splitLayout(renderPlotly(pl1()),renderPlotly(pl1()))))
    })




output$Amy<-renderUI({fluidPage(mainPanel(renderPlotly(fpl1())),
                                fluidRow(renderDataTable(Finaldata1)))})
  
output$DataTable<-renderDataTable({Data()})
output$ST2<-renderUI({S1})
output$S1<-renderUI({ST2})
output$AutoClassifer<-renderUI({ACC})
output$rte<-renderUI({Dnload})
  })