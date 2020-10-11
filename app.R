library('shiny')
library('shinydashboard')
library('DT')
library('plotly')
library('dplyr')
library('ggplot2')

df <- read.csv('vgsalesss.csv',header = T)
attach(df)

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(width = 130,sidebarMenu(
    menuItem('Data',badgeLabel = 'Hello',badgeColor = 'aqua',icon = icon('home'),tabName = 'data'),
    menuItem('Visualisation',icon = icon('line-chart'),tabName = 'visualise')
  )),
  dashboardBody(tags$head(tags$style('h2 {text-align : center;color : white; padding : 20px;background-color : black;}')),tabItems(
    tabItem('data',tags$h2('VIDEO GAME SALES 1980-2016'),fluidRow(column(12,
     box(width = 550,height = 600,DT::dataTableOutput('dtag',height = 300),status = 'primary',background = 'aqua')))
    ),
    tabItem('visualise',
            fluidRow(
              column(4,
                     fluidRow(column(12,box(title = 'Sales Distribution Filter',solidHeader = T,status = 'info',height = 255,width = 200,
                                            selectInput('histyear','YEAR',choices = 1980:2016),
                                            selectInput('histgenre','GENRE',choices = data.frame(table(df$Genre))$Var1)))),
                     fluidRow(column(12,wellPanel(
                       sliderInput('bins','BINS',min = 5,max = 30,value = 30)
                     )))),
              column(4,tabBox(height = 400,width = 350,
                              tabPanel('All Games',plotlyOutput('pie',height = 350,width = 350)),
                              tabPanel('Games By Year',
                                       selectInput('select1','YEAR',choices = 1980:2016),
                                       plotlyOutput('pie2',height = 270,width = 350)),
                              tabPanel('Games By Publisher',
                                       selectInput('select2','Publisher',choices = data.frame(table(df$Publisher))$Var1),
                                       plotlyOutput('pie3',height = 270,width = 350)))),
              column(4,
                     fluidRow(column(12,box(height = 120,width = 200,title = 'Filter for BarPlot & Boxplot',solidHeader = T,status = 'info',
                                            selectInput('yearbarbox','YEAR',choices = c('All','1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016'))))
                        ),
                     fluidRow(column(12,box(height = 273,width = 200,status = 'info',solidHeader = T,title = 'Total Sales By Genre',collapsible = T,
                                            plotlyOutput('barsum',height = 200)
                     ))))
            ),
            fluidRow(
              column(7,box(title = 'Sales Distribution',status = 'info',solidHeader = T,height = 250,width = 400,plotlyOutput('hist',height = 170)
              )),
              column(5,box(height = 250,width = 400,status = 'info',title = 'Sales By Genre Boxplot',solidHeader = T,collapsible = T,
                           plotlyOutput('box1',height = 195))))))
  ))

    
server <- shinyServer(function(input,output){
  
  output$hist <- renderPlotly({
    ggplotly(ggplot(df[(Year == input$histyear) & (Genre == input$histgenre),],aes(Global_Sales)) + geom_histogram(bins = input$bins) )
  })
  
  output$box1 <- renderPlotly({
    if (input$yearbarbox == 'All'){
      ggplotly(ggplot(df,aes(Genre,Global_Sales)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)))
    }else{
      ggplotly(ggplot(df[Year == input$yearbarbox,],aes(Genre,Global_Sales)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)))
    }
    
  })
  
  output$barsum <- renderPlotly({
    if (input$yearbarbox == 'All'){
      aer = data.frame(tapply(Global_Sales,INDEX = Genre,FUN = sum))
      colnames(aer) = 'Sum'
      ggplotly(ggplot(aer,aes(row.names(aer),Sum)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) + labs(x = 'Genre',y = 'Sum') ) 
    }else{
      hj = df[Year == input$yearbarbox,]
      aer = data.frame(with(hj,tapply(Global_Sales,INDEX = Genre,FUN = sum)))
      colnames(aer) = 'Sum'
      ggplotly(ggplot(aer,aes(row.names(aer),Sum)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) + labs(x = 'Genre',y = 'Sum') )
    }

  })
  
  byyeaLabell <- reactive({
    dfa <- data.frame(table(df[Publisher == input$select2,]$Genre))$Var1
    dfa
  })
  
  byyeaValuess <- reactive({
    dfaa <- data.frame(table(df[Publisher == input$select2,]$Genre))$Freq
    dfaa
  })
  
  output$pie3 <- renderPlotly({
    plot_ly(labels = byyeaLabell(),values = byyeaValuess(),type = 'pie')
    
  })
  
  
  byyeaLabel <- reactive({
    dfaz <- data.frame(table(df[Year == input$select1,]$Genre))$Var1
    dfaz
  })
  
  byyeaValues <- reactive({
    dfaaz <- data.frame(table(df[Year == input$select1,]$Genre))$Freq
    dfaaz
  })
  
  output$pie2 <- renderPlotly({
    plot_ly(labels = byyeaLabel(),values = byyeaValues(),type = 'pie')
    
  })
  
  byyeaLabe <- reactive({
    dfaq <- data.frame(table(select(df,Genre)))$Var1
    dfaq
  })
  
  byyeaValue <- reactive({
    dfaaq <- data.frame(table(select(df,Genre)))$Freq
    dfaaq
  })
  
  output$pie <- renderPlotly({
    plot_ly(labels = byyeaLabe(),values = byyeaValue(),type = 'pie')
    
  })
 
  
  output$dtag <- DT::renderDataTable(df,extensions = 'Buttons',options = list(dom = 'Bfrtip',buttons = list('pdf','excel')))
  
})

shinyApp(ui,server)