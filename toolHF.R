rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions

library(shiny)
library(shinydashboard)
library(ggplot2)

source('tool functions.R')

#get data
grid1=read.csv('spat/fake data gridded precalc.csv',as.is=T)
hf=read.csv('spat/fake data hf coord.csv',as.is=T)
opt1=read.csv('spat/optimized hf coord.csv',as.is=T)
pop=read.csv('spat/fake data popcenter.csv',as.is=T)
pop$type="PopCen"
coef1=data.matrix(read.csv('spat/glm table coef.csv',as.is=T))



# Define UI 
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    radioButtons(inputId = "tipo", 
                 label = "Type:", 
                 choices = c("Population at risk" = "PAR",
                             "Prevalence" = "Prev")),
    radioButtons(inputId = "opcao", 
                 label = "Option:", 
                 choices = c("User-defined", "Optimized")),
    conditionalPanel(
      condition = "input.opcao == 'User-defined'",
      textInput("x", label="Y coordinates of proposed HF", 
                value='3.75',placeholder='0-1'),
      textInput("y", label="X coordinates of proposed HF", 
                value='11.25',placeholder='0-1')
    ),
    conditionalPanel(
      condition = "input.opcao == 'Optimized'",
      radioButtons(inputId = "nhf", 
                   label = "Number of new health facilities:", 
                   choices = c("1", "2","3"))
    )
  ),
  dashboardBody(
    h1("Determining the optimal spatial distribution of health facilities (HF)"),
    fluidRow(
      column(
        width = 8,
        box(
          title = "Effect of proposed HF",
          width = NULL,
          status = "primary",
          solidHeader = T,
          plotOutput("MalariaPlot", height = "600px")
        )
      ),
      column(
        width = 4,
        box(title = "Wealth distribution", width = NULL, plotOutput("wealthPlot"),
            collapsible = T, solidHeader = T),
        box(title = "Population density", width = NULL, plotOutput("popPlot"),
            collapsible = T, solidHeader = T),
        box(title = "Distance to HF", width = NULL, plotOutput("distPlot"),
            collapsible = T, solidHeader = T)
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  s.incid.base=sum(grid1$incid)
  
  outPlot <- reactive({
    var1='preval'; max1=1; nome='Prevalence'
    if (input$tipo=='Cases') {var1='incid';max1=2210; nome='Cases'}
    
    if (input$opcao=='User-defined'){
      tmp=unlist(strsplit(input$x,split=','))
      x.user=as.numeric(tmp); nx=length(x.user)
      tmp=unlist(strsplit(input$y,split=','))
      y.user=as.numeric(tmp); ny=length(y.user)
      xlab1=0.3
      ylab1=0.9
      
      grid2=get.incid.preval(coef1,grid1,x.hf.new=0.25,y.hf.new=0.75)
      grid2$zzz=grid2[,var1]
      
      
      if (nx==ny & nx!=0 & ny!=0){
        grid2=get.incid.preval(coef1,grid1,x.hf.new=x.user,y.hf.new=y.user)
        grid2$zzz=grid2[,var1]
        hf1=data.frame(x=c(hf$x,x.user),y=c(hf$y,y.user))
        hf1$type=ifelse(hf1$x==hf$x & hf1$y==hf$y, "HFExist", "HFNew")
      }
      
      hf1=rbind(hf1,pop)
      hf1$type=factor(hf1$type, levels = c("HFExist", "HFNew", "PopCen"))
      propCase=round(100*sum(grid2$incid)/s.incid.base,0)
      label1=paste("Proportion original cases = ",propCase,'%', sep='')
      label2=paste(100-propCase, '% of cases reduced', sep='')
      label=paste(label1, ', ', label2, sep = '')
    }
    
    if (input$opcao=='Optimized'){
      cond=as.numeric(input$nhf)==opt1$nhf
      grid2=get.incid.preval(coef1,grid1,x.hf.new=opt1$x[cond],y.hf.new=opt1$y[cond])
      hf1=data.frame(x=c(hf$x,opt1$x[cond]),y=c(hf$y,opt1$y[cond]),
                     type=c("HFExist", rep("HFNew", input$nhf)))
      hf1=rbind(hf1,pop)
      hf1$type=factor(hf1$type, levels = c("HFExist", "HFNew", "PopCen"))
      grid2$zzz=grid2[,var1]
      
      propCase=round(100*sum(grid2$incid)/s.incid.base,0)
      label1=paste("Proportion original cases = ",propCase,'%', sep='')
      label2=paste(100-propCase, '% of cases reduced', sep='')
      label=paste(label1, ', ', label2, sep = '')
      
      
    }
    # Basic frameworks for output plot
    res= ggplot() + 
      xlab('Longitude') + ylab("Latitude") + theme_bw(base_size = 11) +
      coord_fixed()
    
    # Main plot
    res1=res + 
      geom_tile(data = grid2, alpha = 0.8,aes(x = x, y = y,fill = zzz)) +
      scale_fill_gradient2(low = "cyan", mid = "red",high='purple',
                           limits=c(0,max1),midpoint=max1/2,
                           name = nome) + 
      geom_point(data = hf1, aes(x = x,y=y,shape=type),size=5) +
      scale_shape_manual(name="Point of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F) +
      ggtitle(label)
    
    # Wealth plot
    wealthPlot=res + 
      geom_tile(data = grid2, alpha = 0.8,aes(x = x, y = y,fill = wealth)) +
      scale_fill_gradient(low = "#ffffff", high = "#ff0000", limits = c(0, 1), name = '') + 
      geom_point(data = hf1, aes(x = x, y=y, shape=type),size=5,show.legend = F) +
      scale_shape_manual(name="Point of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F)
    
    # Pop plot
    popPlot=res + 
      geom_tile(data = grid2, alpha = 0.8,aes(x = x, y = y,fill = pop)) +
      scale_fill_gradient(low = "#ffffff", high = "#ff0000", name = '') + 
      geom_point(data = hf1, aes(x = x,y=y,shape=type),size=5, show.legend = F) +
      scale_shape_manual(name="Point of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F)
    
    # Dist plot
    distPlot=res + 
      geom_tile(data = grid2, alpha = 0.8,aes(x = x, y = y,fill = dist1)) +
      scale_fill_gradient(low = "#ffffff", high = "#ff0000", name = '') + 
      geom_point(data = hf1, aes(x = x,y=y,shape=type),size=5, show.legend = F) +
      scale_shape_manual(name="Point of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F)
    
    L <- list(res1, wealthPlot, popPlot, distPlot)
  })
  
  output$MalariaPlot <- renderPlot(outPlot()[[1]])
  output$wealthPlot <- renderPlot(outPlot()[[2]])
  output$popPlot <- renderPlot(outPlot()[[3]])
  output$distPlot <- renderPlot(outPlot()[[4]])
}

# Run the application 
shinyApp(ui = ui, server = server)