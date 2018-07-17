rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions

library(shiny)
library(shinydashboard)
library(ggplot2)

source('tool functions.R')

# grid and location data
grid1=read.csv('data/fake data gridded precalc.csv',as.is=T)
hf=read.csv('data/fake data hf coord.csv',as.is=T)
opt1=read.csv('data/optimized hf coord.csv',as.is=T)
pop=read.csv('data/fake data popcenter.csv',as.is=T)
pop$type="PopCen"

# model data
coef1=read.csv('data/glmer coeff.csv',as.is=T)
coef2=coef1[,'Estimate']
names(coef2)=coef1$X
stats=read.csv('data/glmer covariate stats.csv',as.is=T)
rownames(stats)=stats$X
grid1$dist_uc.sd=(grid1$dist_uc-stats['media','dist_UC'])/stats['sd1','dist_UC']

# Miscellaneous data
s.incid.base=sum(grid1$incid)
size.land=15

# Debug use
# input=list()
# input$x='0.5,0.2'
# input$x=''
# input$y='0.5,0.1'
# input$nhf=2
# input$tipo='Population at risk'
# input$opcao='User-defined'


# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = "Optimal Locations of Health Facilities (HF)",
                  titleWidth = 400),
  dashboardSidebar(
    radioButtons(inputId = "tipo", 
                 label = "Output variable:", 
                 choices = c("Population at risk", "Prevalence")),
    radioButtons(inputId = "opcao", 
                 label = "Location of proposed HF:", 
                 choices = c("User-defined", "Optimized")),
    conditionalPanel(
      condition = "input.opcao == 'User-defined'",
      textInput("x", label="X coordinates of proposed HF", 
                value='3.75',placeholder='0 - 15'),
      textInput("y", label="Y coordinates of proposed HF", 
                value='11.25',placeholder='0 - 15'),
      div("Use commas to specify more than one coordinate (e.g. 3.75, 5.25)", 
          class="form-group shiny-input-container")
    ),
    conditionalPanel(
      condition = "input.opcao == 'Optimized'",
      radioButtons(inputId = "nhf", 
                   label = "Number of new health facilities:", 
                   choices = c("1", "2","3"))
    )
  ),
  
  dashboardBody(
    p("This app is designed to demonstrate how the malaria risk factor analysis can
      be used to help determine the location of new health facilities (HF). Although
      there are several risk factors, we simplify the model to include only two 
      important distance based factors: the distance to nearest HF and to nearest 
      urban center. Here, users can explore how the placement of new HF(s) on a 
      hypothetical landscape can change the malaria prevalence and population at 
      risk based on the outcome of the statistical model and the population
      distribution."),
    p("The data used to estimate the model parameters is based on surveys conducted
      in northern Ghana between 2010 and 2013. Therefore, the relationship between 
      malaria prevalence and the predictors depicted here may not be applicable in 
      other localities. We also assume that their relationships are unchanged after 
      the addition of new HF."),
    fluidRow(
      column(
        width = 8,
        infoBoxOutput("riskReduce", width = NULL),
        uiOutput("primaryBox")
      ),
      column(
        width = 4,
        box(title = "Distance to nearest HF", width = NULL, 
            plotOutput("distPlot", height = "200px"),
            collapsible = T, solidHeader = T),
        box(title = "Distance to nearest urban center", width = NULL, 
            plotOutput("distucPlot", height = "200px"),
            collapsible = T, solidHeader = T),
        box(title = "Population density", width = NULL, 
            plotOutput("popPlot", height = "200px"),
            collapsible = T, solidHeader = T)
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  out.plot <- reactive({
    # Prevalence or incidence
    nome=input$tipo
    var1=ifelse(nome == 'Prevalence', 'preval', 'incid')
    
    # Calculate grid and hf data based on User-defined coordinates
    if (input$opcao=='User-defined'){
      # Convert coordinates with commas to vector
      tmp=unlist(strsplit(input$x,split=','))
      x.user=as.numeric(tmp); nx=length(x.user)
      tmp=unlist(strsplit(input$y,split=','))
      y.user=as.numeric(tmp); ny=length(y.user)
      
      # Get predicted prevalence and pop at risk
      grid.user=get.incid.preval(coef2, grid1, x.hf.new=0.25*size.land, 
                                 y.hf.new=0.75*size.land, stats)
      grid.user$zzz=grid.user[,var1]
      hf.user=hf
      hf.user$type="HFExist"
      
      # Check if there are new coordinates and equal no. of x and y
      # Then update incid and prev
      if (nx==ny & nx!=0 & ny!=0){
        grid.user=get.incid.preval(coef2, grid1, x.hf.new=x.user,
                                   y.hf.new=y.user, stats)
        grid.user$zzz=grid.user[,var1]
        hf.user=data.frame(x=c(hf$x, x.user), y=c(hf$y, y.user))
        hf.user$type=ifelse(hf.user$x==hf$x & hf.user$y==hf$y, "HFExist", "HFNew")
      }
    }
    
    if (input$opcao=='Optimized'){
      cond=as.numeric(input$nhf)==opt1$nhf
      grid.user=get.incid.preval(coef2, grid1, x.hf.new=opt1$x[cond],
                                 y.hf.new=opt1$y[cond], stats)
      grid.user$zzz=grid.user[,var1]
      hf.user=data.frame(x=c(hf$x, opt1$x[cond]),y=c(hf$y, opt1$y[cond]),
                     type=c("HFExist", rep("HFNew", input$nhf)))
    }
    
    # Make table of point of interests
    poi=rbind(hf.user,pop)
    poi$type=factor(poi$type, levels = c("HFExist", "HFNew", "PopCen"))
    
    # Calculate risk reduction
    s.incid.user=sum(grid.user$incid)
    
    # Basic frameworks for output plot
    res <- ggplot() + 
      xlab('X') + ylab("Y") + theme_bw(base_size = 11) +
      coord_fixed()
    
    # Main plot
    max1 <- ceiling(max(grid1[,var1]))
    plot.main=res + 
      geom_tile(data=grid.user, alpha=0.8, aes(x=x, y=y, fill=zzz)) +
      scale_fill_gradient2(low="cyan", mid="red", high='purple',
                           limits=c(0,max1), midpoint=max1/2, name=nome) + 
      geom_point(data=poi, aes(x=x, y=y, shape=type), size=5) +
      scale_shape_manual(name="Points of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F)
    
    # Sub plots template
    res1=res + 
      scale_fill_gradient(low='#ffffff', high='#ff0000', name='') +
      scale_shape_manual(name="Point of Interest", 
                         breaks=c("HFExist", "HFNew", "PopCen"),
                         labels=c("Existing HF", "Proposed HF", "Population Center"),
                         values=c(17, 2, 10), drop=F)
    # Wealth plot
    plot.distuc=res1 + 
      geom_tile(data=grid.user, alpha=0.8, aes(x=x, y=y, fill=dist_uc)) +
      geom_point(data=poi, aes(x=x, y=y, shape=type), size=4, show.legend=F)
    
    # Pop plot
    plot.pop=res1 + 
      geom_tile(data=grid.user, alpha=0.8, aes(x=x, y=y, fill=pop)) +
      geom_point(data=poi, aes(x=x, y=y, shape=type), size=4, show.legend=F)
    
    # Dist plot
    plot.dist=res1 + 
      geom_tile(data=grid.user, alpha=0.8, aes(x=x, y=y, fill=dist1)) +
      geom_point(data=poi, aes(x=x, y=y, shape=type), size=4, show.legend=F)
    
    L <- list(plot.main=plot.main, plot.distuc=plot.distuc, 
              plot.pop=plot.pop, plot.dist=plot.dist,
              s.incid.user=s.incid.user)
  })
  
  output$malariaPlot <- renderPlot(out.plot()$plot.main)
  
  output$distucPlot <- renderPlot(out.plot()$plot.distuc)
  
  output$popPlot <- renderPlot(out.plot()$plot.pop)
  
  output$distPlot <- renderPlot(out.plot()$plot.dist)
  
  output$riskReduce <- renderInfoBox({
    s.incid.user=out.plot()$s.incid.user
    prop.reduce=100*(s.incid.user-s.incid.base)/s.incid.base
    prop.reduce=round(prop.reduce)
    if (prop.reduce < 0) {
      icon <- icon("arrow-down")
      out.value <- paste("Prior to new HF: ", round(s.incid.base), HTML('<br>'), 
                         sep="")
      out.value <- paste(out.value, "After new HF: ", round(s.incid.user), 
                         " (", prop.reduce, "%)", sep="")
      out.value <- HTML(out.value)
    } else {
      icon <- icon("arrows-h")
      out.value <- paste("Prior to new HF: ", round(s.incid.base), HTML('<br>'), 
                         sep="")
      out.value <- paste(out.value, "After new HF: ", round(s.incid.user), 
                         " (Unchanged)", sep="")
      out.value <- HTML(out.value)
    }
    infoBox(
      title = "Total Population at risk",
      value = out.value,
      icon = icon
    )
  })
  
  output$primaryBox <- renderUI({
    box(title = paste("Predicted", input$tipo),
        width = NULL,
        status = "primary",
        solidHeader = T,
        plotOutput("malariaPlot", height = "600px"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)