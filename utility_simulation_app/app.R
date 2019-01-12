#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# setwd('~/github/consmap-prep/utility_simulation_app')
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(markdown)
  
  library(dplyr)
  library(DT)
  
  library(scales)
  library(RColorBrewer)
  library(ggplot2)
})
source('functions.R')
load('d.Rdata')

# Define UI for application that draws a histogram
ui = dashboardPage(
  
  dashboardHeader(
    title='Conservation Mapping'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        'Routing', tabName = 'routing', icon = icon('road')),
      menuItem(
        'Siting', tabName = 'siting', icon = icon('map-marker'), selected=T))),
    
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'siting',
        
        fluidRow( # boxes need to be put in a row (or column)
      
          box(
            title = 'Tradeoff Plot', status = 'primary', collapsible=T, width=6,
            plotOutput(
              'utility_plot', height = 500)),
          
          box(
            title = 'Controls', status = 'primary', collapsible=T,
            
            withMathJax(helpText(
              '$$
              u = a * W - (1 -a ) * B
              $$')),
            
            sliderInput(
              'a_range', 'Range of alpha:',
              min = 0, max = 1, value = c(0, 1), dragRange=T) ))),
      
      tabItem(
        tabName = 'routing',
        
        p('To be ported here: ', a(href='http://shiny.env.duke.edu/bbest/consmap/', 'shiny.env.duke.edu/bbest/consmap')))))
  )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  get_d_sum <- reactive({
    req(input$a_range)
    
    n = 11 # should be odd to get median
    d_sim = simulate_utility(
      d, x='B_ssi', y='I_usm', 
      a_vec = seq(input$a_range[1], input$a_range[2], length.out=n), 
      fxn=lazyeval::lazy( a * y - (1 - a) * x ),
      fxn_slope=lazyeval::lazy( -1*(1 - a)/a ), 
      fxn_intercept=lazyeval::lazy( u[u_max] / a ))
    #d_sim = simulate_utility(d, x='B_ssi', y='I_usm', a_vec = seq(0, 1, length.out=n))
    
    # summarize sites by average utility across alphas
    d_sum = d_sim %>%
      group_by(i, x, y) %>%
      summarize(
        u_avg = sum(u_sim)/n) %>%
      ungroup() %>%
      arrange(desc(u_avg)) %>%
      mutate(
        rank = row_number(),
        pct_rank = percent_rank(u_avg)) %>%
      left_join(
        d_sim %>%
          filter(u_max==T) %>%
          select(i, a_sim, u_max, u_slope, u_yintercept),
        by='i') %>%
      mutate(
        u_max = ifelse(is.na(u_max), F, T))
  })
   
  output$utility_plot <- renderPlot({
     
     d_sum = get_d_sum()
     
     # get ticks for x and y based on original values
     brks = c(0,0.25,0.5,0.75,1)
     lbls_B = sprintf('%0.3f', approx(x=d$x, y=d$B_ssi, xout=brks, rule=c(2,2))$y)
     lbls_I = sprintf('%0.1f', approx(x=d$y, y=d$I_usm, xout=brks, rule=c(2,2))$y)
     
     # summary plot
     # TODO: add contour around points
     ggplot(d_sum, aes(x, y, colour=u_avg)) +
       geom_point() +
       coord_equal() + 
       scale_x_continuous(
         name = 'Bird Sensitivity', breaks=brks, labels=lbls_B, trans='reverse') +
       scale_y_continuous(
         name = 'Wind Profitablity ($NPV)', breaks=brks, labels=lbls_I) +
       scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')) + 
       geom_text(aes(x, y, label=rank), data = filter(d_sum, rank <= 5), colour='purple', size=4, hjust=0, vjust=0) +
       with(filter(d_sum, a_sim == median(a_sim, na.rm=T)), geom_abline(slope = u_slope, intercept = u_yintercept, linetype=1)) + #, show_guide=T)) + # median a
       with(filter(d_sum, a_sim == min(a_sim, na.rm=T)),                                                                          # min a
            {if ( is.infinite(u_slope) ) geom_vline(                                     
              xintercept = x, linetype=3) else geom_abline(
                slope = u_slope, intercept = u_yintercept, linetype=3)}) +
       with(filter(d_sum, a_sim == max(a_sim, na.rm=T)),                                                                          # max a
            {if ( is.infinite(u_slope) ) geom_vline(                                     
              xintercept = x, linetype=3) else geom_abline(
                slope = u_slope, intercept = u_yintercept, linetype=3)}) +
       xlab('Bird Conservation') +
       ylab('Wind Profitability ($NPV)')

   })
})

# Run the application 
shinyApp(ui = ui, server = server)

