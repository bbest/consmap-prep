# Load Libraries ----
library(plotly)
library(shiny)
library(htmlwidgets)

# Prepare Data ---
mtcars$name = rownames(mtcars)

ui <- fluidPage(
  plotlyOutput('myplot'),
  textOutput('hover')
)

renderPlotly2 <- function (expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {
    expr <- substitute(expr)
  }
  shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}

addHoverBehavior <- "function(el, x){
  el.on('plotly_hover', function(data){
    var infotext = data.points.map(function(d){
      console.log(d)
      return (d.data.name[d.pointNumber]+': x= '+d.x+', y= '+d.y.toPrecision(3));
    });
    console.log(infotext)
    Shiny.onInputChange('hover_data', infotext)
  })
}"



server <- function(input, output){
  output$hover <- renderText({
    input$hover_data
  })
  output$myplot <- renderPlotly2({
    p <- plot_ly(mtcars, x = mpg, y = wt, color = gear, name = name, mode = "markers")
    as.widget(p) %>% onRender(addHoverBehavior)
  })
}


shinyApp(ui = ui, server = server)

library(leaflet)

leaflet() %>% addTiles() %>%
  onRender("
           function(el, x) {
           // Navigate the map to the user's location
           this.locate({setView: true});
           }
           ")