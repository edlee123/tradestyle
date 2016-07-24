library(shiny)
library(ggvis)
library(dplyr)
library(magrittr)
library(DT)
library(ggplot2)

gpdat = readRDS("./data/gpdat.rds")
models = readRDS("./data/models2.rds")

shinyServer(function(input, output, session) {
  
  vis <- reactive({
    gpdat %>% 
      filter(label == "ind") %>%
      droplevels %>%
      filter(dt >= input$year[1] & dt <= input$year[2] ) %>%
      ggvis(~dt, ~ val, stroke = ~label) %>%
      add_axis("x", title = "Date", properties = axis_props(
        labels = list(angle = 45, align = "left", fontSize = 10),
        title = list(fontSize = 12)
      )) %>%
      layer_lines(strokeWidth := 0.25)  %>%
      layer_points(size := 5, size.hover := 200,
                   fillOpacity := 0.01, fillOpacity.hover := 0.25) %>%
      add_tooltip(function(x) 
        paste( as.Date(x$dt/86400000, origin = '1970-01-01'), 
               x$val %>% as.character, sep = "<br/>"), on = "hover" ) 
    
  })
  
  vis %>% bind_shiny("plot1")
  
  output$perf_chart <- renderPlot({
    models_to_chart <- input$multichart
    if(!is.null(models_to_chart)){
      chtdat <- lapply(models_to_chart, 
                     function(x) monthlyReturn(models[[x]]$equity)) %>%
      Reduce(f = merge)
      colnames(chtdat) = models_to_chart
      # TODO (Ed): option to align starts.     
      charts.PerformanceSummary( chtdat, Rf = 0, ylog = T,
                                 begin = "axis",
                                 wealth.index = T,
                                 main = "Monthly Returns",
                                 legend.loc = "bottomright",
                                 cex.legend = 1.5,
                                 cex.main = 2,
                                 cex.lab = 1.25,
                                 cex.axis = 1.5
                                 )
    }
  }, width = 900, height = 400)
  
  output$perf_metrics <- DT::renderDataTable({ 
    models_to_chart <- input$multichart
    if(!is.null(models_to_chart)){
      lookup <- lapply(models_to_chart, function(x)models[[x]])
      names(lookup) <- models_to_chart
      out = plotbt.strategy.sidebyside(lookup, return.table=T, make.plot = F)
    }
    DT::datatable(out, options = 
                    list(searching = FALSE, 
                         paging = F, 
                         bInfo = F, 
                         style = 'bootstrap',
                         selection = "single")) 
  })
  
  #' Calculate calendar rets.  Will extract first full year.
  output$calret_table <- DT::renderDataTable({ 
    pfname = input$portfolio 
    calret = plotbt.monthly.table(models[[pfname]]$equity, smain = pfname, 
                                  make.plot = F)
    
    monret = monthlyReturn(models[[pfname]]$equity)
    calret2  = PerformanceAnalytics::table.CalendarReturns(monret)
    df <- calret %>% as.data.frame() 
    
    DT::datatable(calret, options = 
                    list(searching = FALSE, paging = F, bInfo = F, selection = "single")) %>%
      formatStyle(1:12,  
                  backgroundColor = styleInterval(0, c('pink', 'lightgreen'))) %>%
      formatStyle(13,  
                  backgroundColor = styleInterval(0, c('red', 'lightgreen')),
                  fontWeight = "bold"
                  ) %>%
      formatStyle(14,  
                  backgroundColor = "lightgrey") 
    
  })
  
  # Plot the selected style attribution
  # TODO (Ed): 
  # Add some annotation https://admainnew.morningstar.com/webhelp/glossary_definitions/mutual_fund/glossary_all_Alpha.html
  output$style_attrib <- renderPlot({
    pfname = input$portfoliostyle
    withProgress(message = 'Generating plot...', value = 0, {
      setProgress(0.5)
      Sys.sleep(0.1)
      setProgress(1)
    })
    style_roll_detail_plot_lattice(models[[pfname]]$style)
  }, height = 1000)
  
  output$factorlist <- renderUI(
    
    HTML("<ul><li>Alpha (Intercept)</li>
         <li>Market Factor</li>
        <li>SML: Small-Minus-Big</li>         
        <li>HML: High-Minus-Low Book-to-Price (Value-Growth)</li>
         <li>Mom: Momentum</li>
         </ul>")
  )
  
})