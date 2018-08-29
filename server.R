shinyServer
(
  
  function(input, output) 
  {
    output$diab__pg <- renderPlot({
      
      plot(diab_pg)
      
      
    })
    
    output$diab__si <- renderPlot({
      
      plot(diab_si)
      
      
    })
    
    output$bp__bmi <- renderPlot({
      
      plot(bp_bmi)
      
      
    })
    
    output$pregnancy__ <- renderPlot({
     plot(pregnancy)
      
      
    })
    output$relation <- renderPlot({
      
      plot(acc_diab)
      
      
    })
    
    output$neural <- renderPlot({
      
      plot(nn)
      
      
    })
    
    
  }
)

