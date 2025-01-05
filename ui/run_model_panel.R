run_model_panel <- function() {
  # layout is a sidebar-layout
  sidebarLayout(    
    
    sidebarPanel( # open sidebar panel    
      
      # input type slider    
      noUiSliderInput(
                        inputId = "set_age_range",
                        label = "Age range to investigate",
                        min = 0, max = 18,
                        connect = c(FALSE, TRUE, FALSE),
                        value = c(1, 8),
                        pips = list(
                            mode = "values",
                            values = seq(0, 30, 5),
                            density = 5
                        ),
                        step = 1
                        ),
      
      # input type numeric    
      numericInput(inputId = "SI_d_r",  
                  label = "Discount rate",      
                  value = 0.04,                
                  min = 0.01,                   
                  max = 0.06),                  

      div(
        style = "display: flex; align-items: center; gap: 10px;",
        
        numericInput(inputId = "SI_n_sim",     
                    label = "PSA runs",       
                    value = 1000,              
                    min = 0,                   
                    max = 400),
        
        actionButton(inputId = "run_model",     
                    label   = "Run model",
                    style = "margin-top: 10px;")
      ) # close div
      
    ),  # close sidebarPanel
    
    # open main panel    
    mainPanel(                                
      
      # heading (results table)       
      h3("Results Table"),                                 
      
      # tableOutput id = icer_table, from server    
      tableOutput(outputId = "SO_icer_table"),   
      
      # heading (Cost effectiveness plane)    
      h3("Cost-effectiveness Plane"),         
      
      # plotOutput id = SO_CE_plane, from server
      plotOutput(outputId = "SO_CE_plane")       
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
}