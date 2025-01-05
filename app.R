# load packages from library
library(truncnorm)
library(shiny)  
library(shinyWidgets)
library(bslib)
library(miceadds)
library(ggplot2)

source.all("./src")
source.all("./ui")
source.all("./public")

ui <- page_navbar(    # creates empty page
  nav_panel("About", 
    about_panel()
  ), # about panel end
  nav_panel("PSA Parameters", 
    psa_parameters_panel()
  ), 
  nav_panel("Run Model", 
    run_model_panel()
  ),
  id = "page",
) 

# Shiny server function ----
server <- function(input, output){  

    # List of plot parameters
  plot_params <- psa_param_ids

  # Initialize a list to store reactive values for each plot
  plot_values <- setNames(lapply(plot_params, function(x) reactiveValues(dist = NULL, v1 = NULL, v2 = NULL)), plot_params)

  # Function to create observers for each plot
  createObserver <- function(param) {
    observe({
      dist <- input[[paste0(param, "_dist")]]
      val_1 <- input[[paste0(param, "_v1")]]
      val_2 <- input[[paste0(param, "_v2")]]
      
      if (!identical(plot_values[[param]]$dist, dist) || 
          !identical(plot_values[[param]]$v1, val_1) || 
          !identical(plot_values[[param]]$v2, val_2)) {
        plot_values[[param]]$dist <- dist
        plot_values[[param]]$v1 <- val_1
        plot_values[[param]]$v2 <- val_2

        if (!is.null(dist_params[[dist]])) {
          updateTextInput(inputId = paste0(param, "_v1"), label = dist_params[[dist]][1])
          updateTextInput(inputId = paste0(param, "_v2"), label = dist_params[[dist]][2])
        }
      }
    })
  }

  # Function to create renderPlot for each plot
  createRenderPlot <- function(param) {
    output[[paste0(param, "_plot")]] <- renderPlot({
      tblPlotter(plot_values[[param]]$dist, plot_values[[param]]$v1, plot_values[[param]]$v2)
    })
  }

  # Apply the observer and renderPlot functions to each plot parameter
  lapply(plot_params, function(param) {
    createObserver(param)
    createRenderPlot(param)
  })



  # when action button pressed ...
  observeEvent(input$run_model,       
               ignoreNULL = F, {

    n_age_init <- input$set_age_range[1]
    n_age_max  <- input$set_age_range[2]
    d_r <- input$SI_d_r
    c_Trt <- input$c_Trt
    n_sims <- input$SI_n_sim
    
    model_params <- data.frame(
      n_age_init = n_age_init,
      n_age_max  = n_age_max,
      d_r = d_r,
      n_sim = n_sims
    )

    n_t <- model_params$n_age_max - model_params$n_age_init

    model_params$n_t <- n_t

    psa_params <- list()

    if (is.null(plot_values$u_D$dist)) {
      psa_params <- default_psa_params
    } else {
      psa_params <- lapply(psa_param_ids, function(param) {
        list(
          def_dis = plot_values[[param]]$dist,
          def_v1 = plot_values[[param]]$v1,
          def_v2 = plot_values[[param]]$v2
        )
      })
      names(psa_params) <- psa_param_ids
    }
    # Run model function with Shiny inputs
    df_model_res = f_wrapper(psa_params, model_params)
    
    #-- CREATE COST EFFECTIVENESS TABLE --#
    
    # renderTable continuously updates table
    output$SO_icer_table <- renderTable({ 
      
      df_res_table <- data.frame( # create dataframe
        
        Option =  c("Treatment","No Treatment"), 
        
        QALYs  =  c(mean(df_model_res$QALY_Trt),
                    mean(df_model_res$QALY_NoTrt)),
        
        Costs  =  c(mean(df_model_res$Cost_Trt),
                    mean(df_model_res$Cost_NoTrt)),
        
        Inc.QALYs = c(mean(df_model_res$QALY_Trt) - 
                        mean(df_model_res$QALY_NoTrt),
                      NA),
        
        Inc.Costs = c(mean(df_model_res$Cost_Trt) -
                        mean(df_model_res$Cost_NoTrt),
                      NA),
        
        ICER = c(mean(df_model_res$ICER), NA)
        
      ) # close data-frame
      
      # round the data-frame to two digits
      df_res_table[,2:6] = round(
        df_res_table[,2:6],digits = 2) 
      
      # print the results table
      df_res_table
      
    }) # table plot end.
    
    
    #--  CREATE COST EFFECTIVENESS PLANE --#
    
    # render plot repeatedly updates.
    output$SO_CE_plane <- renderPlot({ 
      
      # calculate incremental costs and qalys
      df_model_res$inc_C <- df_model_res$Cost_Trt - 
        df_model_res$Cost_NoTrt
      
      df_model_res$inc_Q <- df_model_res$QALY_Trt - 
        df_model_res$QALY_NoTrt
      
      # create cost effectiveness plane plot
      
      plot(
        # x y are incremental QALYs Costs
        x = df_model_res$inc_Q, 
        y = df_model_res$inc_C,
        
        # label axes
        xlab = "Incremental QALYs", 
        ylab = "Incremental Costs", 
        
        # set x-limits and y-limits for plot.
        xlim = c( min(df_model_res$inc_Q,
                      df_model_res$inc_Q*-1),
                  max(df_model_res$inc_Q,
                      df_model_res$inc_Q*-1)),
        
        ylim = c( min(df_model_res$inc_C,
                      df_model_res$inc_C*-1),
                  max(df_model_res$inc_C,
                      df_model_res$inc_C*-1)),
        
        # include y and y axis lines.
        abline(h = 0,v=0)
        
      ) # CE plot end
      
    }) # renderplot end
    
  }) # Observe event end
  
  
} # Server end

# Running the App ----

shinyApp(ui, server)