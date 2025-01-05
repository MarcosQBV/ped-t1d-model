psa_param_ids <- names(default_psa_params)
rows_rendered <- reactiveValues()

psa_parameters_panel <- function() {
  rendered_rows <- reactiveValues(list = list())
  fluidPage(
    div(style = 'margin-top:20px; margin-bottom:80px',
      h2("Probabilistic Sensitivity Analysis Parameters"),
    ),
    fluidRow(
      column(width = 6,
        psa_name_row(),
        lapply(psa_param_ids[1:6], function(param_id) {
          psa_table_row(
            id = param_id,
            name = default_psa_params[[param_id]]$name,
            def_dis = default_psa_params[[param_id]]$def_dis,
            def_v1 = default_psa_params[[param_id]]$def_v1,
            def_v2 = default_psa_params[[param_id]]$def_v2
          )
        })
      ),
      column(width = 6,
        psa_name_row(),
        lapply(psa_param_ids[7:12], function(param_id) {
          psa_table_row(
            id = param_id,
            name = default_psa_params[[param_id]]$name,
            def_dis = default_psa_params[[param_id]]$def_dis,
            def_v1 = default_psa_params[[param_id]]$def_v1,
            def_v2 = default_psa_params[[param_id]]$def_v2
          )
        })
      ),
      rows_rendered$all <- TRUE
    )
  )
}

psa_name_row <- function() {
  fluidRow(
    column(width = 3,
      p("Parameter")
    ),
    column(width = 2,
      p("Distribution")
    ),
    column(width = 2,
      p("Value 1")
    ),
    column(width = 2,
      p("Value 2")
    ),
    column(width = 2, offset = 1,
      p("Curve")
    )
  )
}



dist_params <- list(
  normal = c("mean", "sd"),
  random_uniform = c("min", "max"),
  gamma = c("shape", "rate"),
  log_normal = c("meanlog", "sdlog"),
  beta = c("shape1", "shape2"),
  fixed = c("value")
)

# row_configs <- lapply(row_ids, function(id) {
#   list(
#     id = id,
#     name = paste("Row", gsub("row", "", id)),
#     def_dis = "normal",
#     def_v1 = "",
#     def_v2 = ""
#   )
# })

psa_table_row <- function(id, name, def_dis, def_v1, def_v2) {
  all_dists <- names(dist_params)
  
  fluidRow(
    column(width = 3,
      p(style="margin-top: 25px",
      paste(name))
    ),
    column(width = 2,
      div(style="margin-top: 8px",
        selectizeInput(
          inputId = paste0(id, "_dist"),
          label = "",
          choices = all_dists,
          width = "100%",
          selected = def_dis
        )
      )
    ),
    column(width = 2,
      textInput(
        inputId = paste0(id, "_v1"),
        label = dist_params[[def_dis]][1],
        value = def_v1
      )
    ),
    column(width = 2,
      textInput(
        inputId = paste0(id, "_v2"),
        label = dist_params[[def_dis]][2],
        value = def_v2
      )
    ),
    column(width = 3,
      plotOutput(outputId = paste0(id, "_plot"), height = "100px")
    )
  )
}

plotSwitcher = function(dist, vals){

  switch(dist,

    "random_uniform" = {
      plot_x = seq(vals[1], vals[2], length.out = 100)
      plot_y = dunif(plot_x, vals[1], vals[2])
      plot_range_x = c(vals[1] - ((vals[1] + vals[2]) / 10), vals[2] + ((vals[1] + vals[2]) / 10))
      plot_range_y = c(0, max(plot_y) + max(plot_y) / 10)
    },

    "normal" = {
      plot_x = qnorm(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
      plot_y = dnorm(plot_x, vals[1], vals[2])
      plot_range_x = range(plot_x)
      plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
    },
    "log_normal" = {
      plot_x = qlnorm(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
      plot_y = dlnorm(plot_x, vals[1], vals[2])
      plot_range_x = range(plot_x)
      plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
    },
    "beta" = {
      plot_x = qbeta(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
      plot_y = dbeta(plot_x, vals[1], vals[2])
      plot_range_x = c(0,1)
      plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
    },
    "fixed" = {
      return(NULL)
    },
    "gamma" = {
      plot_x = qgamma(p = seq(0.01, 0.99, length.out = 100), shape = vals[1], scale = vals[2])
      plot_y = dgamma(plot_x, shape = vals[1], scale = vals[2])
      plot_range_x = range(plot_x)
      plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
    }
  )
  res = list(
    plot_x = plot_x,
    plot_y = plot_y,
    plot_range_x = plot_range_x,
    plot_range_y = plot_range_y
  )

  return(res)
}

# takes a list with data from plotSwitcher() and generate a ggplot
tblPlotter = function(dist, val1, val2){


  vals = as.numeric(c(val1, val2))
  print(paste0("[vals in plot]", vals, "[dist]", dist))

  if(dist =="fixed") {
    tbl_plot = ggplot() +
      theme_void() +
      geom_label(aes(x = 1,y = 1,label = val1),size = 15) +
      ggtitle(paste("Fixed"))
    return(tbl_plot)
  } else if (NA %in% vals | -1 %in% sign(vals)) {
    # if there is a NA value
    tbl_plot = ggplot() +
      theme_void() +
      geom_label(aes(x = 1,y = 1,label = "Invalid input"),size = 10,col="#ff0000") 
    return(tbl_plot)
  }

  
  # else
  x = plotSwitcher(dist, vals)

  tbl_plot = ggplot() +
    geom_density(
      aes(x = x$plot_x, y = x$plot_y),
      stat = "identity", fill = "cadetblue",
      col = "cadetblue4", alpha = 0.5
    ) +
    coord_cartesian(
      xlim = x$plot_range_x,
      ylim = x$plot_range_y
    ) +
    xlab("Value") +
    scale_y_continuous(name = "Density", labels = NULL) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )


  return(tbl_plot)
}

# Simple plot to try out displaying plots
dummyPlot = function(){
  
}
