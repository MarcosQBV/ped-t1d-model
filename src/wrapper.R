f_wrapper <- function(psa_params, model_params) {
  # need to specify environment of inner functions (to use outer function enviroment)
  # alternatively - define functions within the wrapper function.
  environment(f_gen_psa)         <- environment()
  environment(f_hybrid_markov_model)   <- environment()

  df_psa <- f_gen_psa(psa_params, model_params)

  # the 4 health states of the model:
  v_n <- c("S2", "S3", "D") 
  # number of health states 
  n_states <- length(v_n) 

  n_sim <- model_params$n_sim

  # Initialize matrix of results outcomes
  m_out <- matrix(NaN, 
                  nrow = n_sim, 
                  ncol = 5,
                  dimnames = list(1:n_sim,
                                  c("Cost_NoTrt", "Cost_Trt",
                                    "QALY_NoTrt", "QALY_Trt",
                                    "ICER")))

  # run model for each row of PSA inputs
  for(i in 1:n_sim){

    # store results in row of results matrix
    m_out[i,] <- f_hybrid_markov_model(df_psa[i, ], model_params)

  } # close model loop


  #-- Return results --#

  # convert matrix to dataframe (for plots)
  df_out <- as.data.frame(m_out) 

  # output the dataframe from the function  
  return(df_out) 
}