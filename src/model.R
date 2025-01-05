f_gen_psa <- function(psa_params, model_params) {
  # Initialize an empty list to store the generated data
  df_list <- list()
  
  # Iterate over each parameter in the psa_params list
  for (param_name in names(psa_params)) {
    param_info <- psa_params[[param_name]]
    dist_type <- param_info$def_dis
    v1 <- as.numeric(param_info$def_v1)
    v2 <- as.numeric(param_info$def_v2)
    
    # Generate samples based on the distribution type
    samples <- switch(dist_type,
      "beta" = rbeta(n = model_params$n_sim, shape1 = v1, shape2 = v2),
      "log_normal" = rlnorm(n = model_params$n_sim, meanlog = log(v1), sdlog = v2),
      "normal" = rnorm(n = model_params$n_sim, mean = v1, sd = v2),
      "gamma" = rgamma(n = model_params$n_sim, shape = v1, rate = 1/v2),
      "fixed" = rep(v1, model_params$n_sim),
      stop("Unsupported distribution type")
    )
    
    # Store the samples in the list with the parameter name
    df_list[[param_name]] <- samples
  }
  
  # Convert the list to a data frame
  df <- as.data.frame(df_list)
  return(df)
}

# Calculate transition rates and probabilities
calculate_transition_rates <- function(psa_table_row) {
  with(as.list(psa_table_row), {
    r_S2_S3_unt <- -log(1 - p_S2_S3_unt)
    r_S2_S3_trt <- hr_S2_S3 * r_S2_S3_unt
    p_S2_S3_trt <- 1 - exp(-r_S2_S3_trt)
    
    r_S2_D <- -log(1 - p_S2_D)
    r_S3_D <- hr_S3_D * r_S2_D
    p_S3_D <- 1 - exp(-r_S3_D)
    
    list(r_S2_S3_unt = r_S2_S3_unt, r_S2_S3_trt = r_S2_S3_trt, p_S2_S3_trt = p_S2_S3_trt,
         r_S2_D = r_S2_D, r_S3_D = r_S3_D, p_S3_D = p_S3_D)
  })
}

calculate_discount_weights <- function(model_params) {
  n_t <- model_params$n_t
  d_r <- model_params$d_r
  v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:n_t)
  list(v_dwe = v_dwe, v_dwc = v_dwc)
}

create_transition_matrices <- function(psa_table_row, transition_rates) {
  with(as.list(psa_table_row), {
    m_P_unt <- m_P_trt <- matrix(0, nrow = n_states, ncol = n_states, dimnames = list(v_n, v_n))
    
    # Untreated
    m_P_unt["S2", "S2"] <- 1 - (p_S2_S3_unt + p_S2_D)
    m_P_unt["S2", "S3"] <- p_S2_S3_unt
    m_P_unt["S2", "D"] <- p_S2_D
    m_P_unt["S3", "S3"] <- 1 - transition_rates$p_S3_D
    m_P_unt["S3", "D"] <- transition_rates$p_S3_D
    m_P_unt["D", "D"] <- 1
    
    # Treated
    m_P_trt["S2", "S2"] <- 1 - (transition_rates$p_S2_S3_trt + p_S2_D)
    m_P_trt["S2", "S3"] <- transition_rates$p_S2_S3_trt
    m_P_trt["S2", "D"] <- p_S2_D
    m_P_trt["S3", "S3"] <- 1 - transition_rates$p_S3_D
    m_P_trt["S3", "D"] <- transition_rates$p_S3_D
    m_P_trt["D", "D"] <- 1
    
    list(m_P_unt = m_P_unt, m_P_trt = m_P_trt)
  })
}

simulate_markov_traces <- function(model_params, transition_matrices) {
  n_t <- model_params$n_t
  m_TR_trt <- m_TR_unt <- matrix(NA, nrow = n_t + 1, ncol = n_states, dimnames = list(0:n_t, v_n))
  m_TR_unt[1, ] <- m_TR_trt[1, ] <- c(1, 0, 0)
  
  for (t in 1:n_t) {
    m_TR_unt[t + 1, ] <- m_TR_unt[t, ] %*% transition_matrices$m_P_unt
    m_TR_trt[t + 1, ] <- m_TR_trt[t, ] %*% transition_matrices$m_P_trt
  }
  
  list(m_TR_unt = m_TR_unt, m_TR_trt = m_TR_trt)
}

calculate_costs_qalys <- function(psa_table_row, markov_traces, discount_weights) {
  with(as.list(psa_table_row), {
    v_u <- c(u_S2, u_S3, u_D)
    v_c_trt <- c(c_S2 + c_Trt_S2, c_S3, c_D)
    v_c_unt <- c(c_S2, c_S3, c_D)
    
    v_E_unt <- markov_traces$m_TR_unt %*% v_u
    v_E_trt <- markov_traces$m_TR_trt %*% v_u
    v_C_unt <- markov_traces$m_TR_unt %*% v_c_unt
    v_C_trt <- markov_traces$m_TR_trt %*% v_c_trt
    
    te_unt <- t(v_E_unt) %*% discount_weights$v_dwe
    te_trt <- t(v_E_trt) %*% discount_weights$v_dwe
    tc_unt <- t(v_C_unt) %*% discount_weights$v_dwc
    tc_trt <- t(v_C_trt) %*% discount_weights$v_dwc
    
    results <- c(
      "Cost_NoTrt" = tc_unt,
      "Cost_Trt" = tc_trt,
      "QALY_NoTrt" = te_unt,
      "QALY_Trt" = te_trt,
      "ICER" = (tc_trt - tc_unt) / (te_trt - te_unt)
    )
    return(results)
  })
}

f_hybrid_markov_model <- function(psa_table_row, model_params) {
  environment(create_transition_matrices) <- environment()
  environment(simulate_markov_traces)     <- environment()

  transition_rates <- calculate_transition_rates(psa_table_row)
  discount_weights <- calculate_discount_weights(model_params)
  transition_matrices <- create_transition_matrices(psa_table_row, transition_rates)
  markov_traces <- simulate_markov_traces(model_params, transition_matrices)
  results <- calculate_costs_qalys(psa_table_row, markov_traces, discount_weights)
  return(results)
}