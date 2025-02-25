default_psa_params <- list(
  # prob Stage 2 -> Stage 3 if untreated
  p_S2_S3_unt = list(
    name = "P(S2 → S3) if untreated",
    def_dis = "beta", 
    def_v1 = "30", 
    def_v2 = "170"
  ),
  
  # rate ratio Stage 2 -> Stage 3 treated/untreated
  hr_S2_S3 = list(
    name = "Hazard Ratio for P(S2 → S3) Treated/Untreated",
    def_dis = "normal", 
    def_v1 = "0.7", 
    def_v2 = "0.3"
  ),
  
  # prob Stage 2 -> Dead
  p_S2_D = list(
    name = "P(S2 → Dead)",
    def_dis = "beta", 
    def_v1 = "1", 
    def_v2 = "1990"
  ),
  
  # rate ratio death Stage 3 vs Stage 2
  hr_S3_D = list(
    name = "Hazard Ratio for death S3/S2",
    def_dis = "normal", 
    def_v1 = "0.1", 
    def_v2 = "0.3"
  ),
  
  # cost p/cycle in state S2
  c_S2 = list(
    name = "Cost per cycle in Stage 2",
    def_dis = "gamma", 
    def_v1 = "100", 
    def_v2 = "20"
  ),
  
  # cost p/cycle in state S3
  c_S3 = list(
    name = "Cost per cycle in Stage 3",
    def_dis = "gamma", 
    def_v1 = "177.8", 
    def_v2 = "22.5"
  ),
  
  # cost p/cycle of treatment in state S2
  c_Trt_S2 = list(
    name = "Cost of treatment in Stage 2",
    def_dis = "fixed", 
    def_v1 = "2000", 
    def_v2 = ""
  ),
  
  # cost p/cycle of treatment in state S3 (we stop treating in S3)
  c_Trt_S3 = list(
    name = "Cost of treatment in Stage 3",
    def_dis = "fixed", 
    def_v1 = "0", 
    def_v2 = ""
  ),

  # cost p/cycle in state D (fixed value)
  c_D = list(
    name = "Cost of Death",
    def_dis = "fixed", 
    def_v1 = "0", 
    def_v2 = ""
  ),
  
  # utility when stage 2
  u_S2 = list(
    name = "Utility in Stage 2",
    def_dis = "normal", 
    def_v1 = "1", 
    def_v2 = "0.01"
  ),
  
  # utility when stage 3
  u_S3 = list(
    name = "Utility in Stage 3",
    def_dis = "normal", 
    def_v1 = "0.75", 
    def_v2 = "0.02"
  ),
  
  # utility when dead
  u_D = list(
    name = "Utility when dead",
    def_dis = "fixed", 
    def_v1 = "0", 
    def_v2 = ""
  )
)
