library(shape)
library(markovchain)
library(diagram)

# Creating a transition matrix
trans_mat <- matrix(c(0.98144,0.01856, 0, 0,0.97638, 0.02362, 0,0,1),nrow = 3, byrow = TRUE)
trans_mat

# create the Discrete Time Markov Chain
disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=c("PFS","PD", "Death"), name="Markov Chain for VRD") 
disc_trans
plot(disc_trans)

#VRD
params_vrd = list(
#Transition matrix in VRD for PFS and PD
  p_pfs_pd_vrd = 0.01856,
  p_pd_death_vrd = 0.02362,
  
  #Cost in VRD for PFS and PD
  NHIF_Induction_AE_PFS_vrd = 20585.32951,
  NHIF_Induction_AE_PD_vrd = 6719.213517,
  NHIF_Induction_AE_nonmedicalcost_PFS_vrd = 25463.70789,
  NHIF_Induction_AE_nonmedicalcost_PD_vrd = 11305.42041,
  CASH_Induction_AE_PFS_vrd =19499.816,
  CASH_Induction_AE_PD_vrd = 5610.110069,
  CASH_Induction_AE_nonmedical_cost_PFS_vrd = 24378.19438,
  CASH_Induction_AE_nonmedical_cost_PD_vrd = 10196.31697,
  U_PFS_vrd = 0.81,
  U_PD_vrd = 0.645
)

model_vrd = function(.params_vrd) {
  with(.params_vrd, {
    n_cycles_vrd = 72
    n_states_vrd = 3
    n_patients_vrd = 1000
    
    v_states_names_vrd = c("PFS", "PD", "Death")
    
    m_p_vrd = matrix(0, nrow = 3, ncol = 3,
                 dimnames = list(from = v_states_names_vrd,
                                 to = v_states_names_vrd))
    m_p_vrd["PFS", "PFS"] = 1 - p_pfs_pd_vrd
    m_p_vrd["PFS", "PD"] = p_pfs_pd_vrd
    m_p_vrd["PFS", "Death"] = 0
    m_p_vrd["PD", "Death"] = p_pd_death_vrd
    m_p_vrd["PD", "PFS"] = 0
    m_p_vrd["PD", "PD"] = 1- p_pd_death_vrd
    m_p_vrd["Death", "Death"] = 1
    
    state_membership_vrd = array(NA_real_,
                             dim = c(n_cycles_vrd, n_states_vrd),
                             dimnames = list(cycle = 1:n_cycles_vrd,
                                             state = v_states_names_vrd))
    state_membership_vrd[1, ] = c(n_patients_vrd, 0, 0)
    
    for (i in 2:n_cycles_vrd) {
      state_membership_vrd[i, ] = state_membership_vrd[i-1, ] %*% m_p_vrd
    }
    
    #NHIF_Induction_AE
    m_payoffs_NHIF_Induction_AE_vrd = matrix(0, nrow = 3, ncol = 2,
                       dimnames = list(state = v_states_names_vrd,
                                       payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_vrd["PFS", "Cost"] = NHIF_Induction_AE_PFS_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PD", "Cost"] = NHIF_Induction_AE_PD_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PD", "QALYs"] = U_PD_vrd
    
    #NHIF_Induction_AE_NONMEDICAL_COST
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd = matrix(0, nrow = 3, ncol = 2,
                                         dimnames = list(state = v_states_names_vrd,
                                                         payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PFS", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PFS_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PD", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PD_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PD", "QALYs"] = U_PD_vrd
    
    #CASH_Induction_AE
    m_payoffs_CASH_Induction_AE_vrd = matrix(0, nrow = 3, ncol = 2,
                                         dimnames = list(state = v_states_names_vrd,
                                                         payoffs = c("Cost", "QALYs")))
    
    m_payoffs_CASH_Induction_AE_vrd["PFS", "Cost"] = CASH_Induction_AE_PFS_vrd
    m_payoffs_CASH_Induction_AE_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_CASH_Induction_AE_vrd["PD", "Cost"] = CASH_Induction_AE_PD_vrd
    m_payoffs_CASH_Induction_AE_vrd["PD", "QALYs"] = U_PD_vrd
    
    #CASH_Induction_AE_NONMEDICAL_COST
    m_payoffs_cash_Induction_AE_nonmedical_vrd = matrix(0, nrow = 3, ncol = 2,
                                                    dimnames = list(state = v_states_names_vrd,
                                                                    payoffs = c("Cost", "QALYs")))
    
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PFS", "Cost"] = CASH_Induction_AE_nonmedical_cost_PFS_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PD", "Cost"] = CASH_Induction_AE_nonmedical_cost_PD_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PD", "QALYs"] = U_PD_vrd
    
    #View the matrix
    m_payoffs_CASH_Induction_AE_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd
    m_payoffs_NHIF_Induction_AE_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    

    #Total  cost and QALYs of for PFS state
    #m_payoffs_CASH_Induction_AE_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*% m_payoffs_CASH_Induction_AE_vrd
    #m_payoffs_cash_Induction_AE_nonmedical_trace_PFS= state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    #m_payoffs_NHIF_Induction_AE_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_vrd
    #m_payoffs_NHIF_Induction_AE_nonmedical_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Total  cost and QALYs of for PD state
    #m_payoffs_CASH_Induction_AE_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*% m_payoffs_CASH_Induction_AE_vrd
    #m_payoffs_cash_Induction_AE_nonmedical_trace_PD= state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    #m_payoffs_NHIF_Induction_AE_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_vrd
    #m_payoffs_NHIF_Induction_AE_nonmedical_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Total  cost of all patients
    m_payoffs_CASH_Induction_AE_trace_vrd = state_membership_vrd %*% m_payoffs_CASH_Induction_AE_vrd
    m_payoffs_cash_Induction_AE_nonmedical_trace_vrd= state_membership_vrd %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    m_payoffs_NHIF_Induction_AE_trace_vrd = state_membership_vrd %*%m_payoffs_NHIF_Induction_AE_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_trace_vrd = state_membership_vrd %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Average cost per patients per state and cumulatively
    average_cost_QALYS=list(
      #Average cost and QALYS per patients for each payment option
      colSums(m_payoffs_CASH_Induction_AE_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_cash_Induction_AE_nonmedical_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_NHIF_Induction_AE_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_NHIF_Induction_AE_nonmedical_trace_vrd) / n_patients_vrd
    )
    #Adding labels to each output
    label_names_vrd <- c("Average Cost and QALYs (In Induction, Maintenance and AE paid through CASH)",
                        "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through CASH)",
                        "Average Cost and QALYs (In Induction, Maintenance and AE paid through NHIF)",
                        "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through NHIF)")
    #Title for the overall output
    cat("Summary of Payoffs Matrix (Normalized by Number of Patients):\n")
    # Print each labeled output
    for (i in seq_along(average_cost_QALYS)) {
      cat("\n[", i, "]", label_names_vrd[i], ":\n")
      print(average_cost_QALYS[[i]])
}
}
)
}
model_vrd(params_vrd)



#RD Reference Arm
params_rd = list(
  #Transition matrix in RD for PFS and PD
  p_pfs_pd_rd = 0.02631,
  p_pd_death_rd = 0.02362,
  
  #Cost in RD for PFS and PD
  NHIF_Induction_AE_PFS_rd = 17196.47715,
  NHIF_Induction_AE_PD_rd = 25094.08297,
  NHIF_Induction_AE_nonmedicalcost_PFS_rd = 22677.24638,
  NHIF_Induction_AE_nonmedicalcost_PD_rd = 30007.87607,
  CASH_Induction_AE_PFS_rd =16323.78485,
  CASH_Induction_AE_PD_rd = 23932.35883,
  CASH_Induction_AE_nonmedical_cost_PFS_rd = 21804.55408,
  CASH_Induction_AE_nonmedical_cost_PD_rd = 28846.15193,
  U_PFS_rd = 0.81,
  U_PD_rd = 0.645
)

model_rd = function(.params_rd) {
  with(.params_rd, {
    n_cycles_rd = 72
    n_states_rd = 3
    n_patients_rd = 1000
    
    v_states_names_rd = c("PFS", "PD", "Death")
    
    m_p_rd = matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(from = v_states_names_rd,
                                     to = v_states_names_rd))
    m_p_rd["PFS", "PFS"] = 1 - p_pfs_pd_rd
    m_p_rd["PFS", "PD"] = p_pfs_pd_rd
    m_p_rd["PFS", "Death"] = 0
    m_p_rd["PD", "Death"] = p_pd_death_rd
    m_p_rd["PD", "PFS"] = 0
    m_p_rd["PD", "PD"] = 1- p_pd_death_rd
    m_p_rd["Death", "Death"] = 1
    
    state_membership_rd = array(NA_real_,
                                 dim = c(n_cycles_rd, n_states_rd),
                                 dimnames = list(cycle = 1:n_cycles_rd,
                                                 state = v_states_names_rd))
    state_membership_rd[1, ] = c(n_patients_rd, 0, 0)
    
    for (i in 2:n_cycles_rd) {
      state_membership_rd[i, ] = state_membership_rd[i-1, ] %*% m_p_rd
    }
    
    #NHIF_Induction_AE
    m_payoffs_NHIF_Induction_AE_rd = matrix(0, nrow = 3, ncol = 2,
                                             dimnames = list(state = v_states_names_rd,
                                                             payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_rd["PFS", "Cost"] = NHIF_Induction_AE_PFS_rd
    m_payoffs_NHIF_Induction_AE_rd["PFS", "QALYs"] = U_PFS_rd
    m_payoffs_NHIF_Induction_AE_rd["PD", "Cost"] = NHIF_Induction_AE_PD_rd
    m_payoffs_NHIF_Induction_AE_rd["PD", "QALYs"] = U_PD_rd
    
    #NHIF_Induction_AE_NONMEDICAL_COST
    m_payoffs_NHIF_Induction_AE_nonmedical_rd = matrix(0, nrow = 3, ncol = 2,
                                                        dimnames = list(state = v_states_names_rd,
                                                                        payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_nonmedical_rd["PFS", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PFS_rd
    m_payoffs_NHIF_Induction_AE_nonmedical_rd["PFS", "QALYs"] = U_PFS_rd
    m_payoffs_NHIF_Induction_AE_nonmedical_rd["PD", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PD_rd
    m_payoffs_NHIF_Induction_AE_nonmedical_rd["PD", "QALYs"] = U_PD_rd
    
    #CASH_Induction_AE
    m_payoffs_CASH_Induction_AE_rd = matrix(0, nrow = 3, ncol = 2,
                                             dimnames = list(state = v_states_names_rd,
                                                             payoffs = c("Cost", "QALYs")))
    
    m_payoffs_CASH_Induction_AE_rd["PFS", "Cost"] = CASH_Induction_AE_PFS_rd
    m_payoffs_CASH_Induction_AE_rd["PFS", "QALYs"] = U_PFS_rd
    m_payoffs_CASH_Induction_AE_rd["PD", "Cost"] = CASH_Induction_AE_PD_rd
    m_payoffs_CASH_Induction_AE_rd["PD", "QALYs"] = U_PD_rd
    
    #CASH_Induction_AE_NONMEDICAL_COST
    m_payoffs_cash_Induction_AE_nonmedical_rd = matrix(0, nrow = 3, ncol = 2,
                                                        dimnames = list(state = v_states_names_rd,
                                                                        payoffs = c("Cost", "QALYs")))
    
    m_payoffs_cash_Induction_AE_nonmedical_rd["PFS", "Cost"] = CASH_Induction_AE_nonmedical_cost_PFS_rd
    m_payoffs_cash_Induction_AE_nonmedical_rd["PFS", "QALYs"] = U_PFS_rd
    m_payoffs_cash_Induction_AE_nonmedical_rd["PD", "Cost"] = CASH_Induction_AE_nonmedical_cost_PD_rd
    m_payoffs_cash_Induction_AE_nonmedical_rd["PD", "QALYs"] = U_PD_rd
    
    #View the matrix
    m_payoffs_CASH_Induction_AE_rd
    m_payoffs_cash_Induction_AE_nonmedical_rd
    m_payoffs_NHIF_Induction_AE_rd
    m_payoffs_NHIF_Induction_AE_nonmedical_rd
    
    
    #total  cost of all patients
    m_payoffs_CASH_Induction_AE_trace_rd = state_membership_rd %*% m_payoffs_CASH_Induction_AE_rd
    m_payoffs_cash_Induction_AE_nonmedical_trace_rd= state_membership_rd %*%m_payoffs_cash_Induction_AE_nonmedical_rd
    m_payoffs_NHIF_Induction_AE_trace_rd = state_membership_rd %*%m_payoffs_NHIF_Induction_AE_rd
    m_payoffs_NHIF_Induction_AE_nonmedical_trace_rd = state_membership_rd %*%m_payoffs_NHIF_Induction_AE_nonmedical_rd
    
    #Average cost per patients
    average_cost_QALYS_rd=list(
      #Average cost and QALYS per patients for each payment option
      colSums(m_payoffs_CASH_Induction_AE_trace_rd) / n_patients_rd,
      colSums(m_payoffs_cash_Induction_AE_nonmedical_trace_rd) / n_patients_rd,
      colSums(m_payoffs_NHIF_Induction_AE_trace_rd) / n_patients_rd,
      colSums(m_payoffs_NHIF_Induction_AE_nonmedical_trace_rd) / n_patients_rd
    )
    #Adding labels to each output
    label_names_rd <- c("Average Cost and QALYs (In Induction, Maintenance and AE paid through CASH)",
                        "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through CASH)",
                        "Average Cost and QALYs (In Induction, Maintenance and AE paid through NHIF)",
                        "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through NHIF)")
    #Title for the overall output
    cat("Summary of Payoffs Matrix (Normalized by Number of Patients):\n")
    # Print each labeled output
    for (i in seq_along(average_cost_QALYS_rd)) {
      cat("\n[", i, "]", label_names_rd[i], ":\n")
      print(average_cost_QALYS_rd[[i]])
}
}
)
}
model_rd(params_rd)

#PROBABILISTIC SENSITIVITY ANALYSIS (VRD)
N_psa_vrd = 1000

params_vrd = data.frame(
  #Transition matrix in VRD for PFS and PD
  p_pfs_pd_vrd = 0.01856,
  p_pd_death_vrd = 0.02362,
  
  #Cost in VRD for PFS and PD
  NHIF_Induction_AE_PFS_vrd = 20585.32951,
  NHIF_Induction_AE_PD_vrd = 6719.213517,
  NHIF_Induction_AE_nonmedicalcost_PFS_vrd = 25463.70789,
  NHIF_Induction_AE_nonmedicalcost_PD_vrd = 11305.42041,
  CASH_Induction_AE_PFS_vrd =19499.816,
  CASH_Induction_AE_PD_vrd = 5610.110069,
  CASH_Induction_AE_nonmedical_cost_PFS_vrd = 24378.19438,
  CASH_Induction_AE_nonmedical_cost_PD_vrd = 10196.31697,
  U_PFS_vrd = 0.81,
  U_PD_vrd = 0.645
)

model_vrd = function(.params_vrd) {
  with(.params_vrd, {
    n_cycles_vrd = 72
    n_states_vrd = 3
    n_patients_vrd = 1000
    
    v_states_names_vrd = c("PFS", "PD", "Death")
    
    m_p_vrd = matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(from = v_states_names_vrd,
                                     to = v_states_names_vrd))
    m_p_vrd["PFS", "PFS"] = 1 - p_pfs_pd_vrd
    m_p_vrd["PFS", "PD"] = p_pfs_pd_vrd
    m_p_vrd["PFS", "Death"] = 0
    m_p_vrd["PD", "Death"] = p_pd_death_vrd
    m_p_vrd["PD", "PFS"] = 0
    m_p_vrd["PD", "PD"] = 1- p_pd_death_vrd
    m_p_vrd["Death", "Death"] = 1
    
    state_membership_vrd = array(NA_real_,
                                 dim = c(n_cycles_vrd, n_states_vrd),
                                 dimnames = list(cycle = 1:n_cycles_vrd,
                                                 state = v_states_names_vrd))
    state_membership_vrd[1, ] = c(n_patients_vrd, 0, 0)
    
    for (i in 2:n_cycles_vrd) {
      state_membership_vrd[i, ] = state_membership_vrd[i-1, ] %*% m_p_vrd
    }
    
    #NHIF_Induction_AE
    m_payoffs_NHIF_Induction_AE_vrd = matrix(0, nrow = 3, ncol = 2,
                                             dimnames = list(state = v_states_names_vrd,
                                                             payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_vrd["PFS", "Cost"] = NHIF_Induction_AE_PFS_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PD", "Cost"] = NHIF_Induction_AE_PD_vrd
    m_payoffs_NHIF_Induction_AE_vrd["PD", "QALYs"] = U_PD_vrd
    
    #NHIF_Induction_AE_NONMEDICAL_COST
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd = matrix(0, nrow = 3, ncol = 2,
                                                        dimnames = list(state = v_states_names_vrd,
                                                                        payoffs = c("Cost", "QALYs")))
    
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PFS", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PFS_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PD", "Cost"] = NHIF_Induction_AE_nonmedicalcost_PD_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd["PD", "QALYs"] = U_PD_vrd
    
    #CASH_Induction_AE
    m_payoffs_CASH_Induction_AE_vrd = matrix(0, nrow = 3, ncol = 2,
                                             dimnames = list(state = v_states_names_vrd,
                                                             payoffs = c("Cost", "QALYs")))
    
    m_payoffs_CASH_Induction_AE_vrd["PFS", "Cost"] = CASH_Induction_AE_PFS_vrd
    m_payoffs_CASH_Induction_AE_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_CASH_Induction_AE_vrd["PD", "Cost"] = CASH_Induction_AE_PD_vrd
    m_payoffs_CASH_Induction_AE_vrd["PD", "QALYs"] = U_PD_vrd
    
    #CASH_Induction_AE_NONMEDICAL_COST
    m_payoffs_cash_Induction_AE_nonmedical_vrd = matrix(0, nrow = 3, ncol = 2,
                                                        dimnames = list(state = v_states_names_vrd,
                                                                        payoffs = c("Cost", "QALYs")))
    
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PFS", "Cost"] = CASH_Induction_AE_nonmedical_cost_PFS_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PFS", "QALYs"] = U_PFS_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PD", "Cost"] = CASH_Induction_AE_nonmedical_cost_PD_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd["PD", "QALYs"] = U_PD_vrd
    
    #View the matrix
    m_payoffs_CASH_Induction_AE_vrd
    m_payoffs_cash_Induction_AE_nonmedical_vrd
    m_payoffs_NHIF_Induction_AE_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    
    #Total  cost and QALYs of for PFS state
    #m_payoffs_CASH_Induction_AE_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*% m_payoffs_CASH_Induction_AE_vrd
    #m_payoffs_cash_Induction_AE_nonmedical_trace_PFS= state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    #m_payoffs_NHIF_Induction_AE_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_vrd
    #m_payoffs_NHIF_Induction_AE_nonmedical_trace_PFS = state_membership_vrd[, "PFS", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Total  cost and QALYs of for PD state
    #m_payoffs_CASH_Induction_AE_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*% m_payoffs_CASH_Induction_AE_vrd
    #m_payoffs_cash_Induction_AE_nonmedical_trace_PD= state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    #m_payoffs_NHIF_Induction_AE_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_vrd
    #m_payoffs_NHIF_Induction_AE_nonmedical_trace_PD = state_membership_vrd[, "PD", , drop = FALSE] %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Total  cost of all patients
    m_payoffs_CASH_Induction_AE_trace_vrd = state_membership_vrd %*% m_payoffs_CASH_Induction_AE_vrd
    m_payoffs_cash_Induction_AE_nonmedical_trace_vrd= state_membership_vrd %*%m_payoffs_cash_Induction_AE_nonmedical_vrd
    m_payoffs_NHIF_Induction_AE_trace_vrd = state_membership_vrd %*%m_payoffs_NHIF_Induction_AE_vrd
    m_payoffs_NHIF_Induction_AE_nonmedical_trace_vrd = state_membership_vrd %*%m_payoffs_NHIF_Induction_AE_nonmedical_vrd
    
    #Average cost per patients per state and cumulatively
    average_cost_QALYS=list(
      #Average cost and QALYS per patients for each payment option
      colSums(m_payoffs_CASH_Induction_AE_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_cash_Induction_AE_nonmedical_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_NHIF_Induction_AE_trace_vrd) / n_patients_vrd,
      colSums(m_payoffs_NHIF_Induction_AE_nonmedical_trace_vrd) / n_patients_vrd
    )
    #Adding labels to each output
    label_names_vrd <- c("Average Cost and QALYs (In Induction, Maintenance and AE paid through CASH)",
                         "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through CASH)",
                         "Average Cost and QALYs (In Induction, Maintenance and AE paid through NHIF)",
                         "Average Cost and QALYs (In Induction, Maintenance, AE and Non-Medical paid through NHIF)")
    #Title for the overall output
    cat("Summary of Payoffs Matrix (Normalized by Number of Patients):\n")
    # Print each labeled output
    for (i in seq_along(average_cost_QALYS)) {
      cat("\n[", i, "]", label_names_vrd[i], ":\n")
      print(average_cost_QALYS[[i]])
    }
  }
  )
}
model_vrd(params_vrd)

