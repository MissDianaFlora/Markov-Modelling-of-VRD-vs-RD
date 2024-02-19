#Install the necessary markov libraries
#install.packages("markovchain")
#install.packages("Markovchart")
#install.packages("shape")
#install.packages("diagram")

#Load the Packages 
library(Markovchart)
library(markovchain)
library(shape)
library(diagram)


#DEFINE THE PROJECT OVERVIEW
#Number of states (PFS, PD, Death)
#Two Chemotherapy drug options (VRD, RD)
#Cost (Induction,Maintenance, Adverse Events)
#Payment Options (NHIF, CASH)
#Life Horizon (6 years = 72 months)
#Cycle length = 28 days = 1 month = 1 cycle
#The discount rate is 3% p.a for both cost and QALY.
#Outcomes (Average cost per patient, QALYs, ICER, PSA, DSA, Visualization)

#Number of cycles
n_cycles = 72

#Number of states
n_states = 3

#Number of cohorts
cohort_size = 1000

#Vector for the state names
v_state_names = c("PFS", "PD", "Death")

# Step One: Define the transition matrix of VRD and RD
t_vrd <- matrix(c(0.98144, 0.01856, 0, 0, 0.97638, 0.02362, 0, 0, 1), nrow = 3, byrow = TRUE, dimnames = list(from = v_state_names, to = v_state_names))
t_rd <- matrix(c(0.97369, 0.02631, 0, 0, 0.97638, 0.02362, 0, 0, 1), nrow = 3, byrow = TRUE, dimnames = list(from = v_state_names, to = v_state_names))

#View the transition matrix of VRD and VD
t_vrd
t_rd

#Plot transition matrix of VRD and RD
# Increase the plotting area size
par(mar = c(5, 4, 4, 2) + 0.1)

#Save the plot for t_vrd to a file
png("markovchain_plot_vrd.png", width = 800, height = 600)
plot(t_vrd)
dev.off()

#Save the plot for t_rd to a different file
png("markovchain_plot_rd.png", width = 800, height = 600)
plot(t_rd)
dev.off()

#Reset the plotting parameters to default after plotting
par(mar = c(5, 4, 4, 2))

#Create an array for the state membership for VRD and RD
state_membership_VRD = array(NA_real_, dim = c(n_cycles, n_states), dimnames = list(cycles = 1:n_cycles, state = v_state_names))
state_membership_RD = array(NA_real_, dim = c(n_cycles, n_states), dimnames = list(cycles = 1:n_cycles, state = v_state_names))

#View the state memberships of VRD and RD
state_membership_VRD
state_membership_RD

#At the first cycle everyone is in the PFS state no one is in PD in both VRD and RD
state_membership_VRD[1, ] = c(cohort_size, 0,0)
state_membership_RD[1, ] = c(cohort_size, 0,0)

#View the state membership
state_membership_VRD
state_membership_RD

#Number of patients in each state in each cycle for VRD
for (i in 2:n_cycles) {
  state_membership_VRD[i, ] = state_membership_VRD[i -1, ]%*% t_vrd
  
}
state_membership_VRD

#Number of patients in each state in each cycle for RD
for (i in 2:n_cycles) {
  state_membership_RD[i, ] = state_membership_RD[i -1, ]%*% t_rd
  
}
state_membership_RD

#Step Two: Define Payoff (costs and QALYS) Matrix 
#Array/matrix of the payoffs(cost+qalys)for VRD
nhif_induction_AE_VRD = matrix(c(20585.32951, 6719.213517,0, 0.81,0.645, 0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, nhif_induction_AE = c("Cost", "QALYS")))
NHIF_Induction_AE_nonmedicalcost_VRD = matrix(c(25463.70789, 11305.42041,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, NHIF_Induction_AE_nonmedicalcost = c("Cost", "QALYS")))
CASH_Induction_AE_VRD = matrix(c(19499.816, 5610.110069,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, CASH_Induction_AE = c("Cost", "QALYS")))
CASH_Induction_AE_nonmedical_cost_VRD = matrix(c(24378.19438, 10196.31697,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, CASH_Induction_AE_nonmedical_cost = c("Cost", "QALYS")))

#View all the costs in VRD
nhif_induction_AE_VRD
NHIF_Induction_AE_nonmedicalcost_VRD
CASH_Induction_AE_VRD
CASH_Induction_AE_nonmedical_cost_VRD

#Array/matrix of the payoffs(cost+qalys)for RD
nhif_induction_AE_RD = matrix(c(17196.47715,25094.08297,0, 0.81,0.645, 0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, nhif_induction_AE= c("Cost", "QALYS")))
NHIF_Induction_AE_nonmedicalcost_RD = matrix(c(22677.24638,30007.87607,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, NHIF_Induction_AE_nonmedicalcost = c("Cost", "QALYS")))
CASH_Induction_AE_RD = matrix(c(16323.78485,23932.35883,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, CASH_Induction_AE = c("Cost", "QALYS")))
CASH_Induction_AE_nonmedical_cost_RD = matrix(c(21804.55408, 28846.15193,0, 0.81, 0.645,0), nrow = 3, ncol = 2, byrow = FALSE, dimnames = list(state = v_state_names, CASH_Induction_AE_nonmedical_cost = c("Cost", "QALYS")))

#View all the costs in RD
nhif_induction_AE_RD
NHIF_Induction_AE_nonmedicalcost_RD
CASH_Induction_AE_RD
CASH_Induction_AE_nonmedical_cost_RD

#VRD cost and QALYs for all state memberships 
payoff_nhif_induction_AE_VRD = state_membership_VRD %*% nhif_induction_AE_VRD
payoff_NHIF_Induction_AE_nonmedicalcost_VRD = state_membership_VRD %*% NHIF_Induction_AE_nonmedicalcost_VRD
payoff_CASH_Induction_AE_VRD = state_membership_VRD %*% CASH_Induction_AE_VRD
payoff_CASH_Induction_AE_nonmedical_cost_VRD = state_membership_VRD %*% CASH_Induction_AE_nonmedical_cost_VRD

#RD cost and QALYs for all state memberships 
payoff_nhif_induction_AE_RD = state_membership_VRD %*% nhif_induction_AE_RD
payoff_NHIF_Induction_AE_nonmedicalcost_RD = state_membership_VRD %*% NHIF_Induction_AE_nonmedicalcost_RD
payoff_CASH_Induction_AE_RD = state_membership_VRD %*% CASH_Induction_AE_RD
payoff_CASH_Induction_AE_nonmedical_cost_RD = state_membership_VRD %*% CASH_Induction_AE_nonmedical_cost_RD


#View the payoffs of all state memberships of VRD and RD
#VRD
payoff_nhif_induction_AE_VRD
payoff_NHIF_Induction_AE_nonmedicalcost_VRD
payoff_CASH_Induction_AE_VRD
payoff_CASH_Induction_AE_nonmedical_cost_VRD

#RD
payoff_nhif_induction_AE_RD
payoff_NHIF_Induction_AE_nonmedicalcost_RD
payoff_CASH_Induction_AE_RD
payoff_CASH_Induction_AE_nonmedical_cost_RD

#AVERAGE COST AND AVERAGE QALYS FOR EACH GROUP
#VRD
averagecost_nhif_induction_AE_VRD = colSums(payoff_nhif_induction_AE_VRD) / cohort_size 
averagecost_NHIF_Induction_AE_nonmedicalcost_VRD = colSums(payoff_NHIF_Induction_AE_nonmedicalcost_VRD) / cohort_size
averagecost_CASH_Induction_AE_VRD= colSums(payoff_CASH_Induction_AE_VRD) / cohort_size
averagecost_CASH_Induction_AE_nonmedical_cost_VRD = colSums(payoff_CASH_Induction_AE_nonmedical_cost_VRD) / cohort_size

#RD
averagecost_nhif_induction_AE_RD = colSums(payoff_nhif_induction_AE_RD) / cohort_size 
averagecost_NHIF_Induction_AE_nonmedicalcost_RD = colSums(payoff_NHIF_Induction_AE_nonmedicalcost_RD) / cohort_size
averagecost_CASH_Induction_AE_RD= colSums(payoff_CASH_Induction_AE_RD) / cohort_size
averagecost_CASH_Induction_AE_nonmedical_cost_RD = colSums(payoff_CASH_Induction_AE_nonmedical_cost_RD) / cohort_size

#VIEW THE AVERAGE COST PER PATIENT
#VRD
averagecost_nhif_induction_AE_VRD
averagecost_NHIF_Induction_AE_nonmedicalcost_VRD
averagecost_CASH_Induction_AE_VRD
averagecost_CASH_Induction_AE_nonmedical_cost_VRD

#RD
averagecost_nhif_induction_AE_RD
averagecost_NHIF_Induction_AE_nonmedicalcost_RD
averagecost_CASH_Induction_AE_RD
averagecost_CASH_Induction_AE_nonmedical_cost_RD



