# Markov Model Analysis of Chemotherapy Options
## Project Overview
This project employs a Markov model to analyze the cost-effectiveness of two chemotherapy regimens (VRD and RD) for treating patients with multiple myeloma. The model incorporates various parameters, including treatment states, costs, quality-adjusted life years (QALYs), and payment options. The analysis is conducted over a six-year life horizon with 72 cycles, each representing a month.

## Key Components
- State Definitions:
-- States: The model consists of three health states: Progression-Free Survival (PFS), Progressive Disease (PD), and Death.
-- Cohort Size: A cohort of 1,000 patients is used to simulate the outcomes.

- Transition Matrices:
Two transition matrices are defined for the VRD and RD treatment options, specifying the probabilities of moving between health states each cycle.
The matrices are visualized using plots saved as PNG files for VRD and RD.

- State Membership:
An array is initialized to track the number of patients in each state for both treatments over 72 cycles.
The initial state for all patients is set to PFS, and the subsequent states are calculated using matrix multiplication based on the transition probabilities.

- Payoff Matrices:
Cost and QALY data are captured in matrices for each treatment regimen, covering induction, maintenance, adverse events, and non-medical costs.
Each matrix provides values for costs and QALYs associated with each state for both NHIF and cash payment options.

- Cost and QALY Calculations:
The model calculates the total costs and QALYs for both treatment regimens based on the state memberships and payoff matrices.
Average costs per patient for each payment option are computed, providing insights into the financial implications of each treatment strategy.

- Outputs:
The project outputs average costs and QALYs for both VRD and RD options, allowing for a comparative analysis of the two treatment pathways.

- Conclusion
This Markov model framework facilitates a comprehensive evaluation of the cost-effectiveness of chemotherapy options for multiple myeloma patients. By analyzing transition probabilities, patient outcomes, and associated costs, the project aims to inform healthcare decision-making and optimize resource allocation in oncology. Future steps may include sensitivity analyses and incorporating additional variables to enhance the model's robustness and applicability.
