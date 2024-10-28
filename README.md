# Markov Model Analysis of Chemotherapy Options
## Project Overview
This project employs a Markov model to analyze the cost-effectiveness of two chemotherapy regimens (VRD and RD) for treating patients with multiple myeloma. The model incorporates various parameters, including treatment states, costs, quality-adjusted life years (QALYs), and payment options. The analysis is conducted over a six-year life horizon with 72 cycles, each representing a month.

## Key Components
- State Definitions:
  - States: The model consists of three health states: Progression-Free Survival (PFS), Progressive Disease (PD), and Death.
  - Cohort Size: A cohort of 1,000 patients is used to simulate the outcomes.

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

## Analysis
The analysis focuses on comparing the cost-effectiveness of VRD and RD regimens by evaluating their average costs and QALYs. Key points include:
- Cost Comparison: By examining the average costs associated with each treatment option under different payment methods (NHIF and cash), insights into financial burdens on patients and healthcare systems can be assessed.
- QALY Assessment: The QALYs derived from each regimen will help evaluate the quality of life years gained, providing a metric for comparing the health benefits of each treatment option.
- Incremental Cost-Effectiveness Ratio (ICER): The ICER can be calculated to determine the additional cost per QALY gained for one treatment over the other, informing which regimen offers better value for money.
- Sensitivity Analysis: Conducting sensitivity analyses will assess how changes in key parameters (e.g., transition probabilities, costs) affect outcomes, identifying critical factors that influence the cost-effectiveness of treatment options.

## Expected Outcomes
The expected outcomes from the analysis include:
- Average Costs: Clear insights into the average treatment costs per patient for both VRD and RD regimens, highlighting the financial implications for different payment methods.
- Quality-Adjusted Life Years (QALYs): Estimates of QALYs gained for each treatment option, indicating the expected health benefits and overall effectiveness of the treatments.
- Cost-Effectiveness Analysis: A comparative framework that presents the cost per QALY for each regimen, allowing healthcare providers to make informed decisions regarding treatment options based on value.

## Recommendations: 
Based on the findings, recommendations can be made regarding the preferred treatment option, considering both cost and patient outcomes. This may include suggestions for policy changes or adjustments in treatment protocols based on economic evaluations.

## Visualizations: 
Graphical representations of the transition matrices, cost distributions, and QALYs will enhance understanding and communication of the results to stakeholders.

## Conclusion
This Markov model framework facilitates a comprehensive evaluation of the cost-effectiveness of chemotherapy options for multiple myeloma patients. By analyzing transition probabilities, patient outcomes, and associated costs, the project aims to inform healthcare decision-making and optimize resource allocation in oncology. Future steps may include sensitivity analyses and incorporating additional variables to enhance the model's robustness and applicability.
