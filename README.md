# FIFA-Wages Analysis & Offline A/B-Test Simulation

> Explore wage drivers in *FIFA 23* data & simulate a true A/B experiment that randomises contract length.

---

## Project Goals
| ID | Objective |
|----|-----------|
| **O1** | Compare wages of long (≥ 2026) vs. short contracts. |
| **O2** | Model wage as a function of age, international reputation, potential, and value. |
| **O3** | Demonstrate an A/B-testing workflow in **R** and estimate the wage “lift” if clubs switch to longer deals. |

---

## Methodology
1. **Data prep** – clean monetary fields, compute `log_wage`.
2. **Diagnostics** – Shapiro-Wilk, Q-Q plots, variance tests, *t* / Mann-Whitney.
3. **Model** – Gaussian GLM:  

   \[
   \log_{10}(\text{Wage}) \sim \text{Age} + \text{IntlRep} + \text{Potential} + \text{Value} + \text{ContractGroup}
   \]

4. **Missing-data robustness** – mean & median imputation, re-fit GLM.
5. **Offline A/B simulation** – randomly force half the rows to `Contract.Valid.Until = 2027`, predict wages with the frozen GLM, test for lift, run 1 000 Monte-Carlo replications.

---

## Key Results
| Hypothesis | Outcome |
|------------|---------|
| **H1** | No sig. wage gap between long & short contracts. |
| **H2** | Age, Intl Rep, Potential, Value all ↑ wage (p < 0.001); Contract length non-sig. |
| **A/B** | Mean simulated lift ≈ +1 %; only 22 % of sims reach p < 0.05 → echoes observational result. |

---
