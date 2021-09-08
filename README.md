# Simulated Experiemnt: Tailored Incentives to Increase COVID-19 Vaccination Rates
## Overview
- Identified the optimal sample size for the desired significance level and statistical power
- Generated a realistic data set using simulation techniques for study design
- Created two scenarios: one with the expected effect and another with no effect
- Analyzed the simulation data in both scenarios

## Part 1: Research Proposal
### Background
- There is vast literature suggesting that incentive rewards motivate people to change health behaviors (Seal et al., 2003; Gong et al., 2018)
- A survey experiment by the U.C.L.A. suggests that approximately a third of the U.S. unvaccinated population reported they would be more likely to get the vaccine with a monetary incentive (New York Times, 2021)
- The literature also suggests that tailored interventions have the potential to improve health behavior (Krebs et al., 2010; McCurley et al., 2017)

### Problem Statement
Louisiana has the 4th lowest COVID-19 vaccine rate, with only 42% of the adult population with at least one vaccine dose

### Research Question & Hypotheses
**Research Question:**\
What effect do tailored incentives have on the COVID-19 vaccine rate?

**Incentive Offer:**
- Lottery entry with five winners selected at random, who will each receive 2 Saints football season tickets for the 2021 season
- 75% of New Orleans residents identify as Saints fans. Top 3 most passionate sports fans in America

**Null Hypothesis:**
The proportion of participants who get at least one dose of the COVID-19 vaccine will be the same between the control group (defined as participants who were mailed a flyer without an incentive) and the treatment group (defined as the participants who were sent a flyer with an incentive)

**Alternative Hypothesis:**
The proportion of participants who get at least one dose of the COVID-19 vaccine will not be the same between the control group (defined as participants who were mailed a flyer without an incentive) and the treatment group (defined as the participants who were sent a flyer with an incentive)

### Research Plan 
**The Population of Interest:**
Gen Z & Millennials who live in New Orleans that have not yet been vaccinated
- New Orleans has been the hotspot in Louisiana, with 17% of the state's COVID-19 cases
- Only 32% of Gen Z & Millennials are partially vaccinated, compared to 64% of adults 40 years and older
<img width="500" alt="Screen Shot 2021-09-08 at 6 38 01 PM" src="https://user-images.githubusercontent.com/52983514/132595000-87dbb452-1b01-4d8d-b8dc-71f16142fdba.png">

**Operational Procedures:**

<img width="772" alt="Screen Shot 2021-09-08 at 6 40 32 PM" src="https://user-images.githubusercontent.com/52983514/132595172-d48f1d1e-300f-4c74-b81b-93bb701720aa.png">

**Statistical Plan:**

**Sample Size Estimation:**
- Current vaccination rate = 32%
- Estimated treatment rate = 42% 
  - A recent study suggests that 34% of unvaccinated adults would be more likely to get vaccinated with a cash payment [(New York Times, 2021)](https://www.nytimes.com/2021/05/04/upshot/vaccine-incentive-experiment.html)
  - For the present simulation, we used 32% as the expected additional vaccination probability for the treatment group 
  - We used 32% instead of 34% to account for the difference between what participants said they will do vs. what they do
```{r q1_scenario2_analysis}
# p1 = control = no incentive
# p2 = treatment = with incentive 

library(pwr)
power.prop.test(p1= .32, p2= (.32 + 0.32*0.30), sig.level = 0.05, power = .8)

```
## Part 2: Simulation 
