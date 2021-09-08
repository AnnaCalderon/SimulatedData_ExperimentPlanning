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
```{r}
# p1 = control = no incentive
# p2 = treatment = with incentive 

library(pwr)
power.prop.test(p1= .32, p2= (.32 + 0.32*0.30), sig.level = 0.05, power = .8)

```
## Part 2: Simulation 
### Scenario 1: No Effect Simulation
```{r}
library(data.table)
library(DT)
library(purrr)

n <- 790 
set.seed(seed = 329)

bp.dat <- data.table(Group = sample(x = c("Treatment", "Control"), size = n, replace = T))

bp.dat[Group == "Treatment", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)]
bp.dat[Group == "Control", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)] 

table(bp.dat)
```
```{r}
           VR
Group         0   1
  Control   281 137
  Treatment 246 126
```

```{r}
prop.test(table(bp.dat$Group, bp.dat$VR))
```
```{r}
    2-sample test for equality of proportions with continuity correction

data:  table(bp.dat$Group, bp.dat$VR)
X-squared = 0.062809, df = 1, p-value = 0.8021
alternative hypothesis: two.sided
95 percent confidence interval:
 -0.05744408  0.07936104
sample estimates:
   prop 1    prop 2 
0.6722488 0.6612903 
```
```{r}
analyze.experiment <- function(the.dat) {
  setDT(the.dat)

  the.test <- prop.test(table(the.dat$Group, the.dat$VR))
 
  the.effect <- the.test$estimate[1] - the.test$estimate[2] 
  lower.bound <- the.test$conf.int[1] 
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, lower_ci = lower.bound, p = p)
  
  return(result)
}

analyze.experiment(the.dat = bp.dat)
```
```{r}
       effect    lower_ci         p
1: 0.01095848 -0.05744408 0.8021103
```
```{r}
table(bp.dat)
```
```{r}
           VR
Group         0   1
  Control   281 137
  Treatment 246 126
```
```{r}
prop.test(table(bp.dat$Group, bp.dat$VR))
```
**Scenario 1 Analysis**
```{r}
n <- 790 
B <- 1000 
RNGversion(vstr = 3.6)
set.seed(seed = 4172)

Experiment <- rep.int(x = 1:B, times = n)
Group <- sample(x = c("Treatment", "Control"), size = n*B, replace = T)

sim.dat <- data.table(Experiment, Group)
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Treatment", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)]
sim.dat[Group == "Control", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)]

dim(sim.dat)
```
```{r}
[1] 790000      3
```
```{r}
exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"] 

DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
    rownames = F)
```
<img width="922" alt="Screen Shot 2021-09-08 at 7 36 22 PM" src="https://user-images.githubusercontent.com/52983514/132599264-e6fc5456-3439-4a36-b456-af81cdf146ea.png">

```{r}
exp.results[, mean(p < 0.05)]
```
```{r}
[1] 0.035
```
```{r}
exp.results[, summary(effect)]
```
```{r}
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-0.101412 -0.021927  0.001065  0.000511  0.023402  0.125469
```
```{r}
exp.results[, summary(lower_ci)]
```
```{r}
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.16727 -0.08955 -0.06528 -0.06695 -0.04394  0.05961 
```
### Scenario 1: No Effect Simulation
```{r}
n <- 790 
set.seed(seed = 329)

bp.dat <- data.table(Group = sample(x = c("Treatment", "Control"), size = n, replace = T))
bp.dat[Group == "Treatment", VR := round(x = rbernoulli(n = .N, p=.32 + 0.32*0.30), digits = 1)]
bp.dat[Group == "Control", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)] 

table(bp.dat)
```
```{r}
           VR
Group         0   1
  Control   281 137
  Treatment 207 165
```
```{r}
prop.test(table(bp.dat$Group, bp.dat$VR))
```
```{r}
    2-sample test for equality of proportions with continuity correction

data:  table(bp.dat$Group, bp.dat$VR)
X-squared = 10.692, df = 1, p-value = 0.001076
alternative hypothesis: two.sided
95 percent confidence interval:
 0.04562873 0.18596565
sample estimates:
   prop 1    prop 2 
0.6722488 0.5564516 
```
```{r}
analyze.experiment <- function(the.dat) {
  setDT(the.dat)
  
  the.test <- prop.test(table(the.dat$Group, the.dat$VR))
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2] 
  lower.bound <- the.test$conf.int[1] 
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, lower_ci = lower.bound, p = p)
  
  return(result)
}

analyze.experiment(the.dat = bp.dat)
```
```{r}
      effect   lower_ci           p
1: 0.1157972 0.04562873 0.001076139
```
**Scenario 1 Analysis**
```{r}
n <- 820 
B <- 1000 

RNGversion(vstr = 3.6)
set.seed(seed = 4172)

Experiment <- rep.int(x = 1:B, times = n)
Group <- sample(x = c("Treatment", "Control"), size = n*B, replace = T)

sim.dat <- data.table(Experiment, Group)
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Treatment", VR := round(x = rbernoulli(n = .N, p=.32 + 0.32*0.30), digits = 1)]
sim.dat[Group == "Control", VR := round(x = rbernoulli(n = .N, p=.32), digits = 1)]

dim(sim.dat)
```
```{r}
[1] 820000      3
```
```{r}
names(sim.dat)
```
```{r}
[1] "Experiment" "Group"      "VR"    
```
```{r}
exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"] 


DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
    rownames = F)
```
<img width="922" alt="Screen Shot 2021-09-08 at 7 36 22 PM" src="https://user-images.githubusercontent.com/52983514/132599852-b8f7a0ee-29e0-45f8-8a75-a5076bed0e87.png">

```{r}
exp.results[, mean(p < 0.05)]
```
```{r}
[1] 0.801
```
```{r}
exp.results[, summary(effect)]
```
```{r}
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.03328  0.07413  0.09578  0.09614  0.11955  0.21595 
```
```{r}
exp.results[, summary(lower_ci)]
```
```{r}
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-0.100707  0.006117  0.027686  0.028056  0.051713  0.148724 
```

## References 
[## References](file:///Users/annacalderon/Documents/DESKTOP/APAN/Summer%202021/Research%20Design/Group%20Project/Final%20Submission/Group-5---Final-Project-Submission_FINAL.html)

