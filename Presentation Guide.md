# Project Overview

**Evaluating Habitat Edge Influence on Bat Acoustic Activity Using a Negative Binomial GAMM**

---

## Objective

Determine whether bat acoustic activity (**total calls per night**) differs among **interior**, **edge**, and **open** detector placements, while properly accounting for **unequal recording effort**, **seasonal temporal change**, and the **hierarchical structure of detectors nested within sites**.

This project is structured to support poster-ready scientific inference.

---

## 1. Background & Justification

Acoustic monitoring is a core tool for assessing bat activity, yet observed call counts are strongly influenced by **detector placement**, **recording effort**, and **seasonal timing**. In particular, detectors placed along **habitat edges** may systematically over‑ or under‑estimate activity relative to forest interior or open habitats.

This project evaluates whether detector placement along a **habitat gradient (interior → edge → open)** influences measured bat activity after explicitly controlling for realistic field constraints.

### Habitat edges background: why we expect more calls on edges
Bat research often predicts **higher levels of acoustic activity** along **habitat edges** because edges combine **structural features** and **resource availability** from adjacent habitat types, creating favorable **foraging** and **commuting conditions** for many bat species. Edge zones tend to concentrate **insects** due to **vegetation gradients** and **microclimatic effects**, providing abundant prey, while still offering **open flight space** that allows bats to maneuver and detect prey efficiently using echolocation. In contrast, **dense forest interiors can constrain flight and echolocation** for edge- and open-adapted species, and **fully open habitats may lack the shelter and navigational cues bats** use for orientation and predator avoidance. As a result, habitat edges frequently function as ecological transition zones that **support higher bat activity and diversity than either forest interiors or open areas alone**.

|Challenge|Modeling Response|
|---|---|
|Overdispersed call counts|**Negative Binomial distribution**|
|Unequal nightly recording hours|**Offset: `log(recording_hours)`**|
|Seasonal activity decline (Oct–Nov)|**Smooth term on time**|
|Detectors nested within sites|**Hierarchical random effects**|
|Partial / short recording nights|**Retained with correct effort weighting**|

---

## 2. Response Variable & Effort

### 2.1 Primary response variable

- **calls_per_night**
    
    - Total number of bat call files recorded per detector per night
        
    - All species pooled
        

### 2.2 Why an offset is used (critical)

Recording duration varies substantially among nights (from very short partial nights to full ~13‑hour deployments). More recording hours naturally yield more detected calls, even when true activity rates are identical.

To prevent confounding **biological activity** with **sampling effort**, the model includes:

```r
offset(log(recording_hours))
```

This ensures that:

- Inference is made on **activity rate per unit time**
    
- Nights with fewer hours contribute **less information**, not bias
    
- Short nights (including 1–7 hour nights) remain statistically valid
    

> Conceptually, the model estimates **calls per hour**, even though the response is expressed as nightly counts.

---

## 3. Time Representation

### 3.1 Night variable

- Each night is represented as a calendar date
    
- Dates are converted to numeric time and **scaled** prior to modeling
    

```r
night_scaled = scale(as.numeric(night))
```

### 3.2 Why scaling is required

Scaling time:

- Improves model convergence and numerical stability
    
- Centers the model intercept on the **midpoint of the study period**
    
- Ensures smooth penalties behave appropriately
    

The intercept therefore represents expected log activity (per hour) for an **average night** in the study.

### 3.3 Smooth temporal effect

Rather than assuming a strictly linear seasonal trend, this project models time using a **penalized smooth**, allowing for:

- Non‑linear seasonal decline
    
- Plateaus or accelerations
    
- Data‑driven temporal structure
    

This is especially appropriate given:

- Short study duration
    
- Late‑season migration dynamics
    
- Sparse activity levels
    

---

## 4. Study Design

### 4.1 Spatial structure

- **3 independent sites** (>400 m apart)
    
- **3 detectors per site** (N = 9 detectors)
    

Habitat gradient:

- **Interior** — 50 m into forest, facing inward
    
- **Edge** — positioned at forest boundary, oriented parallel to edge
    
- **Open** — 50 m into open habitat, facing away from forest
    

### 4.2 Temporal structure

- **Oct 4 – Nov 1** (28 nights)
    
- Start‑time shift on Oct 23 (**18:00 → 17:30**) due to sunset
    
- Expected runtime: **~13 hours per night**
    

---

## 5. Research Question & Hypothesis

> **RQ:** Does detector placement (interior, edge, open) influence nightly bat activity after accounting for recording effort and seasonal timing?

- **H₀:** Habitat placement has no effect on bat activity.
    
- **H₁:** Habitat placement significantly affects bat activity.
    

No directional hypothesis is assumed.

---

## 6. Model Structure

Primary Negative Binomial GAMM:

```r
calls_per_night ~ habitat +
  s(night_scaled, k = 7) +
  offset(log(recording_hours)) +
  s(site, bs = "re") +
  s(detector_id, bs = "re")
```

|Component|Role|
|---|---|
|`calls_per_night`|Count response|
|`habitat`|Fixed effect of placement|
|`s(night_scaled)`|Seasonal temporal structure|
|`offset(log(recording_hours))`|Effort standardization|
|`s(site)`|Site‑level heterogeneity|
|`s(detector_id)`|Detector‑level variation|

- Framework: `mgcv::gam()`
    
- Distribution: Negative Binomial
    
- Inference: smooth significance, parametric tests, AIC
    
- α = 0.05
    

---

## 7. Limitations
- All species pooled; community differences not addressed
- Short seasonal window limits time generalization
- Offset assumes linear duration-activity scaling
- Partial nights add uncertainty but reflect real sampling
- Habitat categories based on deployment design, not field-verified vegetation structure
    

---

## 8. Summary (Poster sentence)

> We used a Negative Binomial GAMM to test whether detector placement (interior, edge, open) influences bat acoustic activity while accounting for unequal recording effort, seasonal timing, and the nested structure of detectors within sites.

---

## 9. Results (Plain-Language Takeaways)

### 9.1 Headline result

- **Main point:** The analysis is not just about modeling overall activity; the core objective is the **pairwise comparison** among **interior, edge, and open** placements.
- **Top-line finding:** **Open vs Interior is higher and significant** (IRR = 5.37, 95% CI: 1.21–23.84, p = 0.027).
- **Secondary findings:** **Edge vs Interior is higher but not significant** (IRR = 1.75, 95% CI: 0.39–7.81, p = 0.463), and **Open vs Edge is higher but not significant** (IRR = 3.07, 95% CI: 0.68–13.80, p = 0.143).

### 9.2 Effect size in plain language

After accounting for recording effort and seasonal timing, **open** recorded **437% more calls per hour** than **interior** (model-estimated).

### 9.3 Model-based certainty

- Pairwise results use model-based estimates with 95% confidence intervals.
- Differences account for uneven effort and seasonal change.

---

## 10. Results (Pairwise Comparison Table)

|Comparison|Direction|Significant?|Plain-language translation|
|---|---|---|---|
|Edge vs Interior|Edge higher|No|Edge is 75% higher calls/hr than Interior (IRR 1.75, p = 0.463)|
|Edge vs Open|Open higher|No|Open is 207% higher calls/hr than Edge (IRR 3.07, p = 0.143)|
|Interior vs Open|Open higher|Yes|Open is 437% higher calls/hr than Interior (IRR 5.37, p = 0.027)|

---

## 11. Presenter Scripts (30s / 1 min / 3 min)

These are designed for entry-level presenters. Speak clearly, keep it natural, and emphasize the pairwise comparisons.

### 11.1 30-second version

This poster tests whether bat activity differs by detector placement: **interior, edge, or open**. We used a Negative Binomial GAMM with an **offset for recording hours** and a **smooth seasonal term**, plus random effects for site and detector. The key point is the **pairwise comparison** among the three habitats, not just overall activity. Our results show **Open higher than Interior (IRR 5.37, p = 0.027)**, **Edge higher than Interior but not significant (IRR 1.75, p = 0.463)**, and **Open higher than Edge but not significant (IRR 3.07, p = 0.143)**.

### 11.2 1-minute version

We asked whether detector placement along a habitat gradient affects bat activity. The response is **total calls per night**, but we correct for unequal effort using an **offset of log(recording hours)**, and we model seasonal change with a smooth term on night. The model is a Negative Binomial GAMM with random effects for **site and detector**. The main goal is the **pairwise comparison** among **interior, edge, and open** placements, not just population-level activity. Our results show **Open higher than Interior (IRR 5.37, p = 0.027)**, **Edge higher than Interior but not significant (IRR 1.75, p = 0.463)**, and **Open higher than Edge but not significant (IRR 3.07, p = 0.143)**. In plain terms, **open habitat recorded about 437% more calls per hour than interior**, even after accounting for effort and seasonal timing.

### 11.3 3-minute version

This project evaluates whether bat acoustic activity changes across a habitat gradient: **interior, edge, and open** placements. We used nightly call counts, but because effort varied widely among nights, we included an **offset for recording hours** so the model estimates **calls per hour**. We also modeled seasonal change with a **penalized smooth** of night and included random effects for **detectors nested within sites**. The distribution is **Negative Binomial** to handle overdispersion.

The key point is that this is not just a population activity model. The primary inference is the **pairwise comparison** among the three habitat placements. The results show **Open higher than Interior (IRR 5.37, p = 0.027)**, **Edge higher than Interior but not significant (IRR 1.75, p = 0.463)**, and **Open higher than Edge but not significant (IRR 3.07, p = 0.143)**. Put simply, **open habitat recorded about 437% more calls per hour than interior**, and that difference is model-based after controlling for effort and seasonal timing. This matters for monitoring design, because detector placement can systematically influence activity estimates.