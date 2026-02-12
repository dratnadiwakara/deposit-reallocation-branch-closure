
# 1. Branch-Year Panel: Deposit Growth of Surviving Branches

This section summarizes results from the branch-level panel analysis.

**Basic idea.** The analysis uses a **branch–year** panel to study how deposit growth at surviving branches relates to the intensity of branch closures in the same county. The focus is on **incumbent** banks (those that did not close branches in that county-year) and on reallocation of deposits from closed branches to surviving branches across banks within a county.

**Unit of observation.** Branch–year. Each observation is a branch that existed at both *t*−1 and *t*+1 (survivors). The main regressor is the county-year share of deposits held by branches that closed, measuring closure exposure in the local market.

**Outcome.** Branch deposit growth (`gr_branch`): change in branch deposits from *t*−1 to *t*+1 divided by *t*−1 deposits.

**Main regressor.** County-year share of deposits held by branches that closed (`share_deps_closed`), i.e., deposits at closed branches as a fraction of total county deposits at *t*−1.

**Sample construction.** Incumbent banks only. Branch-year observations where the branch existed at both *t*−1 and *t*+1. Counties with at least 3 banks (lagged). Years 2012–2025.

---

## 1.1 Baseline result

**Regression.** OLS with fixed effects. Dependent variable: branch deposit growth (`gr_branch`), defined as the change in branch deposits from t−1 to t+1 divided by t−1 deposits. Main regressor: county-year share of deposits held by branches that closed (`share_deps_closed`), i.e., deposits at closed branches as a fraction of total county deposits at t−1. Controls: `log1p(banks_county_lag1)`, `county_dep_growth_t4_t1`, `log1p(dep_lag1_aligned)`. Fixed effects: branch (UNINUMBR), state×year, bank×year. Standard errors clustered by branch.

**Sample.** Incumbent banks only (banks that did not close any branches in that county-year). Branch-year observations where the branch existed at both t−1 and t+1. Counties with at least 3 banks (lagged). Years 2012–2025. Model 1: 2012–2019; Model 2: 2020–2025.



```
                                     model 1             model 2
                                   2012-2019           2020-2025
Dependent Var.:                    gr_branch           gr_branch
                                                                
share_deps_closed         0.0709*** (0.0182)     0.0099 (0.0204)
log1p(banks_county_lag1)     0.0094 (0.0112)  -0.0517** (0.0169)
county_dep_growth_t4_t1      0.0022 (0.0020)    6.73e-6 (0.0005)
log1p(dep_lag1_aligned)  -0.5085*** (0.0062) -0.6131*** (0.0117)
Fixed-Effects:           ------------------- -------------------
UNINUMBR                                 Yes                 Yes
state_yr                                 Yes                 Yes
bank_yr                                  Yes                 Yes
________________________ ___________________ ___________________
S.E.: Clustered                 by: UNINUMBR        by: UNINUMBR
Observations                         530,646             278,933
R2                                   0.68058             0.76632
Within R2                            0.30307             0.35500
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## 1.2 Closure Exposure × Bank Type

**Regression.** Same as above, but `share_deps_closed` is interacted with bank type (`bank_type_fct`): top-4 banks, large-but-not-top-4 (assets > $100B), and other (reference). The interaction allows the association between closure exposure and deposit growth to vary by incumbent bank type.

**Sample.** Same as above.

```

                                                                model 1             model 2
                                                              2012-2019           2020-2025
Dependent Var.:                                               gr_branch           gr_branch
                                                                                           
share_deps_closed                                    0.0909*** (0.0232)    0.0438. (0.0241)
bank_type_fcttop4                                      7.667*** (1.012)      0.7558 (1.702)
bank_type_fctlarge_but_not_top4                         -0.4951 (1.291)       2.206 (1.423)
log1p(banks_county_lag1)                                0.0097 (0.0112)  -0.0524** (0.0169)
county_dep_growth_t4_t1                                 0.0022 (0.0020)    9.14e-6 (0.0005)
log1p(dep_lag1_aligned)                             -0.5085*** (0.0062) -0.6131*** (0.0117)
share_deps_closed x bank_type_fcttop4                 -0.1107* (0.0447)   -0.0919. (0.0503)
share_deps_closed x bank_type_fctlarge_but_not_top4    -0.0203 (0.0473)    -0.0905 (0.0565)
Fixed-Effects:                                      ------------------- -------------------
UNINUMBR                                                            Yes                 Yes
state_yr                                                            Yes                 Yes
bank_yr                                                             Yes                 Yes
________________________________________            ___________________ ___________________
S.E.: Clustered                                            by: UNINUMBR        by: UNINUMBR
Observations                                                    530,646             278,933
R2                                                              0.68061             0.76633
Within R2                                                       0.30312             0.35502
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## 1.3 Closure Exposure by Closer Type and Incumbent App Rating

**Regression.** Same dependent variable, controls, and fixed effects as above. Closure exposure is decomposed by the type of the closing bank. Model 1 (2012–2019): `share_deps_closed_app` (non–top-4 banks with an app), `share_deps_closed_noapp` (banks without an app), `share_deps_closed_top4`. Model 2 (2020–2025): `share_deps_closed_small`, `share_deps_closed_large_but_not_top4`, `share_deps_closed_top4` (by closer size). Model 3 (2020–2025): same decomposition as Model 2, with each share interacted with the incumbent bank’s app rating (`own_bank_app_rating`).

**Sample.** Models 1–2: same incumbent sample as above. Model 3: incumbent banks in 2020–2025 with `own_bank_app_rating > 1` (banks that have an app and a non-missing rating).

```
                                                                       model 1             model 2             model 3
                                                                     2012-2019           2020-2025           2020-2025
Dependent Var.:                                                      gr_branch           gr_branch           gr_branch
                                                                                                                      
share_deps_closed_app                                       0.1028*** (0.0238)                                        
share_deps_closed_noapp                                      0.1393** (0.0474)                                        
share_deps_closed_top4                                        -0.0118 (0.0340)    -0.0486 (0.0311)     0.0589 (0.1731)
own_bank_app_rating x share_deps_closed_top4                                                          -0.0240 (0.0444)
share_deps_closed_large_but_not_top4                                              0.0779. (0.0401)     0.0155 (0.1653)
own_bank_app_rating x share_deps_closed_large_but_not_top4                                             0.0199 (0.0472)
share_deps_closed_small                                                            0.0318 (0.0365)    -0.2026 (0.1769)
share_deps_closed_small x own_bank_app_rating                                                          0.0488 (0.0467)
log1p(banks_county_lag1)                                       0.0096 (0.0112)  -0.0513** (0.0169) -0.0619*** (0.0185)
county_dep_growth_t4_t1                                        0.0022 (0.0020)    8.55e-6 (0.0005)    4.75e-6 (0.0007)
log1p(dep_lag1_aligned)                                    -0.5085*** (0.0062) -0.6130*** (0.0117) -0.6412*** (0.0144)
own_bank_app_rating                                                                                    0.0713 (0.6886)
Fixed-Effects:                                             ------------------- ------------------- -------------------
UNINUMBR                                                                   Yes                 Yes                 Yes
state_yr                                                                   Yes                 Yes                 Yes
bank_yr                                                                    Yes                 Yes                 Yes
________________________________________                   ___________________ ___________________ ___________________
S.E.: Clustered                                                   by: UNINUMBR        by: UNINUMBR        by: UNINUMBR
Observations                                                           530,646             278,933             172,412
R2                                                                     0.68059             0.76634             0.78212
Within R2                                                              0.30309             0.35503             0.37403
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1    
```                                                                       

## 1.4 Closure Exposure × Incumbent vs. Closer-Remaining

**Regression.** Same specification as the original, but `share_deps_closed` is interacted with `closer_remaining_branch` (1 if the branch belongs to a bank that closed some branches in that county-year, 0 if incumbent). The interaction allows the association between closure exposure and deposit growth to differ for incumbent branches vs. remaining branches of banks that closed other branches in the county.

**Sample.** All banks (incumbent and closer), i.e., full `reg_dt`. Includes both banks that did not close in the county-year and banks that closed some branches but kept others open. Model 1: 2012–2019; Model 2: 2020–2025.

```
                                                        model 1             model 2
                                                      2012-2019           2020-2025
Dependent Var.:                                       gr_branch           gr_branch
                                                                                   
share_deps_closed                            0.0651*** (0.0174)     0.0022 (0.0191)
closer_remaining_branch                      0.0400*** (0.0014)  0.0444*** (0.0022)
log1p(banks_county_lag1)                        0.0027 (0.0099)   -0.0321* (0.0153)
county_dep_growth_t4_t1                        0.0039* (0.0016)    -0.0003 (0.0004)
log1p(dep_lag1_aligned)                     -0.5271*** (0.0056) -0.6296*** (0.0106)
share_deps_closed x closer_remaining_branch  0.4979*** (0.0426)  0.4341*** (0.0422)
Fixed-Effects:                              ------------------- -------------------
UNINUMBR                                                    Yes                 Yes
state_yr                                                    Yes                 Yes
bank_yr                                                     Yes                 Yes
________________________________________    ___________________ ___________________
S.E.: Clustered                                    by: UNINUMBR        by: UNINUMBR
Observations                                            675,022             361,678
R2                                                      0.64665             0.73833
Within R2                                               0.30224             0.34868
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## 1.5 Closure Exposure by Closer Type × Incumbent Bank Type

**Regression.** Combines the decomposition by closer type with interactions by incumbent bank type. Model 1 (2012–2019): `share_deps_closed_app`, `share_deps_closed_noapp`, `share_deps_closed_top4`, each interacted with `bank_type_fct` (top-4, large-but-not-top-4, other). Model 2 (2020–2025): `share_deps_closed_small`, `share_deps_closed_large_but_not_top4`, `share_deps_closed_top4`, each interacted with `bank_type_fct`. Allows the association between each type of closure exposure and deposit growth to vary by incumbent bank type.

**Sample.** Incumbent banks only. Same as above. Model 1: 2012–2019; Model 2: 2020–2025.

```

                                                                                   model 1             model 2
                                                                                 2012-2019           2020-2025
Dependent Var.:                                                                  gr_branch           gr_branch
                                                                                                              
share_deps_closed_app                                                   0.1270*** (0.0301)                    
share_deps_closed_app x bank_type_fcttop4                                -0.1289* (0.0550)                    
share_deps_closed_app x bank_type_fctlarge_but_not_top4                   -0.0325 (0.0626)                    
share_deps_closed_noapp                                                   0.1223* (0.0570)                    
bank_type_fcttop4 x share_deps_closed_noapp                               -0.0028 (0.1616)                    
bank_type_fctlarge_but_not_top4 x share_deps_closed_noapp                  0.0812 (0.1159)                    
share_deps_closed_top4                                                     0.0103 (0.0442)     0.0249 (0.0374)
bank_type_fcttop4 x share_deps_closed_top4                                -0.0922 (0.0793)    -0.0446 (0.0672)
bank_type_fctlarge_but_not_top4 x share_deps_closed_top4                  -0.0305 (0.0885)  -0.2230** (0.0797)
share_deps_closed_large_but_not_top4                                                          0.0841. (0.0462)
bank_type_fcttop4 x share_deps_closed_large_but_not_top4                                      -0.1404 (0.1196)
bank_type_fctlarge_but_not_top4 x share_deps_closed_large_but_not_top4                         0.0759 (0.1146)
share_deps_closed_small                                                                        0.0216 (0.0432)
share_deps_closed_small x bank_type_fcttop4                                                   -0.1049 (0.0941)
share_deps_closed_small x bank_type_fctlarge_but_not_top4                                      0.1265 (0.1051)
bank_type_fcttop4                                                         7.669*** (1.012)      0.7500 (1.702)
bank_type_fctlarge_but_not_top4                                            -0.4956 (1.291)       2.205 (1.426)
log1p(banks_county_lag1)                                                   0.0098 (0.0112)  -0.0522** (0.0169)
county_dep_growth_t4_t1                                                    0.0022 (0.0020)    7.83e-6 (0.0005)
log1p(dep_lag1_aligned)                                                -0.5085*** (0.0062) -0.6130*** (0.0117)
Fixed-Effects:                                                         ------------------- -------------------
UNINUMBR                                                                               Yes                 Yes
state_yr                                                                               Yes                 Yes
bank_yr                                                                                Yes                 Yes
________________________________________                               ___________________ ___________________
S.E.: Clustered                                                               by: UNINUMBR        by: UNINUMBR
Observations                                                                       530,646             278,933
R2                                                                                 0.68062             0.76637
Within R2                                                                          0.30314             0.35512
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1        
```


## 1.6 Closure Exposure by Closer Type × Incumbent vs. Closer-Remaining

**Regression.** Decomposes closure exposure by closer type, with each share interacted with `closer_remaining_branch`. Model 1 (2012–2019): `share_deps_closed_app`, `share_deps_closed_noapp`, `share_deps_closed_top4`, each × `closer_remaining_branch`. Model 2 (2020–2025): `share_deps_closed_small`, `share_deps_closed_large_but_not_top4`, `share_deps_closed_top4`, each × `closer_remaining_branch`. Allows the association between each type of closure exposure and deposit growth to differ for incumbent branches vs. remaining branches of closers.

**Sample.** All banks (incumbent and closer). Same as the preceding incumbent vs. closer-remaining regression. Model 1: 2012–2019; Model 2: 2020–2025.

```

                                                                           model 1             model 2
                                                                         2012-2019           2020-2025
Dependent Var.:                                                          gr_branch           gr_branch
                                                                                                      
share_deps_closed_app                                           0.0813*** (0.0225)                    
share_deps_closed_app x closer_remaining_branch                 0.7668*** (0.0652)                    
share_deps_closed_noapp                                          0.1273** (0.0482)                    
closer_remaining_branch x share_deps_closed_noapp                1.257*** (0.1636)                    
share_deps_closed_top4                                             0.0026 (0.0319)    -0.0417 (0.0291)
closer_remaining_branch x share_deps_closed_top4                 0.1528** (0.0591)  0.3351*** (0.0732)
share_deps_closed_large_but_not_top4                                                   0.0507 (0.0375)
closer_remaining_branch x share_deps_closed_large_but_not_top4                      0.4789*** (0.0672)
share_deps_closed_small                                                                0.0202 (0.0337)
share_deps_closed_small x closer_remaining_branch                                   0.4773*** (0.0748)
closer_remaining_branch                                         0.0365*** (0.0014)  0.0445*** (0.0022)
log1p(banks_county_lag1)                                           0.0034 (0.0099)   -0.0314* (0.0153)
county_dep_growth_t4_t1                                           0.0038* (0.0016)    -0.0003 (0.0004)
log1p(dep_lag1_aligned)                                        -0.5270*** (0.0056) -0.6296*** (0.0106)
Fixed-Effects:                                                 ------------------- -------------------
UNINUMBR                                                                       Yes                 Yes
state_yr                                                                       Yes                 Yes
bank_yr                                                                        Yes                 Yes
________________________________________                       ___________________ ___________________
S.E.: Clustered                                                       by: UNINUMBR        by: UNINUMBR
Observations                                                               675,022             361,678
R2                                                                         0.64683             0.73835
Within R2                                                                  0.30260             0.34873
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1                                                                            
```


# 2. Bank–County Panel: Deposit Retention Through Remaining Branches

This section summarizes results from the bank–county–year panel analysis.

**Basic idea.** The analysis builds a **bank–county–year** panel to study how deposits at **remaining branches** evolve following branch closures. Deposits are observable only through branches; if a bank closes all branches in a county and retains deposits digitally, those deposits do not appear in branch-level data. The analysis is therefore best interpreted as **deposit retention or reallocation through remaining local branches**.

**Unit of observation.** Bank–county–year. For each bank in each county in each year, we aggregate:
- **Closed branches**: branches that close in year *t* and had deposits at *t*−1.
- **Remaining branches**: branches that survive year *t* and exist at both *t*−1 and *t*+1 (so we can measure deposit outcomes consistently around the closure event).

> **Note.** Only about 14% of bank–county–years with closures are full exits (no remaining branches); **the vast majority retain at least one branch** in the county.

**Outcome.** Log growth of deposits at remaining branches from *t*−1 to *t*+1 or *t*+3 (`dlog_deps_remaining` or `dlog_deps_remaining_t3`).

**Main regressor.** Log of deposits at closed branches at *t*−1 (`log1p(deps_closed_t1)`).

**Sample construction.**
- Base sample: bank–county–years in 2001–2024 with at least one remaining branch and non-missing deposit growth.
- Main sample: base sample excluding "extreme intensity" observations—where closure intensity (`deps_closed_t1` / `deps_remain_t1`) is in the top 5% or below 0.01.
- Fixed effects: bank×year and county×year. Standard errors clustered by bank.

---

## 2.1 Main Regressions: Deposit Retention at Remaining Branches

**Regression.** OLS with fixed effects. Dependent variable: log growth of deposits at remaining branches from *t*−1 to *t*+3 (`dlog_deps_remaining_t3`). Main regressor: log of deposits at closed branches at *t*−1 (`log1p(deps_closed_t1)`). Control: `log1p(deps_remain_t1)`. Fixed effects: bank×year and county×year. Standard errors clustered by bank.

**Sample.** Model 1 (Base): base sample. Model 2 (Main): main sample (excludes extreme intensity). Models 3–5: main sample by subperiod—pre 2012, 2012–2019, 2020–2024.

```
                                     model 1                model 2                model 3                model 4                model 5
                                        Base                   Main               Pre 2012              2012-2019              2020-2024
Dependent Var.:       dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3
                                                                                                                                        
log1p(deps_closed_t1)     0.0171*** (0.0005)     0.0141*** (0.0005)     0.0122*** (0.0008)     0.0132*** (0.0007)     0.0167*** (0.0011)
log1p(deps_remain_t1)    -0.1415*** (0.0043)    -0.1410*** (0.0043)    -0.1633*** (0.0059)    -0.1205*** (0.0058)    -0.1312*** (0.0082)
Fixed-Effects:        ---------------------- ---------------------- ---------------------- ---------------------- ----------------------
bank_id-YEAR                             Yes                    Yes                    Yes                    Yes                    Yes
county-YEAR                              Yes                    Yes                    Yes                    Yes                    Yes
_____________________ ______________________ ______________________ ______________________ ______________________ ______________________
S.E.: Clustered                  by: bank_id            by: bank_id            by: bank_id            by: bank_id            by: bank_id
Observations                         515,570                511,488                238,189                201,213                 72,086
R2                                   0.59757                0.60067                0.66046                0.54772                0.49940
Within R2                            0.14428                0.14173                0.17870                0.11198                0.12191
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## 2.2 Closure Intensity × Has App

**Regression.** Same specification as above, but `log1p(deps_closed_t1)` is interacted with `has_app` (1 if the bank has a mobile app, 0 otherwise). The interaction allows the association between closure intensity and deposit retention at remaining branches to vary by whether the bank has an app.

**Sample.** Models 1–2: base and main samples, restricted to 2010–2024. Models 3–5: main sample by subperiod—pre 2012 (2010–2011), 2012–2019, 2020–2024.

```
                                               model 1                model 2                model 3                model 4                model 5
                                                  Base                   Main               Pre 2012              2012-2019              2020-2024
Dependent Var.:                 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3
                                                                                                                                                  
log1p(deps_closed_t1)               0.0160*** (0.0011)     0.0129*** (0.0010)     0.0086*** (0.0017)     0.0124*** (0.0013)     0.0163*** (0.0019)
has_app                               0.9883 (1,340.2)        1.406 (2,278.9)       0.7966 (1,608.4)                            -0.1632 (11,314.5)
log1p(deps_remain_t1)              -0.1219*** (0.0045)    -0.1210*** (0.0046)    -0.1064*** (0.0070)    -0.1205*** (0.0058)    -0.1312*** (0.0082)
log1p(deps_closed_t1) x has_app        0.0012 (0.0010)        0.0014 (0.0009)      0.0072** (0.0023)        0.0009 (0.0012)        0.0007 (0.0017)
Fixed-Effects:                  ---------------------- ---------------------- ---------------------- ---------------------- ----------------------
bank_id-YEAR                                       Yes                    Yes                    Yes                    Yes                    Yes
county-YEAR                                        Yes                    Yes                    Yes                    Yes                    Yes
_______________________________ ______________________ ______________________ ______________________ ______________________ ______________________
S.E.: Clustered                            by: bank_id            by: bank_id            by: bank_id            by: bank_id            by: bank_id
Observations                                   324,239                321,582                 48,283                201,213                 72,086
R2                                             0.54218                0.54505                0.58375                0.54772                0.49940
Within R2                                      0.11553                0.11208                0.09690                0.11198                0.12191
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## 2.3 Closure Intensity × Large Bank

**Regression.** Same specification as above, but `log1p(deps_closed_t1)` is interacted with `large_bank` (1 if bank assets > $100B, 0 otherwise). The interaction allows the association between closure intensity and deposit retention at remaining branches to vary by bank size.

**Sample.** Same as main regressions: Model 1 (Base), Model 2 (Main), Models 3–5 by subperiod—pre 2012, 2012–2019, 2020–2024.

```
                                                  model 1                model 2                model 3                model 4                model 5
                                                     Base                   Main               Pre 2012              2012-2019              2020-2024
Dependent Var.:                    dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3
                                                                                                                                                     
log1p(deps_closed_t1)                  0.0139*** (0.0007)     0.0114*** (0.0007)     0.0086*** (0.0013)     0.0118*** (0.0009)     0.0152*** (0.0011)
large_bank                                  1.026 (724.0)          2.849 (872.4)         0.1321 (831.3)       0.9124 (7,747.7)     -0.1578 (10,491.7)
log1p(deps_remain_t1)                 -0.1418*** (0.0043)    -0.1411*** (0.0044)    -0.1633*** (0.0059)    -0.1206*** (0.0058)    -0.1314*** (0.0082)
log1p(deps_closed_t1) x large_bank     0.0063*** (0.0010)     0.0053*** (0.0009)     0.0090*** (0.0017)       0.0026* (0.0011)        0.0026 (0.0023)
Fixed-Effects:                     ---------------------- ---------------------- ---------------------- ---------------------- ----------------------
bank_id-YEAR                                          Yes                    Yes                    Yes                    Yes                    Yes
county-YEAR                                           Yes                    Yes                    Yes                    Yes                    Yes
__________________________________ ______________________ ______________________ ______________________ ______________________ ______________________
S.E.: Clustered                               by: bank_id            by: bank_id            by: bank_id            by: bank_id            by: bank_id
Observations                                      515,570                511,488                238,189                201,213                 72,086
R2                                                0.59771                0.60076                0.66060                0.54775                0.49944
Within R2                                         0.14458                0.14192                0.17904                0.11203                0.12198
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## 2.4 Closure Intensity × Top-4 Bank (Large Banks Only)

**Regression.** Same specification as above, but `log1p(deps_closed_t1)` is interacted with `top4_bank` (1 if one of the four largest banks, 0 otherwise). The interaction allows the association between closure intensity and deposit retention to vary between top-4 and other large banks.

**Sample.** Large banks only (assets > $100B). Model 1 (Base), Model 2 (Main), Models 3–5 by subperiod—pre 2012, 2012–2019, 2020–2024.


```
                                                 model 1                model 2                model 3                model 4                model 5
                                                    Base                   Main               Pre 2012              2012-2019              2020-2024
Dependent Var.:                   dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3
                                                                                                                                                    
log1p(deps_closed_t1)                 0.0125*** (0.0013)     0.0108*** (0.0011)       0.0071* (0.0028)     0.0090*** (0.0013)     0.0179*** (0.0027)
log1p(deps_remain_t1)                -0.1202*** (0.0128)    -0.1205*** (0.0135)    -0.1425*** (0.0144)    -0.0868*** (0.0134)    -0.1572*** (0.0340)
log1p(deps_closed_t1) x top4_bank       -0.0006 (0.0021)       -0.0013 (0.0017)        0.0020 (0.0044)        0.0018 (0.0018)     -0.0080** (0.0025)
Fixed-Effects:                    ---------------------- ---------------------- ---------------------- ---------------------- ----------------------
bank_id-YEAR                                         Yes                    Yes                    Yes                    Yes                    Yes
county-YEAR                                          Yes                    Yes                    Yes                    Yes                    Yes
_________________________________ ______________________ ______________________ ______________________ ______________________ ______________________
S.E.: Clustered                              by: bank_id            by: bank_id            by: bank_id            by: bank_id            by: bank_id
Observations                                      98,440                 96,406                 40,718                 41,457                 14,231
R2                                               0.56477                0.57035                0.62385                0.50179                0.54758
Within R2                                        0.10710                0.10625                0.15171                0.06029                0.15181
```


## 2.5 Closure Intensity × Large Good App vs. Large Bad App

**Regression.** Same specification as above, but `log1p(deps_closed_t1)` is interacted with `large_good_app` and `large_bad_app`. Large good app: large bank (assets > $100B) with app rating above the year-specific median among large banks with reviews. Large bad app: large bank that is not large good app (no app or rating at/below median). Reference category: banks that are not large.

**Sample.** Restricted to 2010–2024. Model 1 (Base), Model 2 (Main), Models 3–4 by subperiod—2012–2019, 2020–2024.
```

                                                      model 1                model 2                model 3                model 4
                                                         Base                   Main              2012-2019              2020-2024
Dependent Var.:                        dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3 dlog_deps_remaining_t3
                                                                                                                                  
log1p(deps_closed_t1)                      0.0148*** (0.0007)     0.0122*** (0.0007)     0.0118*** (0.0009)     0.0152*** (0.0011)
large_good_app                                1.234 (1,749.8)        1.556 (2,495.8)       1.483 (14,313.5)     -0.1245 (17,050.3)
log1p(deps_remain_t1)                     -0.1221*** (0.0045)    -0.1210*** (0.0046)    -0.1206*** (0.0058)    -0.1314*** (0.0082)
log1p(deps_closed_t1) x large_good_app     0.0039*** (0.0011)      0.0035** (0.0011)       0.0028* (0.0012)        0.0026 (0.0020)
log1p(deps_closed_t1) x large_bad_app       0.0038** (0.0014)       0.0032* (0.0013)       0.0025. (0.0013)        0.0026 (0.0028)
large_bad_app                                                                                                   -0.0891 (19,130.9)
Fixed-Effects:                         ---------------------- ---------------------- ---------------------- ----------------------
bank_id-YEAR                                              Yes                    Yes                    Yes                    Yes
county-YEAR                                               Yes                    Yes                    Yes                    Yes
______________________________________ ______________________ ______________________ ______________________ ______________________
S.E.: Clustered                                   by: bank_id            by: bank_id            by: bank_id            by: bank_id
Observations                                          324,239                321,582                201,213                 72,086
R2                                                    0.54225                0.54509                0.54775                0.49944
Within R2                                             0.11565                0.11216                0.11203                0.12198
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```