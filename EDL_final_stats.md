EDL final stats
================

``` r
knitr::opts_chunk$set(message=TRUE,error = TRUE,echo=TRUE,results = "markdown",warning=TRUE)
```

``` r
load(".RData")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(afex)
```

    ## Loading required package: lme4
    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## ************
    ## Welcome to afex. For support visit: http://afex.singmann.science/
    ## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ## - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    ## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    ## - NEWS: emmeans() for ANOVA models now uses model = 'multivariate' as default.
    ## - Get and set global package options with: afex_options()
    ## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
    ## - For example analyses see: browseVignettes("afex")
    ## ************
    ## 
    ## Attaching package: 'afex'
    ## 
    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

``` r
library(gt)
library(parallel)
library(gtsummary)
```

``` r
# collapse multiple retrieval presTimes down to one variable for main effect stats
time_to_cond = function(df){
  df$study_test = ifelse(df$presTime == 0, "NoGap","Gap")
  df$NoGap_Gap = as.factor(df$study_test)
  df
}

summarize_models = function(m1,m2){
  models = list(m1 = summary(m1,correlations=FALSE),
                m2 = summary(m2))
return(models)
}

change_pres10 =function(df){
  df$presTime = sub(10,0,df$presTime)
return(df)
}
```

``` r
run_learning_models_and_tables = function(ldf,sdir,exp_flag=NA){
    
  ldf = change_pres10(ldf)
  ldf = time_to_cond(ldf)
  ldf$SOA = as.factor(ldf$presTime)
  ldf$id = as.factor(ldf$sona_id)
  ldf$correct = as.numeric(ldf$correct)
  ldf$first_press = as.numeric(ldf$first_press)
  if(exp_flag == 3){
    ldf$gap_group = as.factor(ldf$gap_group)
  }
  
  # learning target accuracy
  if(exp_flag == 3){
    lm1 = aov_ez("id","correct",ldf,between="gap_group",within=c("gap_nogap","rep_idx"))
  }else{
    lm1 = aov_ez("id","correct",ldf,within=c("SOA","rep_idx"))  
  } 
  
  # first_press average 
  if(exp_flag == 3){
    lm2 = aov_ez("id","first_press",ldf,between="gap_group",within=c("gap_nogap","rep_idx"))
  }else{
    lm2 = aov_ez("id","first_press",ldf,within=c("SOA","rep_idx"))  
  } 
  
  # table accuracy by SOA
  t_learn_accuracy_x_SOA = ldf %>% group_by(id,SOA) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(SOA) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
     columns = c(mean,sd),
    decimals = 2
  )
  t_learn_accuracy_x_SOA %>% gtsave(filename = "t_learn_accuracy_x_SOA.png",path=sdir)
  
  # accuracy grouped by rep_idx...
  t_learn_accuracy_x_rep = ldf %>% group_by(sona_id,rep_idx) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(rep_idx) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
     columns = c(mean,sd),
    decimals = 2
  ) %>%
    fmt_number(columns = rep_idx, decimals = 0)
  t_learn_accuracy_x_rep %>% gtsave(filename = "t_learn_accuracy_x_rep.png",path=sdir)

  # table first_press by presTime
  t_learn_RTxSOA = ldf %>% drop_na() %>%  group_by(id,SOA) %>% summarize(sub_mean_correct = mean(first_press,na.RM=TRUE),.groups="drop") %>% group_by(SOA) %>% summarize(mean = mean(sub_mean_correct,na.RM=TRUE),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
     columns = c(mean,sd),
    decimals = 0
  )
  t_learn_RTxSOA %>% gtsave(filename = "t_learn_RTxSOA.png",path=sdir)
  
  # table first_press by rep_idx
  t_learn_RTxrep = ldf %>% drop_na() %>%  group_by(id,rep_idx) %>% summarize(sub_mean_correct = mean(first_press),.groups="drop") %>% group_by(rep_idx) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
     columns = c(mean,sd),
    decimals = 0
  ) %>% 
    fmt_number(columns = rep_idx, decimals = 0)
  t_learn_RTxrep %>% gtsave(filename = "t_learn_RTxrep.png",path=sdir)
  
  out = list(lm1,lm2)
  return(out)
} # end function
```

``` r
summarize_learning_models = function(lm1,lm2){
    models = list(lm1 = lm1,
                  lm2 = lm2)
return(models)
}
```

``` r
# simplified models. Main effect, categorical of test and ttest
run_simple_models = function(df,nsim){
  
df = change_pres10(df)
df = time_to_cond(df)

df$presTime_numeric = as.numeric(df$presTime)
df$SOA = as.factor(df$presTime)
df$id = as.factor(df$sona_id)
df$correct = as.numeric(df$correct)
df$pair_idx = as.factor(df$pair_idx)
df$fp_diff = as.numeric(df$fp_diff)
df$fp_diff_scale = scale(df$fp_diff)

# just test df
test_df = df %>% filter(presTime != "0")

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm1 = afex::mixed(cl=cl,data=df,formula=(correct ~ NoGap_Gap + (NoGap_Gap|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)))
stopCluster(cl)

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm2 = afex::mixed(cl=cl,data=df,formula=(correct~ SOA + (1|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)),check_contrasts = FALSE,per_parameter="SOA")
stopCluster(cl)

out=list(mm1,mm2) 
return(out)
}
```

``` r
run_tables = function(df,sdir){
### descriptive stats tables
  
df = change_pres10(df)
df = time_to_cond(df)

df$presTime_numeric = as.numeric(df$presTime)
df$SOA = as.factor(df$presTime)
df$id = as.factor(df$sona_id)
df$correct = as.numeric(df$correct)
df$pair_idx = as.factor(df$pair_idx)
df$fp_diff = as.numeric(df$fp_diff)
df$fp_diff_scale = scale(df$fp_diff)

# study_test 
t_study_test = df %>% group_by(id,NoGap_Gap) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(NoGap_Gap) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
   columns = c(mean,sd),
  decimals = 2
)
t_study_test %>% gtsave(filename = "NoGap_Gap_table.png",path=sdir)

# SOA
t_SOA = df %>% group_by(id,SOA) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(SOA) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
   columns = c(mean,sd),
  decimals = 2
)
t_SOA %>% gtsave(filename = "SOA_table.png",path=sdir)
}
```

``` r
# simple summarize models
simple_summarize_models = function(m1,m2){
  models = list(m1 = m1,
                m2 = m2)
return(models)
}
```

## Experiment 1

``` r
ldf1 = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/exp1_learning_data.csv")
```

    ## Rows: 950 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): sona_id, presTime, rep_idx, first_press, correct
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir1 = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/tables/"
# 
lms1 = run_learning_models_and_tables(ldf1,sdir1,exp_flag = 1)
```

    ## Warning: Missing values for following ID(s):
    ## 12807, 12815, 12816, 12817, 12818, 12819, 13247, 13417
    ## Removing those cases from the analysis.

``` r
summarize_learning_models(lms1[[1]],lms1[[2]])
```

    ## $lm1
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: correct
    ##        Effect           df  MSE         F   ges p.value
    ## 1         SOA  1.75, 64.63 0.17 37.89 ***  .073   <.001
    ## 2     rep_idx 2.72, 100.55 0.07      0.53 <.001    .648
    ## 3 SOA:rep_idx 8.23, 304.52 0.03  7.14 ***  .014   <.001
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG 
    ## 
    ## $lm2
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: first_press
    ##        Effect           df        MSE          F  ges p.value
    ## 1         SOA  2.09, 60.50 2130675.96 206.26 *** .586   <.001
    ## 2     rep_idx  1.86, 53.93 2289941.88  87.91 *** .366   <.001
    ## 3 SOA:rep_idx 5.38, 155.97 1268133.57  22.64 *** .192   <.001
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/exp1_n=38_final.csv")
```

    ## Rows: 760 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (16): rowNo, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f, ITI_fDura...
    ## lgl   (4): key, trialText, button, responseCode
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/tables/"
run_tables(df,sdir)
```

``` r
# start_time = Sys.time()
# # exp1 (sona)
# df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/exp1_n=38_final.csv")
# sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp1/tables/"
# 
# exp1_models  = run_simple_models(df,nsim=500)

simple_summarize_models(exp1_models[[1]],exp1_models[[2]])
```

    ## $m1

    ## Warning: lme4 reported (at least) the following warnings for 'full':
    ##   * boundary (singular) fit: see ?isSingular

    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ NoGap_Gap + (study_test | id) + (1 | pair_idx)
    ## Data: df
    ##      Effect df  Chisq p.value
    ## 1 NoGap_Gap  1 4.16 *    .045
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## $m2
    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ SOA + (1 | id) + (1 | pair_idx)
    ## Data: df
    ##    Effect df  Chisq p.value
    ## 1 SOA1500  1   1.92    .166
    ## 2 SOA3000  1 4.48 *    .041
    ## 3 SOA4500  1 3.08 +    .096
    ## 4 SOA6000  1 4.62 *    .033
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

## Experiment 2

``` r
ldf2 = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/data/exp2_learning_data.csv")
```

    ## Rows: 464 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): sona_id, presTime, rep_idx, first_press, correct
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir2 = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/data/tables/"

lms2 = run_learning_models_and_tables(ldf2,sdir2,exp_flag = 2)
```

    ## Warning: Missing values for following ID(s):
    ## 13507
    ## Removing those cases from the analysis.

``` r
summarize_learning_models(lms2[1],lms2[2])
```

    ## $lm1
    ## $lm1[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: correct
    ##        Effect           df  MSE    F  ges p.value
    ## 1         SOA  2.56, 71.56 0.02 2.08 .003    .120
    ## 2     rep_idx  1.35, 37.88 0.10 1.59 .006    .219
    ## 3 SOA:rep_idx 5.67, 158.64 0.01 0.81 .002    .557
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG 
    ## 
    ## 
    ## $lm2
    ## $lm2[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: first_press
    ##        Effect           df      MSE          F  ges p.value
    ## 1         SOA  1.61, 43.55 79982.00 116.40 *** .399   <.001
    ## 2     rep_idx  2.01, 54.17 43986.01  42.25 *** .141   <.001
    ## 3 SOA:rep_idx 4.12, 111.24 24580.86       0.68 .003    .614
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/data/exp2_n=29_final.csv")
```

    ## Rows: 812 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (16): rowNo, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f, ITI_fDura...
    ## lgl   (4): key, trialText, button, responseCode
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/data/tables/"
run_tables(df,sdir)
```

``` r
# df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/data/exp2_n=29_final.csv")
# exp2_models  = run_simple_models(df,nsim=500)
simple_summarize_models(exp2_models[[1]],exp2_models[[2]])
```

    ## $m1

    ## Warning: lme4 reported (at least) the following warnings for 'full':
    ##   * boundary (singular) fit: see ?isSingular

    ## Warning: lme4 reported (at least) the following warnings for 'NoGap_Gap':
    ##   * boundary (singular) fit: see ?isSingular

    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ NoGap_Gap + (NoGap_Gap | id) + (1 | pair_idx)
    ## Data: df
    ##      Effect df   Chisq p.value
    ## 1 NoGap_Gap  1 7.52 **    .004
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## $m2
    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ SOA + (1 | id) + (1 | pair_idx)
    ## Data: df
    ##   Effect df   Chisq p.value
    ## 1 SOA200  1    2.06    .151
    ## 2 SOA600  1 9.39 **    .008
    ## 3 SOA800  1  5.51 *    .032
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

## Experiment 3

``` r
ldf3 = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/exp3_learning_data.csv")
```

    ## Rows: 1016 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): sona_id
    ## dbl (4): presTime, rep_idx, first_press, correct
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir3 = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/"

# ldf = ldf %>% filter(presTime != 10)

# add subjGroup col
ldf3 = ldf3 %>% group_by(sona_id) %>% mutate(gap_group = max(presTime))
# convert SOA to gap or no gap
ldf3 = ldf3 %>% group_by(sona_id) %>% mutate(gap_nogap = ifelse(presTime > 10,"gap","nogap"))

lms3 = run_learning_models_and_tables(ldf3,sdir3,exp_flag = 3)
```

    ## Contrasts set to contr.sum for the following variables: gap_group
    ## Contrasts set to contr.sum for the following variables: gap_group

``` r
summarize_learning_models(lms3[1],lms3[2])
```

    ## $lm1
    ## $lm1[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: correct
    ##                        Effect           df  MSE        F   ges p.value
    ## 1                   gap_group       2, 124 0.40     2.07  .028    .130
    ## 2                   gap_nogap       1, 124 0.02     0.20 <.001    .660
    ## 3         gap_group:gap_nogap       2, 124 0.02     0.87 <.001    .421
    ## 4                     rep_idx 2.27, 281.63 0.01 7.83 ***  .002   <.001
    ## 5           gap_group:rep_idx 4.54, 281.63 0.01     0.83 <.001    .523
    ## 6           gap_nogap:rep_idx 2.53, 314.16 0.01     0.63 <.001    .571
    ## 7 gap_group:gap_nogap:rep_idx 5.07, 314.16 0.01     1.78  .001    .116
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG 
    ## 
    ## 
    ## $lm2
    ## $lm2[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: first_press
    ##                        Effect           df       MSE          F   ges p.value
    ## 1                   gap_group       2, 124 436214.49   8.87 ***  .094   <.001
    ## 2                   gap_nogap       1, 124  76881.46 111.47 ***  .103   <.001
    ## 3         gap_group:gap_nogap       2, 124  76881.46   7.71 ***  .016   <.001
    ## 4                     rep_idx 2.21, 274.14  23981.10 115.93 ***  .076   <.001
    ## 5           gap_group:rep_idx 4.42, 274.14  23981.10       0.75  .001    .573
    ## 6           gap_nogap:rep_idx 2.49, 308.84  14436.10       0.09 <.001    .947
    ## 7 gap_group:gap_nogap:rep_idx 4.98, 308.84  14436.10       0.61 <.001    .694
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
# simplified models. Main effect, categorical of test and ttest
run_exp3_simple_models = function(df,nsim){
  
df = change_pres10(df)
df = time_to_cond(df)

df$cb_condition = as.factor(df$cb_condition)
df$presTime_numeric = as.numeric(df$presTime)
df$SOA = as.factor(df$presTime)
df$id = as.factor(df$sona_id)
df$correct = as.numeric(df$correct)
df$pair_idx = as.factor(df$pair_idx)
df$fp_diff = as.numeric(df$fp_diff)
df$fp_diff_scale = scale(df$fp_diff)

# just test df
test_df = df %>% filter(presTime != "0")

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm1 = afex::mixed(cl=cl,data=df,formula=(correct ~ NoGap_Gap*cb_condition + (NoGap_Gap|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)))
stopCluster(cl)

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm2 = afex::mixed(cl=cl,data=df,formula=(correct ~ SOA*cb_condition + (1|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)),check_contrasts = FALSE,per_parameter="SOA")
stopCluster(cl)

out=list(mm1,mm2) 
return(out)
}
```

``` r
### descriptive stats tables
exp3_tables = function(df,sdir){
  
df = change_pres10(df)
df = time_to_cond(df)

df$cb_condition = as.factor(df$cb_condition)
df$presTime_numeric = as.numeric(df$presTime)
df$SOA = as.factor(df$presTime)
df$id = as.factor(df$sona_id)
df$correct = as.numeric(df$correct)
df$pair_idx = as.factor(df$pair_idx)
df$fp_diff = as.numeric(df$fp_diff)
df$fp_diff_scale = scale(df$fp_diff)

# NoGap_Gap 
t_study_test = df %>% group_by(id,NoGap_Gap) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(NoGap_Gap) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
   columns = c(mean,sd),
  decimals = 2
)
t_study_test %>% gtsave(filename = "study_test_table.png",path=sdir)

# SOA
t_SOA = df %>% group_by(id,SOA) %>% summarize(sub_mean_correct = mean(correct),.groups="drop") %>% group_by(SOA) %>% summarize(mean = mean(sub_mean_correct),sd = sd(sub_mean_correct)) %>% gt() %>% fmt_number(
   columns = c(mean,sd),
  decimals = 2
)
t_SOA %>% gtsave(filename = "SOA_table.png",path=sdir)
}
```

``` r
df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/exp3_n=127_final.csv")
```

    ## Rows: 3556 Columns: 37
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): type, stim1, stim2, stimFormat, responseType, responsePos, fixatio...
    ## dbl (16): rowNo, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f, ITI_fDurat...
    ## lgl  (6): responseOptions, key, trialText, button, responseCode, comp_code
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/"
exp3_tables(df,sdir)

# n per group
df %>% group_by(cb_condition,gap_presTime) %>% summarize(count = n_distinct(sona_id))
```

    ## `summarise()` has grouped output by 'cb_condition'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 6 × 3
    ## # Groups:   cb_condition [2]
    ##   cb_condition gap_presTime count
    ##   <chr>               <dbl> <int>
    ## 1 gap_nogap             200    17
    ## 2 gap_nogap             400    18
    ## 3 gap_nogap             600    29
    ## 4 nogap_gap             200    16
    ## 5 nogap_gap             400    18
    ## 6 nogap_gap             600    29

``` r
# 
# df = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/exp3_n=127_final.csv")
# 
# exp3_models  = run_exp3_simple_models(df,nsim=500)
simple_summarize_models(exp3_models[[1]],exp3_models[[2]])
```

    ## $m1
    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ NoGap_Gap * cb_condition + (NoGap_Gap | id) + (1 | 
    ## Model:     pair_idx)
    ## Data: df
    ##                   Effect df    Chisq p.value
    ## 1              NoGap_Gap  1   6.84 *    .016
    ## 2           cb_condition  1     0.06    .824
    ## 3 NoGap_Gap:cb_condition  1 11.79 **    .002
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## $m2
    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ SOA * cb_condition + (1 | id) + (1 | pair_idx)
    ## Data: df
    ##                         Effect df    Chisq p.value
    ## 1                       SOA200  1   6.79 *    .016
    ## 2                       SOA400  1 15.61 **    .003
    ## 3                       SOA600  1 13.49 **    .003
    ## 4                 cb_condition  1     2.08    .204
    ## 5 SOA200:cb_conditionnogap_gap  1   6.13 *    .027
    ## 6 SOA400:cb_conditionnogap_gap  1 11.53 **    .003
    ## 7 SOA600:cb_conditionnogap_gap  1   2.95 +    .093
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

## Experiment 4a

``` r
ldf4 = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/exp4a_learning_data.csv")
```

    ## Rows: 704 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): sona_id
    ## dbl (4): presTime, rep_idx, first_press, correct
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir4 = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/"

lms4 = run_learning_models_and_tables(ldf4,sdir4,exp_flag =4)
```

    ## Warning: Missing values for following ID(s):
    ## AI6TD8PM938FQ
    ## Removing those cases from the analysis.

``` r
summarize_learning_models(lms4[1],lms4[2])
```

    ## $lm1
    ## $lm1[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: correct
    ##        Effect           df  MSE    F  ges p.value
    ## 1         SOA 2.85, 122.60 0.01 1.36 .001    .258
    ## 2     rep_idx  1.68, 72.32 0.04 1.88 .005    .166
    ## 3 SOA:rep_idx 6.37, 274.08 0.01 1.39 .004    .216
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG 
    ## 
    ## 
    ## $lm2
    ## $lm2[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: first_press
    ##        Effect           df      MSE          F  ges p.value
    ## 1         SOA  2.31, 96.91 20436.63 193.16 *** .259   <.001
    ## 2     rep_idx  1.97, 82.68 45323.89  31.64 *** .098   <.001
    ## 3 SOA:rep_idx 5.44, 228.49 15519.37     2.01 + .006    .072
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
df4a = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/exp4a_n=44_final.csv")
```

    ## Rows: 1232 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (15): rowNo, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f, ITI_fDura...
    ## lgl   (6): responseOptions, key, trialText, button, responseCode, comp_code
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/"
run_tables(df4a,sdir)
```

## Experiment 4b

``` r
ldf4b = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/exp4b_learning_data.csv")
```

    ## Rows: 784 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): sona_id
    ## dbl (4): presTime, rep_idx, first_press, correct
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir4b = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/"

lms4b = run_learning_models_and_tables(ldf4b,sdir4b,exp_flag =4)
summarize_learning_models(lms4b[1],lms4b[2])
```

    ## $lm1
    ## $lm1[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: correct
    ##        Effect           df  MSE    F   ges p.value
    ## 1         SOA 2.55, 122.22 0.01 0.23 <.001    .842
    ## 2     rep_idx 2.39, 114.53 0.03 0.51  .001    .633
    ## 3 SOA:rep_idx 6.86, 329.11 0.01 0.47  .001    .854
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG 
    ## 
    ## 
    ## $lm2
    ## $lm2[[1]]
    ## Anova Table (Type 3 tests)
    ## 
    ## Response: first_press
    ##        Effect           df      MSE          F  ges p.value
    ## 1         SOA  1.77, 84.78 40614.82 156.56 *** .198   <.001
    ## 2     rep_idx  1.65, 79.37 96892.27  13.82 *** .046   <.001
    ## 3 SOA:rep_idx 5.24, 251.68 18699.24       0.73 .002    .605
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
df4b = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/exp4b_n=50_final.csv")
```

    ## Rows: 1372 Columns: 44
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (15): rowNo, presTime, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f,...
    ## lgl  (16): stim3, stim4, ISI, responseWindow, responseOptions, button1, key,...
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sdir = "/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/"
run_tables(df4b,sdir)
```

## Experiment 4 Merged

``` r
# for experiment 4 simplified models with RI factor
exp4_run_simple_models = function(df,nsim){

df = change_pres10(df)
df = time_to_cond(df)

df$Retention_Interval = as.factor(df$RI)
df$presTime_numeric = as.numeric(df$presTime)
df$SOA = as.factor(df$presTime)
df$id = as.factor(df$sona_id)
df$correct = as.numeric(df$correct)
df$pair_idx = as.factor(df$pair_idx)
df$fp_diff = as.numeric(df$fp_diff)
df$fp_diff_scale = scale(df$fp_diff)

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm1 = afex::mixed(cl=cl,data=df,formula=(correct ~ NoGap_Gap*Retention_Interval + (NoGap_Gap|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)))
stopCluster(cl)

cl <- makeCluster(rep("localhost", 6)) # make cluster
mm2 = afex::mixed(cl=cl,data=df,formula=(correct ~ SOA*Retention_Interval + (1|id) + (1|pair_idx)),family=binomial(link='logit'),method="PB",args_test = list(nsim = nsim, cl = cl),progress=TRUE,expand_re = TRUE,control = glmerControl(optCtrl = list(maxfun = 1e6)),check_contrasts = FALSE,per_parameter="SOA")
stopCluster(cl)

out=list(mm1,mm2) 
return(out)
}
```

``` r
# experiment 4 immediate and delayed test run simple models
# need to merge 4a and 4b and add delay factor
df4a = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/exp4a_n=44_final.csv")
```

    ## Rows: 1232 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (15): rowNo, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f, ITI_fDura...
    ## lgl   (6): responseOptions, key, trialText, button, responseCode, comp_code
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df4b = read_csv("/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/exp4b_n=50_final.csv")
```

    ## Rows: 1372 Columns: 44
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): type, stim1, stim2, stimFormat, responseType, responsePos, fixati...
    ## dbl  (15): rowNo, presTime, ITI, random, trial_idx, pair_idx, ITI_ms, ITI_f,...
    ## lgl  (16): stim3, stim4, ISI, responseWindow, responseOptions, button1, key,...
    ## dttm  (1): timestamp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df4a = df4a %>% select(presTime,sona_id,correct,pair_idx,fp_diff)
df4a$RI = '1 day'
df4b = df4b %>% select(presTime,sona_id,correct,pair_idx,fp_diff)
df4b$RI = '1 min'

df4merged = rbind(df4a,df4b) 
```

``` r
# exp4_merged_models  = exp4_run_simple_models(df4merged,nsim=500)

simple_summarize_models(exp4_merged_models[[1]],exp4_merged_models[[2]])
```

    ## $m1

    ## Warning: lme4 reported (at least) the following warnings for 'Retention_Interval':
    ##   * boundary (singular) fit: see help('isSingular')

    ## Warning: lme4 reported (at least) the following warnings for 'NoGap_Gap:Retention_Interval':
    ##   * boundary (singular) fit: see help('isSingular')

    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ NoGap_Gap * Retention_Interval + (NoGap_Gap | id) + 
    ## Model:     (1 | pair_idx)
    ## Data: df
    ##                         Effect df   Chisq p.value
    ## 1                    NoGap_Gap  1 7.92 **    .008
    ## 2           Retention_Interval  1  7.57 *    .011
    ## 3 NoGap_Gap:Retention_Interval  1    0.38    .497
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## $m2
    ## Mixed Model Anova Table (Type 3 tests, PB-method)
    ## 
    ## Model: correct ~ SOA * Retention_Interval + (1 | id) + (1 | pair_idx)
    ## Data: df
    ##                           Effect df  Chisq p.value
    ## 1                         SOA200  1   0.28    .664
    ## 2                         SOA400  1   2.59    .146
    ## 3                         SOA600  1 4.38 *    .032
    ## 4             Retention_Interval  1 5.58 *    .025
    ## 5 SOA200:Retention_Interval1 min  1   0.96    .353
    ## 6 SOA400:Retention_Interval1 min  1   0.13    .835
    ## 7 SOA600:Retention_Interval1 min  1   0.05    .882
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

``` r
df4merged %>% group_by(RI) %>% summarize(count = n_distinct(sona_id))
```

    ## # A tibble: 2 × 2
    ##   RI    count
    ##   <chr> <int>
    ## 1 1 day    44
    ## 2 1 min    49
