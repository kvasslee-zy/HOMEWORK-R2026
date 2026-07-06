* Encoding: UTF-8.

Compute preference for warming activities

COMPUTE pref_warmth=MEAN(Q42,Q44,Q46,Q48,Q50).
EXECUTE.

RELIABILITY
  /VARIABLES=Q42 Q44 Q46 Q48 Q50
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

Compute preference for filling activities

COMPUTE pref_warmth=MEAN(food1,food2,food3,food4,food5).
EXECUTE.

RELIABILITY
  /VARIABLES=food1 food2 food3 food4 food5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

2x2x2 repeated measures

GLM pref_warmth pref_food BY state condition
  /WSFACTOR=preferences 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=preferences 
  /DESIGN=state condition state*condition.


UNIANOVA pref_warmth BY state condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(state*condition) COMPARE(condition)
/EMMEANS=TABLES(state*condition) COMPARE(state)
  /DESIGN=state condition state*condition.


UNIANOVA pref_food BY state condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(state*condition) COMPARE(condition)
/EMMEANS=TABLES(state*condition) COMPARE(state)
  /DESIGN=state condition state*condition.

if state = 1 participants simulated temperature states (hot or cold)

USE ALL.
COMPUTE filter_$=(state = 1).
VARIABLE LABELS filter_$ 'state = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Q51
  /CRITERIA=CI(.95).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pref_warmth
  /METHOD=ENTER Q51.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pref_warmth
  /METHOD=ENTER Q51 condition.


if state = 2 participants simulated hunger states (hungry or full)

USE ALL.
COMPUTE filter_$=(state = 2).
VARIABLE LABELS filter_$ 'state = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=hungry.0
  /CRITERIA=CI(.95).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pref_food
  /METHOD=ENTER hungry.0.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pref_food
  /METHOD=ENTER hungry.0 condition.

