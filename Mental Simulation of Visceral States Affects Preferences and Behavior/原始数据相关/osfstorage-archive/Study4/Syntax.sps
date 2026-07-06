* Encoding: UTF-8.

momentary preferences

RELIABILITY
  /VARIABLES=food1 food2 food3 food4 food5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

COMPUTE choice_now=MEAN(food1,food2,food3,food4,food5).
EXECUTE.

general preferences

RELIABILITY
  /VARIABLES=Q240 Q241 Q242 Q243 Q244
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

COMPUTE choice_general=MEAN(Q240,Q241,Q242,Q243,Q244).
EXECUTE.

COMPUTE choice=MEAN(choice_now,choice_general).
EXECUTE.


UNIANOVA choice BY condition nowgeneral
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*nowgeneral) COMPARE(nowgeneral)
/EMMEANS=TABLES(condition*nowgeneral) COMPARE(condition)
  /DESIGN=condition nowgeneral condition*nowgeneral.

UNIANOVA hungry.0 BY condition nowgeneral
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*nowgeneral) COMPARE(nowgeneral)
/EMMEANS=TABLES(condition*nowgeneral) COMPARE(condition)
  /DESIGN=condition nowgeneral condition*nowgeneral.



SORT CASES  BY nowgeneral.
SPLIT FILE SEPARATE BY nowgeneral.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT choice
  /METHOD=ENTER hungry.0.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT choice
  /METHOD=ENTER hungry.0 condition.

