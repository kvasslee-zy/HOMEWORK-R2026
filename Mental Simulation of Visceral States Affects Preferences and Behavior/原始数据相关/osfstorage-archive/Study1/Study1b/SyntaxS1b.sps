* Encoding: UTF-8.
delete first participant (RA test)

FREQUENCIES VARIABLES=Q3
  /ORDER=ANALYSIS.

DESCRIPTIVES VARIABLES=Q4
  /STATISTICS=MEAN STDDEV MIN MAX.

RECODE DO_BR_FL_6yTYxOmURd7ewO9 ('simulation'=1) ('priming'=2) INTO condition.
EXECUTE.

RECODE NP_H_1 (1=2) INTO hot_cold.
EXECUTE.

RECODE NP_C_2 (1=1) INTO hot_cold.
EXECUTE.

RECODE Q40 (1=2) INTO hot_cold.
EXECUTE.

RECODE Q41 (1=1) INTO hot_cold.
EXECUTE.

RECODE Q244 (1=2) INTO hot_cold.
EXECUTE.

RECODE Q245 (1=1) INTO hot_cold.
EXECUTE.

RECODE Q246 (1=2) INTO hot_cold.
EXECUTE.

RECODE Q247 (1=1) INTO hot_cold.
EXECUTE.

COMPUTE A4rev=10 - A4.
EXECUTE.

COMPUTE A6rev=10 - A6.
EXECUTE.

COMPUTE A10rev=10 - A10.
EXECUTE.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.

RELIABILITY
  /VARIABLES= A4rev A6rev A10rev A3 A8
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

SPLIT FILE OFF.

COMPUTE choices=MEAN(A4rev,A6rev,A10rev,A3,A8).
EXECUTE.

UNIANOVA choices BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q41.0 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.


UNIANOVA Q42 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q44 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q46.0 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q48.0 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q42 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q50 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.

UNIANOVA Q79 BY condition hot_cold
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(hot_cold)
/EMMEANS=TABLES(condition*hot_cold) COMPARE(condition)
  /DESIGN=condition hot_cold condition*hot_cold.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT choices
  /METHOD=ENTER Q46.0.
