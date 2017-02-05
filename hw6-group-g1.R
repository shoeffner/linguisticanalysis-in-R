# Homework 6 by Andrea Suckro and Sebastian Höffner

library('languageR')
library('lme4')

data = lexdec

# 1. Are reaction times affected by the number of trials?
summary(lm(RT ~ Trial, data))

# 2. Are reaction times affected by the number of trials,
# taking Subject and Word differences (their varying means) into account?
summary(lmer(RT ~ Trial + (1|Subject) + (1|Word), data=data))

# 3. Are reaction times also affected by the native language?
summary(lmer(RT ~ Trial + (1|Subject) + (1|Word) + NativeLanguage, data=data))

# 4. Subjects are affected differently depening on the number of trials.
summary(lmer(RT ~ Trial + (1 + Trial|Subject) + (1|Word) + NativeLanguage, data=data))
