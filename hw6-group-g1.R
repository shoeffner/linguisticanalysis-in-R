# Homework 6 by Andrea Suckro and Sebastian Höffner

library('languageR')
library('lme4')

# We assume alpha = 0.05 for all tests.

ttest <- function (model, effect, df, alpha=0.05) {
    # Performs a one-sided t-test for a given alpha value on the given model.
    t = abs(coef(summary(model))[effect, 't value'])
    q = qt(1 - alpha, df=df)
    paste('|t| =', round(t, 3), '>', round(q, 3), '?', t > q)
}


# 1. Are reaction times affected by the number of trials?
# 2 DOF, one-sided test: Pr(|t| < 2.92) = .95
# No: |t| = 2.435

model1 = lm(RT ~ Trial, lexdec)
print(paste("Is reaction time affected by the number of trials only?", ttest(model1, 'Trial', 2)))


# 2. Are reaction times affected by the number of trials,
# taking Subject and Word differences (their varying means) into account?
# 5 DOF, one-sided test: Pr(|t| < 2.015) = .95
# Yes: |t| = 2.647 > 2.015

model2 = lmer(RT ~ Trial + (1|Subject) + (1|Word), lexdec)
print(paste("Is reaction time affected by the number of trials?", ttest(model2, 'Trial', 5)))


# 3. Are reaction times also affected by the native language?
# 6 DOF, one-sided test: Pr(|t| < 1.943) = .95
# Yes (native language): |t| = 2.573 > 1.943
# Yes (trials): |t| = 2.647 > 1.943

model3 = lmer(RT ~ Trial + (1|Subject) + (1|Word) + NativeLanguage, lexdec)
print(paste("Is reaction time affected by native language?", ttest(model3, 'NativeLanguageOther', 6)))
print(paste("Is reaction time still affected by trials if considering native language?", ttest(model3, 'Trial', 6)))


# 4. Subjects are affected differently depending on the number of trials.
# 7 DOF, one-sided test: PR(-1.895 < t < 1.895) = .95
# !! The model does not converge. As such the t-values can not be used for predictions,
#    but if we could, this would be the results:
# No (trials): |t| = 1.369 > 1.415
# Yes (native language): |t| = 2.913 > 1.415

model4 = lmer(RT ~ Trial + (1 + Trial|Subject) + (1|Word) + NativeLanguage, lexdec)
print(paste("Is reaction time still affected by trials if considering native language and by-subject variance?", ttest(model4, 'Trial', 7)))
print(paste("Is reaction time still affected by native language if considering by-subject variance?", ttest(model4, 'NativeLanguageOther', 7)))
