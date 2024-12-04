# Run PRS analyses

library(lme4)
library(sjPlot)
library(ggplot2)
library(lmerTest)

YOUTH_all_LD <- readRDS("N:/durable/projects/cme_trio/data/analysis_ready_data_YOUTH_all_LD.RDS") 
YOUTH_covid_LD <- readRDS("N:/durable/projects/cme_trio/data/analysis_ready_data_YOUTH_covid_LD.RDS") 
MOTHER_LD <- readRDS("N:/durable/projects/cme_trio/data/analysis_ready_data_MOTHER.RDS_LD")
FATHER_LD <- readRDS("N:/durable/projects/cme_trio/data/analysis_ready_data_FATHER.RDS_LD")


# Youth anxiety model for H1

youth_all_anx_model_H1 <- lmer(youth_scl ~ anx_pgs_res.C + anx_pgs_res.M + anx_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_all_LD, REML = FALSE)
summary(youth_all_anx_model_H1) 

# Youth depression model H1

youth_all_mdd_model_H1 <- lmer(youth_scl ~ mdd_pgs_res.C + mdd_pgs_res.M + mdd_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_all_LD, REML = FALSE)
summary(youth_all_mdd_model_H1)

# Youth adhd model H1

youth_all_adhd_model_H1 <- lmer(youth_scl ~ ADHD_pgs_res.C + ADHD_pgs_res.M + ADHD_pgs_res.F + KJONN + age +  time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_all_LD, REML = FALSE)
summary(youth_all_adhd_model_H1)

# Youth neuroticism model H1 

youth_all_neuro_model_H1 <- lmer(youth_scl ~ neuro_pgs_res.C + neuro_pgs_res.M + neuro_pgs_res.F + KJONN + age  +  time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_all_LD, REML = FALSE)
summary(youth_all_neuro_model_H1)

# Youth AN model H1 

youth_all_an_model_H1 <- lmer(youth_scl ~ AN_pgs_res.C + AN_pgs_res.M + AN_pgs_res.F + KJONN + age +  time +  (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_all_LD, REML = FALSE)
summary(youth_all_an_model_H1)

tab_model(youth_all_anx_model_H1,youth_all_mdd_model_H1,youth_all_adhd_model_H1, youth_all_neuro_model_H1,youth_all_an_model_H1, show.ci = FALSE, show.se = TRUE, digits = 4)


# Covid-19 sample

# # Youth anxiey model for H1

youth_covid_anx_model_H1 <- lmer(youth_scl ~ anx_pgs_res.C + anx_pgs_res.M + anx_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_anx_model_H1)

# Youth anxiey model for H2

youth_covid_anx_model_H2 <- lmer(youth_scl ~ anx_pgs_res.C + anx_pgs_res.M + anx_pgs_res.F + KJONN + age + time + time:anx_pgs_res.C + time:anx_pgs_res.M + time:anx_pgs_res.F +
                           (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_anx_model_H2)
anova(youth_covid_anx_model_H2)
tab_model(youth_covid_anx_model_H1, youth_covid_anx_model_H2, show.ci = FALSE, show.se = TRUE, digits = 5)


# Youth depression model H1

youth_covid_mdd_model_H1 <- lmer(youth_scl ~ mdd_pgs_res.C + mdd_pgs_res.M + mdd_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_mdd_model_H1)

summary(youth_mdd_model_H1)
anova(youth_mdd_model_H1)

# Youth depression model H2

youth_covid_mdd_model_H2 <- lmer(youth_scl ~ mdd_pgs_res.C + mdd_pgs_res.M + mdd_pgs_res.F + KJONN + age + time + time:mdd_pgs_res.C + time:mdd_pgs_res.M + time:mdd_pgs_res.F +
                            (1 | M_ID_2306) + (1 | PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_mdd_model_H2)
anova(youth_covid_mdd_model_H2)
tab_model(youth_covid_mdd_model_H1, youth_covid_mdd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)


# Youth adhd model H1

youth_covid_adhd_model_H1 <- lmer(youth_scl ~ ADHD_pgs_res.C + ADHD_pgs_res.M + ADHD_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_adhd_model_H1)
anova(youth_adhd_model_H1)

# Youth adhd model H2

youth_covid_adhd_model_H2 <- lmer(youth_scl ~ ADHD_pgs_res.C + ADHD_pgs_res.M + ADHD_pgs_res.F + KJONN + age + time +  time:ADHD_pgs_res.C + time:ADHD_pgs_res.M + time:ADHD_pgs_res.F +
                                (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_adhd_model_H2)
anova(youth_adhd_model_H2)
tab_model(youth_covid_adhd_model_H1, youth_covid_adhd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Youth neuroticism model H1 

youth_covid_neuro_model_H1 <- lmer(youth_scl ~ neuro_pgs_res.C + neuro_pgs_res.M + neuro_pgs_res.F + KJONN + age + time + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_neuro_model_H1)
anova(youth_neuro_model_H1)

# Youth neuroticism model H2

youth_covid_neuro_model_H2 <- lmer(youth_scl ~ neuro_pgs_res.C + neuro_pgs_res.M + neuro_pgs_res.F + KJONN + age + time + time:neuro_pgs_res.C + time:neuro_pgs_res.M + time:neuro_pgs_res.F +
                                  (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_neuro_model_H2)
anova(youth_neuro_model_H2)
tab_model(youth_covid_neuro_model_H1, youth_covid_neuro_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Youth AN model H1 

youth_covid_an_model_H1 <- lmer(youth_scl ~ AN_pgs_res.C + AN_pgs_res.M + AN_pgs_res.F + KJONN + age + time  + (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_an_model_H1)
anova(youth_covid_an_model_H1)

# Youth AN model H2

youth_covid_an_model_H2 <- lmer(youth_scl ~ AN_pgs_res.C + AN_pgs_res.M + AN_pgs_res.F + KJONN + age + time + time:AN_pgs_res.C + time:AN_pgs_res.M + time:AN_pgs_res.F +
                               (1 | M_ID_2306) + (1|PREG_BARN_NR), data = YOUTH_covid_LD, REML = FALSE)
summary(youth_covid_an_model_H2)
anova(youth_covid_an_model_H2)
tab_model(youth_covid_an_model_H1, youth_covid_an_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

sample_youth_covid <- YOUTH_covid_LD[rownames(youth_covid_an_model_H1@frame),]

#Mother

# Mother anxiety model H1

mother_anx_model_H1 <- lmer(mother_scl ~ anx_pgs_res.M + anx_pgs_res.F + anx_pgs_res.C + time + KJONN + mat_age + (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_anx_model_H1)
anova(mother_anx_model_H1)

# Mother anxiety model H2

mother_anx_model_H2 <- lmer(mother_scl ~ anx_pgs_res.M + anx_pgs_res.F + anx_pgs_res.C + KJONN + time + mat_age + time:anx_pgs_res.M + time:anx_pgs_res.F + time:anx_pgs_res.C +
                          (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_anx_model_H2)
anova(mother_anx_model_H2)
tab_model(mother_anx_model_H1, mother_anx_model_H2, show.ci = FALSE, show.se = TRUE, digits = 5)

# Mother depression model H1

mother_mdd_model_H1 <- lmer(mother_scl ~ mdd_pgs_res.M + mdd_pgs_res.F + mdd_pgs_res.C + time + KJONN + mat_age + (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_mdd_model_H1)
anova(mother_mdd_model_H1)

# Mother depression model H2

mother_mdd_model_H2 <- lmer(mother_scl ~ mdd_pgs_res.M + mdd_pgs_res.F + mdd_pgs_res.C + time + KJONN + mat_age + time:mdd_pgs_res.M + time:mdd_pgs_res.F + time:mdd_pgs_res.C +
                           (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_mdd_model_H2)
anova(mother_mdd_model_H2)
tab_model(mother_mdd_model_H1, mother_mdd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 5)

# Mother adhd model H1

mother_adhd_model_H1 <- lmer(mother_scl ~ ADHD_pgs_res.M + ADHD_pgs_res.F + ADHD_pgs_res.C + time + KJONN + mat_age +(1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_adhd_model_H1)
anova(mother_adhd_model_H1)

# Mother adhd model H2

mother_adhd_model_H2 <- lmer(mother_scl ~ ADHD_pgs_res.M + ADHD_pgs_res.F + ADHD_pgs_res.C + time + KJONN + mat_age + time:ADHD_pgs_res.M + time:ADHD_pgs_res.F + time:ADHD_pgs_res.C +
                            (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_adhd_model_H2)
anova(mother_adhd_model_H2)
tab_model(mother_adhd_model_H1, mother_adhd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Mother neuroticism model H1

mother_neuro_model_H1 <- lmer(mother_scl ~ neuro_pgs_res.M + neuro_pgs_res.F + neuro_pgs_res.C + time + KJONN + mat_age + (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_neuro_model_H1)
anova(mother_neuro_model_H1)

# Mother neuroticism model H2

mother_neuro_model_H2 <- lmer(mother_scl ~ neuro_pgs_res.M + neuro_pgs_res.F + neuro_pgs_res.C + time + KJONN + mat_age + time:neuro_pgs_res.M + time:neuro_pgs_res.F + time:neuro_pgs_res.C +
                             (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_neuro_model_H2)
anova(mother_neuro_model_H2)
tab_model(mother_neuro_model_H1, mother_neuro_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Mother an model H1

mother_an_model_H1 <- lmer(mother_scl ~ AN_pgs_res.M + AN_pgs_res.F + AN_pgs_res.C + time + KJONN  + mat_age + (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_an_model_H1)
anova(mother_an_model_H1)

# Mother an model H2

mother_an_model_H2 <- lmer(mother_scl ~ AN_pgs_res.M + AN_pgs_res.F + AN_pgs_res.C + time + KJONN + mat_age + time:AN_pgs_res.M + time:AN_pgs_res.F + time:AN_pgs_res.C +
                          (1 | M_ID_2306), data = MOTHER_LD, REML = FALSE)
summary(mother_an_model_H2)
anova(mother_neuro_model_H2)
tab_model(mother_an_model_H1, mother_an_model_H2, show.ci = FALSE, show.se = TRUE, digits = 5)

sample_mother <- MOTHER_LD[rownames(mother_an_model_H1@frame),]

# Father

# Father anxiety model H1

father_anx_model_H1 <- lmer(father_scl ~ anx_pgs_res.F + anx_pgs_res.M + anx_pgs_res.C + time + KJONN + pat_age + (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_anx_model_H1)
anova(father_anx_model_H1)

# Father anxiety model H2

father_anx_model_H2 <- lmer(father_scl ~ anx_pgs_res.F + anx_pgs_res.M + anx_pgs_res.C + time + KJONN + pat_age + time:anx_pgs_res.F + time:anx_pgs_res.M + time:anx_pgs_res.C +
                           (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_anx_model_H2)
anova(father_anx_model_H2)
tab_model(father_anx_model_H1, father_anx_model_H2, show.ci = FALSE, show.se = TRUE, digits = 5)


# Father depression model H1

father_mdd_model_H1 <- lmer(father_scl ~ mdd_pgs_res.F + mdd_pgs_res.M + mdd_pgs_res.C + time + KJONN + pat_age + (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_mdd_model_H1)
anova(father_mdd_model_H1)

# Father depression model H2

father_mdd_model_H2 <- lmer(father_scl ~ mdd_pgs_res.F + mdd_pgs_res.M + mdd_pgs_res.C + time + KJONN + pat_age + time:mdd_pgs_res.F + time:mdd_pgs_res.M + time:mdd_pgs_res.C +
                           (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_mdd_model_H2)
anova(father_mdd_model_H2)
tab_model(father_mdd_model_H1, father_mdd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)


# Father adhd model H1

father_adhd_model_H1 <- lmer(father_scl ~ ADHD_pgs_res.F + ADHD_pgs_res.M + ADHD_pgs_res.C + time + KJONN + pat_age + (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_adhd_model_H1)
anova(father_adhd_model_H1)

# Father adhd model H2

father_adhd_model_H2 <- lmer(father_scl ~ ADHD_pgs_res.F + ADHD_pgs_res.M + ADHD_pgs_res.C + time + KJONN + pat_age + time:ADHD_pgs_res.F + time:ADHD_pgs_res.M + time:ADHD_pgs_res.C +
                            (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_adhd_model_H2)
anova(father_adhd_model_H2)
tab_model(father_adhd_model_H1, father_adhd_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Father neuroticism model H1

father_neuro_model_H1 <- lmer(father_scl ~ neuro_pgs_res.F + neuro_pgs_res.M + neuro_pgs_res.C + time + KJONN + pat_age + (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_neuro_model_H1)
anova(father_neuro_model_H1)

# Father neuroticism model H2

father_neuro_model_H2 <- lmer(father_scl ~ neuro_pgs_res.F + neuro_pgs_res.M + neuro_pgs_res.C + time + KJONN + pat_age + time:neuro_pgs_res.F + time:neuro_pgs_res.M + time:neuro_pgs_res.C +
                             (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_neuro_model_H2)
anova(father_neuro_model_H2)
tab_model(father_neuro_model_H1, father_neuro_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

# Father AN model H1

father_an_model_H1 <- lmer(father_scl ~ AN_pgs_res.F + AN_pgs_res.M + AN_pgs_res.C + time + KJONN + pat_age + (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_an_model_H1)
anova(father_neuro_model_H1)

# Father AN model H2

father_an_model_H2 <- lmer(father_scl ~ AN_pgs_res.F + AN_pgs_res.M + AN_pgs_res.C + time + KJONN + pat_age + time:AN_pgs_res.F + time:AN_pgs_res.M + time:AN_pgs_res.C +
                          (1 | F_ID_2306), data = FATHER_LD, REML = FALSE)
summary(father_an_model_H2)
anova(father_neuro_model_H2)
tab_model(father_an_model_H1, father_an_model_H2, show.ci = FALSE, show.se = TRUE, digits = 4)

sample_father <- FATHER_LD[rownames(father_an_model_H1@frame),]

