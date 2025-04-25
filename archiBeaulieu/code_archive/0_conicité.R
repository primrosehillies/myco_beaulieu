archi$aire_conic <- (archi$diam_tig_princip_mm * archi$conicite_mm) - (archi$conicite_mm * (archi$diam_tig_princip_mm - archi$diam_rac_pivot_mm))

archi$tig_ratio <- archi$diam_tig_lat_mm / archi$diam_tig_princip_mm
archi$rac_ratio <- archi$diam_rac_lat_mm / archi$diam_rac_pivot_mm

archi$surface <- archi$empreinte_photo_mm * archi$y_photo_mm

archi$inflo <- archi$axes_caul_inflo / archi$axes_caul_totaux

archi %>%
  filter(tig_ratio > 1) %>%
  select(num, diam_tig_princip_mm, diam_tig_lat_mm)

archi %>%
  filter(rac_ratio > 1) %>%
  select(num, diam_tig_princip_mm, diam_tig_lat_mm)



#####
mod15 <- lm(aire_conic ~ species, data = archi)
mod15_aov <- aov(aire_conic ~ species, data = archi)

anova(mod15)
plot(mod15)

shapiro.test(mod15$residuals)
bptest(mod15)
dwtest(mod15)

summary(mod15)
boxplot(aire_conic ~ species, data = archi)



tuk15 <- glht(mod15_aov, linfct = mcp(species = "Tukey"))
# test de Tukey:
summary(tuk15) 

tuk.cld15 <- cld(tuk15, decreasing = TRUE)   
tuk.cld15 $mcletters$Letters

#####
archi$aire_conic_log <- log(archi$aire_conic)

mod16 <- lm(aire_conic_log ~ species, data = archi)
mod16_aov <- aov(aire_conic_log ~ species, data = archi)

anova(mod16)
plot(mod16)

shapiro.test(mod16$residuals)
bptest(mod16)
dwtest(mod16)

summary(mod16)
boxplot(aire_conic_log ~ species, data = archi)



tuk16 <- glht(mod16_aov, linfct = mcp(species = "Tukey"))
# test de Tukey:
summary(tuk16) 

tuk.cld16 <- cld(tuk16, decreasing = TRUE)   
tuk.cld16 $mcletters$Letters

#####
pairs.panels(archi[,c("aire_conic", "prof_pivot_mm", 
                      "tig_ratio", "rac_ratio", 
                      "site_type", "species",
                      "surface", "inflo"
                      )],
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)





#####
mod17 <- glm(axes_caul_totaux ~ tig_ratio, data = archi, family = poisson(link = "log"))
summary(mod17)

dispersion17 <- sum(residuals(mod17, type = "pearson")^2) / mod17$df.residual
dispersion17

chi17 <- 1 - pchisq(mod17$deviance, mod17$df.residual)
chi17
#####
mod18 <- glm(axes_caul_totaux ~ tig_ratio + species, data = archi, family = poisson(link = "log"))

dispersion18 <- sum(residuals(mod18, type = "pearson")^2) / mod18$df.residual
dispersion18

chi18 <- 1 - pchisq(mod18$deviance, mod18$df.residual)
chi18


summary(mod18)
#tig_ratio has a negative estimate ----> When values of tig_ratio get higher, axes_caul_totaux get smaller.
#Residual deviance (87) much lower than Null deviance (182), suggesting that the model explaines a significant amount of variation


ggplot(archi, aes(x = tig_ratio, y = axes_caul_totaux, color = species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = poisson(link = "log")), se = FALSE) +
  labs(x = "Ratio tige latérale / principale",
       y = "Axes caulinaires totaux") +
  theme_minimal() +
  theme(legend.position = "top")


emmeans18 <- emmeans(mod18, ~ species)

# Perform pairwise comparisons with Tukey adjustment
paircomp18 <- pairs(emmeans18, adjust = "tukey")
summary(paircomp18)




#####
mod19 <- glm(axes_caul_totaux ~ tig_ratio + site_type, data = archi, family = poisson(link = "log"))
dispersion19 <- sum(residuals(mod19, type = "pearson")^2) / mod19$df.residual
dispersion19

chi19 <- 1 - pchisq(mod19$deviance, mod19$df.residual)
chi19

summary(mod19)



boxplot(tig_ratio ~ site, data = archi)
boxplot(axes_caul_totaux ~ site, data = archi)




#####
mod20 <- lm(tig_ratio ~ site_type * species, data = archi)
mod20_aov <- aov(tig_ratio ~ site_type * species, data = archi)

anova(mod20)
plot(mod20)

shapiro.test(mod20$residuals)
bptest(mod20)
dwtest(mod20)

summary(mod20)
boxplot(tig_ratio ~ site_type * species, data = archi)



tuk20 <- glht(mod20_aov, linfct = mcp(species = "Tukey"))
# test de Tukey:
summary(tuk20) 

tuk.cld20 <- cld(tuk20, decreasing = TRUE)   
tuk.cld20 $mcletters$Letters



#####
mod21 <- lm(tig_ratio ~ site_type * species, data = archi)
mod21_aov <- aov(tig_ratio ~ site_type * species, data = archi)

anova(mod21)
plot(mod21)

shapiro.test(mod21$residuals)
bptest(mod21)
dwtest(mod21)

summary(mod21)
boxplot(tig_ratio ~ site_type * species, data = archi)



tuk21 <- glht(mod21_aov, linfct = mcp(species = "Tukey"))
# test de Tukey:
summary(tuk21) 

tuk.cld21 <- cld(tuk21, decreasing = TRUE)   
tuk.cld21 $mcletters$Letters
