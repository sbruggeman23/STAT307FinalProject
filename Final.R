# Importing libraries
library(car)
library(MASS)
library(agricolae)

# Random ordering
brand = rep(c(-1, 1), times=12) # low in haribo
salinity = rep(c(-1, 1), each=2, times=6) # low is no salt
shape = rep(c(-1, 1), each=4, times=3) # low is bear?
bears = sample(1:24)
replicate = rep(1:3, each=8)
df = cbind(brand, salinity, shape, bears, replicate)
df

# Starting weights for each bear/worm
haribo_bear_start_weight=2
haribo_worm_start_weight=9
target_bear_start_weight=2
target_worm_start_weight=4

# Input results as dataframe
brand2 = as.factor(c(rep("Haribo", 12), 
          rep("Target", 14)))
salt2 = as.factor(c(rep("NoSalt", 6), rep("Salt", 6), 
         rep("NoSalt", 8), rep("Salt", 6)))
shape2 = as.factor(c(rep("Bear", 4), rep("Worm", 2), rep("Bear", 2), rep("Worm", 4), 
          rep("Bear", 4), rep("Worm", 4), rep("Bear", 4), rep("Worm", 2)))
weights = c(9,9,8,8,31,31,3,4,12,11,13,13,2,2,2,2,17,16,15,16,2,2,2,2,5,9)
df2 = data.frame(Brand=brand2, Salt=salt2, Shape=shape2, Weight=weights)

# Calculating percentage change in weight after letting sit in water.
# Using instead of raw change in weight so that it's fair to compare the worms and bears
df2['WeightChange'] = 0
df2[(df2$Brand=="Haribo")&(df2$Shape=="Bear"),'WeightChange'] = 
  df2[(df2$Brand=="Haribo")&(df2$Shape=="Bear"),'Weight'] / haribo_bear_start_weight * 100
df2[(df2$Brand=="Haribo")&(df2$Shape=="Worm"),'WeightChange'] = 
  df2[(df2$Brand=="Haribo")&(df2$Shape=="Worm"),'Weight'] / haribo_worm_start_weight * 100
df2[(df2$Brand=="Target")&(df2$Shape=="Bear"),'WeightChange'] = 
  df2[(df2$Brand=="Target")&(df2$Shape=="Bear"),'Weight'] / haribo_bear_start_weight * 100
df2[(df2$Brand=="Target")&(df2$Shape=="Worm"),'WeightChange'] = 
  df2[(df2$Brand=="Target")&(df2$Shape=="Worm"),'Weight'] / haribo_worm_start_weight * 100

# Initial ANOVA
fit = lm(WeightChange~(Brand+Shape+Salt)^3, data=df2)
aov_fit = aov(fit)
summary(aov_fit)

### Assumptions for initial ANOVA
# Normality (look very far off from normal)
shapiro.test(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)

# Homoscedasticity
leveneTest(aov_fit$residuals~Brand, data=df2) # only one below 0.05
leveneTest(aov_fit$residuals~Shape, data=df2)
leveneTest(aov_fit$residuals~Salt, data=df2)

# Plot of residuals against fitted values
plot(fit$fitted.values, fit$residuals)
abline(h=0)

# Transformation to make data more normal - taking square root of response
# Second model after transformation
df2$TransWeightChange = df2$WeightChange^0.5
fit = lm(TransWeightChange~(Brand+Shape+Salt)^3, data=df2)
aov_fit = aov(fit)
summary(aov_fit)

### Assumptions
# Normality
shapiro.test(fit$residuals) # Still doesn't look great, but the p-value is over 0.05
qqnorm(fit$residuals) 
qqline(fit$residuals)

# Homoscedasticity - all p-values now over 0.05
leveneTest(aov_fit$residuals~Brand, data=df2)
leveneTest(aov_fit$residuals~Shape, data=df2)
leveneTest(aov_fit$residuals~Salt, data=df2)

# Plot of residuals against fitted values
plot(fit$fitted.values, fit$residuals)
abline(h=0)

# Post-Hoc
lsd = LSD.test(aov_fit, trt=c("Brand", "Salt", "Shape"), p.adj="bonferroni")
print(lsd)
plot(lsd)
