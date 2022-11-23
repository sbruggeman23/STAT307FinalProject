# Additional Analysis Testing Area

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
# Adding starting weight as new column in dataframe
df2['StartWeight'] = 0
df2[(df2$Brand=="Haribo")&(df2$Shape=="Bear"),'WeightChange'] = haribo_bear_start_weight
df2[(df2$Brand=="Haribo")&(df2$Shape=="Worm"),'WeightChange'] = haribo_worm_start_weight
df2[(df2$Brand=="Target")&(df2$Shape=="Bear"),'WeightChange'] = haribo_bear_start_weight
df2[(df2$Brand=="Target")&(df2$Shape=="Worm"),'WeightChange'] = haribo_worm_start_weight * 100

