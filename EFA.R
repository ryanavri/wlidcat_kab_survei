#load library####
library(psych)
library(corrplot)
library(GPArotation)
library(ggplot2)
library(car)

#load data####
data <- read.csv("data/EFA.csv",header=TRUE) 
describe(data)

#  Correlation matrix####
## look at the correlations among our variables
datamatrix <- cor(data)
corrplot(datamatrix, method= "number")

#  KMO####
## Used to measure sampling adequacy is a better measure of factorability
KMO(r=cor(data))
#KMO values between 0.8 and 1 indicate the sampling is adequate.
#KMO values less than 0.6 indicate the sampling is not adequate and that remedial action should be taken. Some authors put this value at 0.5, so use your own judgment for values between 0.5 and 0.6.
#KMO Values close to zero means that there are large partial correlations compared to the sum of correlations. In other words, there are widespread correlations which are a large problem for factor analysis.

# Bartlett’s Test of Sphericity####
cortest.bartlett(data)
# p.value <0.05 indicate that a factor analysis may be useful with our data

#  Parallel Analysis####
## To find an acceptable number of factors and generate the `scree plot`

parallel <- fa.parallel(data, 
                        fm = "ml", #varies between methods
                        fa = "fa")     #varies between methods

# 1. Look at the large drops in the actual data and spot the point where it levels off to the right
# 2. Locate the point where the gap between simulated data and actual data tends to be minimum


#  Factor Analysis####

tf <- fa(data, nfactors = 3,        #varies between methods
         rotate = "oblimin", #varies between methods
         fm="ml")            #varies between methods
print(tf)
#look at significant values
print(tf$loadings,cutoff = 0.3) 

# evalute factor analysis by;
# RMSR should close to 0
# RMSEA < 0.05
# Tucker Lewis Index > 0.9

# look at the factor mapping
fa.diagram(tf)


# GLM analysis ####
library(tidyverse)
library(glmnet)
library(report)
library(MuMIn)

#load data
data <- read.csv()

# modif dataset dengan mengubah role menjadi factor dan sisanya con.standardize
datas <- data %>% mutate(Role = as.factor(Role)) %>%
  mutate(across(.cols = 4:12, .fns = scale))

# global model
#model_global <- glm(Role ~ demografi + persepsi + perilaku + niat + resiko + ternak + muncul + konsumsi + pengetahuan, 
#             data = datas,
#             family = "binomial")
# gunakan model diatas untuk melihat model tunggal dari best model hasil dredging


model_global <- glm(Role ~ demografi + persepsi + perilaku + niat + resiko + ternak + muncul + konsumsi + pengetahuan, 
                    data = datas,
                    family=binomial(logit),
                    na.action = "na.fail")

mod_dredge <- dredge(model_global)

