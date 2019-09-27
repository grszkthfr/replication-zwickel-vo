library(tidyverse)
library(here)
library(fs)
library(png)
library(afex)
afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.
library(kableExtra)
library(scales) # for percent_format()
library(emmeans)
library(ggplot2)
library(cowplot)
library(papaja)
library(lme4)

