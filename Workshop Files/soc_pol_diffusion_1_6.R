#soc_pol_diffusion.R
 
install.packages('netdiffuseR')
install.packages('stargazer')
install.packages("sandwich")
install.packages("lmtest")
library(netdiffuseR)
library(stargazer)
library(sandwich)
library(lmtest)

rm(list=ls()) # just in case: start from scratch


# load the modified stargazer functions for tables
# this loads stargazer 3 for the edited volume: stargazer3 includes corrected standard errors (they are not shown, but used for significance
# see explanation below)

source("http://www.barkhof.uni-bremen.de/~mwindzio/modified_stargazer.R")


# -- setwd("E:/projekte/a5/workshops/net_diffuse_R/concept/data/harmonized/") 

# - dear colleagues: in the afternoon, search for
# - "here just change time-of-adoption" (without quot. marks)
# - plug in >your< time of adoption

# - overview: steps in this script

# - 1. read networks. All data should already be standardized
#       a) cultural spheres 
#       b) trade networks
#       c) colonial ties (cross-sectional) => will be changed?
#       b) distance/prox. between capitals (cross-sectional)

# - 2. read explanatory variables, basics
# - 3. read years of >social policy introduction< for each project
# - 4. create netdiffuseR objects => generate plots if you like
#      repeat 4. for each network!
# - 5. get the respective exposure variables and time-of-adoption (toa)
# - 6. create process-time control
# - 7. combine the relevant information into one data frame
# - 8. !! estimate discrete-time logistic hazard model !!

#--- network data is huge, so we begin with with time-consuming download >first< 
#--- and unzip the data below with "unz" 

#------------------------------------
#------------------------------------
#------------------------------------
#--- 1. Networks here ---------------
#------------------------------------
#------------------------------------
#------------------------------------

#------------------------------------
#----- 1a) cultural spheres ---------
#------------------------------------

temp <- tempfile()
download.file("http://www.barkhof.uni-bremen.de/~mwindzio/edgelist_cultural_spheres.zip",temp)
edgelist_cultural_spheres <- read.csv(unz(temp, "edgelist_cultural_spheres.csv"))
unlink(temp)
head(edgelist_cultural_spheres)
dim(edgelist_cultural_spheres)

# - ids should be factor variables
edgelist_cultural_spheres$ego_id <- as.factor(edgelist_cultural_spheres$ego_id)
edgelist_cultural_spheres$alter_id <- as.factor(edgelist_cultural_spheres$alter_id)
head(edgelist_cultural_spheres)


#-----------------------------------------------------------
#----- 1b) trade network -----------------------------------
#-----------------------------------------------------------
temp <- tempfile()
download.file("http://www.barkhof.uni-bremen.de/~mwindzio/COW_trade_consolidated.zip",temp)
trade_dyadic_consolidated <- read.csv(unz(temp, "COW_trade_consolidated.csv"))
unlink(temp)

trade_dyadic <- trade_dyadic_consolidated
dim(trade_dyadic)
head(trade_dyadic)

summary(trade_dyadic$smoothtotrade)

ddat <- trade_dyadic[is.na(trade_dyadic$smoothtotrade)==FALSE,]
summary(ddat$smoothtotrade)
d <- density(ddat$smoothtotrade)
plot(d)
polygon(d, col="red", border="blue") 

names(trade_dyadic)[names(trade_dyadic)=="iso3_ego"] <- "ego_id"
names(trade_dyadic)[names(trade_dyadic)=="iso3_alter"] <- "alter_id"
head(trade_dyadic)

# - ids should be factor variables
trade_dyadic$ego_id <- as.factor(trade_dyadic$ego_id)
trade_dyadic$alter_id <- as.factor(trade_dyadic$alter_id)


# - NA to 0 and log positive values
trade_dyadic$smoothtotrade[trade_dyadic$smoothtotrade < 0] <- 0
trade_dyadic$smoothtotrade[is.na(trade_dyadic$smoothtotrade)] <- 0

summary(trade_dyadic$smoothtotrade)
head(trade_dyadic)

trade_dyadic$log_value <- ifelse(trade_dyadic$smoothtotrade > 0, log(trade_dyadic$smoothtotrade + 1), 0) # has been updated
summary(trade_dyadic$log_value)

# -- distribution is still extreme  
d <- density(trade_dyadic$log_value)
plot(d)
polygon(d, col="red", border="blue")
trade_edgelist <- trade_dyadic

# -- trade: is country non-existent in a particular moment? 
# regime type from vdem data
existence <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/trade_existence_dummy.csv", header = TRUE) # actor attributes
head(existence) 
names(existence)[names(existence)=="iso3_ego"] <- "iso3"



#------------------------------------
#----- 1c) colonial ties from COLDAT and own collection --
#------------------------------------
# Reference:Becker, Bastian, 2019, "Colonial Dates Dataset (COLDAT)", https://doi.org/10.7910/DVN/T9SDEW
# The network fulfills the following criteria:
## 1. Time-Variant: i.e. countries at yearly intervals from 1880 to 2010
## 2. Weighted: connections of countries have differing weights based on the time of colonial relation
##    a. existing colonial relationship has the value of 1
##    b. no colonial relationship/relationship before colonial started has the value of 0
##    c. past colonial relationship have the constant values of 0.3, 0.5 and 0.7 (because we assume still some influence)
##    d. create past colonial influence as decay parameter (as a log function and additionally as an exponential function)
## 3. Inclusion of not only European colonial powers, but every relationship in which power was asserted over a space
##    which was and is not still the space of said nation stateb exerting the power through "colonialism"; e.g. USSR.
## 4. Directed: i.e. (analogoues to survey data) countries that were colonized "nominate" their colonizers 

temp <- tempfile()
download.file("http://www.barkhof.uni-bremen.de/~mwindzio/colonies_network_20201001_BT.zip",temp)
colonial_ties_edgelist <- read.csv(unz(temp, "colonies_network_20201001_BT.csv"))
unlink(temp)
head(colonial_ties_edgelist)
names(colonial_ties_edgelist)[names(colonial_ties_edgelist)=="iso_o"] <- "ego_id"
names(colonial_ties_edgelist)[names(colonial_ties_edgelist)=="iso_d"] <- "alter_id"

# - ids should be factor variables
colonial_ties_edgelist$ego_id <- as.factor(colonial_ties_edgelist$ego_id)
colonial_ties_edgelist$alter_id <- as.factor(colonial_ties_edgelist$alter_id)
head(colonial_ties_edgelist)
unique(colonial_ties_edgelist$ego_id)
unique(colonial_ties_edgelist$year)
table(colonial_ties_edgelist$colony)
head(colonial_ties_edgelist)

#----------------------------------------------------------
#----- 1d) spatial distance/ prox. of capitals from CEPI --
#----------------------------------------------------------
# - this is only a cross-sectional network ! 
temp <- tempfile()
download.file("http://www.barkhof.uni-bremen.de/~mwindzio/capital_distance_edgelist.zip",temp)
capital_distance_edgelist <- read.csv(unz(temp, "capital_distance_edgelist.csv"))
unlink(temp)
head(capital_distance_edgelist)

# --  we use log of proximity
capital_distance_edgelist$log_dist <- log(capital_distance_edgelist$capdist)
log(7593.042) # => 8.934988, ok
capital_distance_edgelist$log_proximity <- 1/(capital_distance_edgelist$log_dist)
1/8.934988 # => 0.1119196,  ok
head(capital_distance_edgelist)
# capital_distance_edgelist <- subset(capital_distance_edgelist, select = -c(log_proximity) )
capital_distance_edgelist$proximity <- 1/(capital_distance_edgelist$capdist)

capital_distance_edgelist <- subset(capital_distance_edgelist,  select=c(ego, alter, year,proximity))

names(capital_distance_edgelist)[names(capital_distance_edgelist)=="ego"] <- "ego_id"
names(capital_distance_edgelist)[names(capital_distance_edgelist)=="alter"] <- "alter_id"

# - ids should be factor variables
capital_distance_edgelist$ego_id <- as.factor(capital_distance_edgelist$ego_id)
capital_distance_edgelist$alter_id <- as.factor(capital_distance_edgelist$alter_id)


#------------------------------------
#------------------------------------
#------------------------------------
#--- 2. explanatory variables -------
#------------------------------------
#------------------------------------
#------------------------------------

# --- we just use gdp and regime-type in this introduction

# GDP from vdem data
gdp <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/vdem_gdp_pc_INTERPOLATED_consolidated.csv", header = TRUE) # actor attributes
head(gdp) 

names(gdp)[names(gdp)=="filled_vdem_gdp_interpol"] <- "gdp" # it's just gdp

# regime type from vdem data
regime <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/vdem_regime__INTERPOLATED_consolidated.csv", header = TRUE) # actor attributes
head(regime) 
regime <- subset(regime, select=c(iso3, year,regime))


#-- merge here ----
covar_base <- subset(gdp, select=c(iso3, year,gdp)) 
head(covar_base)  

covar_base <- merge(covar_base, regime,by=c("iso3","year")) 
head(covar_base)



table(covar_base$year) # - every year on board, for 164 countries
table(covar_base$iso3) # - every country on board, for 131 years

#------------------------------------
#------------------------------------
#------------------------------------
#--- 3. projects here ---------------
#------------------------------------
#------------------------------------
#------------------------------------


# ------------------------------------
# ----- Project A 02 -----------------
# ------------------------------------

# Introduction of unemployment programme
A02_unemployment <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A02_unemployment_new.csv", header = TRUE) # actor attributes
head(A02_unemployment) 

# Introduction of work regulations
A02_workinj <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A02_workinj.csv", header = TRUE) # actor attributes
head(A02_workinj) 


# ------------------------------------
# ----- Project A 03 -----------------
# ------------------------------------

# ILO convention ratification data
A03_data <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A03_data.csv", header = TRUE) # actor attributes
head(A03_data)

# ------------------------------------
# ----- Project A 04 -----------------
# ------------------------------------

# Introduction of health care system
A04_HCS_toa <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A04_HCS_toa.csv", header = TRUE) # actor attributes
head(A04_HCS_toa)

# Introduction of sickness legislation
A04_sickness_toa <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A04_sickness_toa.csv", header = TRUE) # actor attributes
head(A04_sickness_toa)

# ------------------------------------
# ----- Project A 05 -----------------
# ------------------------------------

# ---- Introduction of compulsory education

A05_edu_intro_comp_schooling <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/A05_edu_intro_comp_schooling.csv", header = TRUE) # actor attributes
head(A05_edu_intro_comp_schooling)
table(A05_edu_intro_comp_schooling$toa)

# ------------------------------------
# ----- Project A 06 -----------------
# ------------------------------------


# Introduction of family legislation
A06_int <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/a06_int.csv", header = TRUE) # actor attributes
head(A06_int)


# --------------------------------------------------------------
# --- merge here !----------------------------------------------
# -- combine explanatory variables and "times-of-adoption" here, use merge!
# -- if you want to merge your personal x from your project, do it here!
# --------------------------------------------------------------

covar_ <- data.frame(covar_base)
head(covar_)

# ------------------------------------
# ----- Project A 02 -----------------
# ------------------------------------

A02 <- data.frame(subset(  A02_unemployment, select = c(iso3,unemp_firstlaw,year)  ))
covar_ <- merge(covar_, A02,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="unemp_firstlaw"] <- "toa_unemp_A02"
head(covar_)

# ------------------------------------
# ----- Project A 03 -----------------
# ------------------------------------
A03 <- data.frame(subset(  A03_data, select = c(iso3,toa,year)  ))
covar_ <- merge(covar_, A03,by=c("iso3","year")) 
names(covar_)[names(covar_)=="toa"] <- "toa_ILO_A03"
head(covar_)

# ------------------------------------
# ----- Project A 04 -----------------
# ------------------------------------
A04 <- data.frame(subset(  A04_HCS_toa, select = c(iso3,toa,year)  ))
covar_ <- merge(covar_, A04,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="toa"] <- "toa_HCS_A04"
head(covar_)

A04 <- data.frame(subset(  A04_sickness_toa, select = c(iso3,toa,year)  ))
covar_ <- merge(covar_, A04,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="toa"] <- "toa_sickn_A04"
head(covar_)

# ------------------------------------
# ----- Project A 05 -----------------
# ------------------------------------

A05 <- data.frame(subset(  A05_edu_intro_comp_schooling, select = c(iso3,toa,year)  ))
covar_ <- merge(covar_, A05,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="toa"] <- "toa_comp_edu_A05"
head(covar_)

# ------------------------------------
# ----- Project A 06 -----------------
# ------------------------------------
A06 <- data.frame(subset(  A06_int, select = c(iso3,toa_ml,year)  ))
covar_ <- merge(covar_, A06,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="toa_ml"] <- "toa_ml_A06"
head(covar_)

A06 <- data.frame(subset(  A06_int, select = c(iso3,toa_fa,year)  ))
covar_ <- merge(covar_, A06,by=c("iso3","year")) 
head(covar_)
names(covar_)[names(covar_)=="toa_fa"] <- "toa_fa_A06"
head(covar_)

# ------------------------------------
# ----- All -----------------
# ------------------------------------

covar_ <- merge(covar_, existence,by=c("iso3","year")) 
head(covar_)

# ------------------------------------
# ----- Check if toa is within timeframe
# ------------------------------------

summary(covar_$toa_comp_edu_A05) # if the toa is below 1880, or above 2010, it needs to be set to 1880 or to NA, see below:

# ------------------------------------
# only if nessesary! 

# We need to include the left censored cases (toa before 1880) into our dataset, but they are not at risk and will be automatically removed
# in the glm due to the time lag. 
# Right censonred cases need to be set to NA if toa is > 2010, since NetdiffusR will complain. 

covar_$toa_comp_edu_A05[covar_$toa_comp_edu_A05 < 1880] <- 1880
covar_$toa_comp_edu_A05[covar_$toa_comp_edu_A05 > 2010] <- NA

# ------------------------------------

covar <- covar_
dim(covar)
unique(covar$iso3)
unique(covar$year)
unique(edgelist_cultural_spheres$year)

head(covar)
# - combined covar object for all explanatory variables
# - covar$gdp & covar$regime

# --------------------------------------
# --------------------------------------
# -- 4. let's create the netdiffuser ---
# -- objects here ----------------------
# --------------------------------------
# --------------------------------------

# ---- keep in mind what we have for each project
# -     >>toas<<:
# - A 01:
# - A 02: toa_unemp_A02
# - A 03: toa_ILO_A03
# - A 04: toa_HCS_A04 and toa_sickn_A04
# - A 05: toa_comp_edu_A05
# - A 06: toa_ml_A06 and toa_fa_A06

# ---- keep in mind which networks we have
# -     >>edgelists<<: 
# - trade_edgelist                => trade network
# - edgelist_cultural_spheres     => cultural spheres
# - colonial_ties_edgelist        => colonial ties
# - capital_distance_edgelist    => d = distance , prox. = 1/d
# - make netdiffuse object: combine edgelist with covariate object
# - begin with cultural spheres


# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# ----- 4. create netdiffuseR object --------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------

# -------------------------
# - 4a) cultural spheres --
# -------------------------
# - here comes the core: creating the diffnet object
head(edgelist_cultural_spheres)
dim(edgelist_cultural_spheres)
diffnet_culture <- edgelist_to_diffnet(
  edgelist = edgelist_cultural_spheres[,1:2], # As usual, a two column dataset [,1:2]
  w        = edgelist_cultural_spheres$weight,
  t0       = edgelist_cultural_spheres$year,  # An integer vector with starting point of spell
  t1       = edgelist_cultural_spheres$year,  # An integer vector with the endpoint of spell
  undirected = TRUE,        # undirected network
  dat      = covar,         # Attributes dataset
  idvar    = "iso3",                  
  toavar   = "toa_comp_edu_A05",          # -- !! here just change time-of-adoption !! -- #
  timevar  = "year",
  keep.isolates = TRUE    # Keeping isolates (if there's any)
)
diffnet_culture
plot(diffnet_culture)
summary(diffnet_culture)

# the warning messages: In check_var_class_and_coerce(x, edgelist, c("factor", "integer",  :
# Coercing -ego_id- into character. are OK!
# -------------------------------------------------------------------------
# -- 5. get the respective exposure variables and time-of-adoption (toa)
# -- generate exposure and add to diffnet object
diffnet_culture[["lag_w_expo_culture"]] <- exposure(diffnet_culture, valued = T, lags = 1)#
diffnet_culture[["lag_expo_culture"]] <- exposure(diffnet_culture, valued = F, lags = 1)
diffnet_culture[["adopted"]]  <- toa_mat(diffnet_culture)$cumadopt

table(data.frame(diffnet_culture)$adopted) # don't care about no. adopted here
summary(data.frame(diffnet_culture)$lag_w_expo_culture)
head(data.frame(diffnet_culture)) # exposure is NA for 1st 164 cases => 1880
tail(data.frame(diffnet_culture))


# -------------------------
# - 4b) colonial ties -----
# -------------------------
# - here comes the core: creating the diffnet object
head(colonial_ties_edgelist)
dim(colonial_ties_edgelist)
diffnet_colony <- edgelist_to_diffnet(
  edgelist = colonial_ties_edgelist[,1:2], # As usual, a two column dataset [,1:2]
  w        = colonial_ties_edgelist$weight_decay_exp,
  t0       = colonial_ties_edgelist$year,  # An integer vector with starting point of spell
  t1       = colonial_ties_edgelist$year,  # An integer vector with the endpoint of spell
  undirected = FALSE,        # !!! change for >directed< network !!! => we provide it later
  dat      = covar,         # Attributes dataset
  idvar    = "iso3",                  
  toavar   = "toa_comp_edu_A05",          # -- !! here just change time-of-adoption !! -- #
  timevar  = "year",
  keep.isolates = TRUE    # Keeping isolates (if there's any)
)
diffnet_colony
plot(diffnet_colony)
summary(diffnet_colony)

# -------------------------------------------------------------------------
# -- 5. get the respective exposure variables and time-of-adoption (toa)
# compute exposure: Netdiffuser automatically identifies whether the input is dynamic or not.
diffnet_colony[["lag_w_expo_colony"]] <- exposure(diffnet_colony, valued = T, lags = 1)
diffnet_colony[["lag_expo_colony"]] <- exposure(diffnet_colony, valued = F, lags = 1)

diffnet_colony[["adopted"]]  <- toa_mat(diffnet_colony)$cumadopt

diffnet_colony
summary(diffnet_colony)
summary(data.frame(diffnet_colony)$lag_w_expo_colony)

# -------------------------
# - 4c) trade network -----
# -------------------------
# - here comes the core: creating the diffnet object
head(trade_edgelist)
dim(trade_edgelist)
diffnet_trade <- edgelist_to_diffnet(
  edgelist = trade_edgelist[,1:2], # As usual, a two column dataset [,1:2]
  t0       = trade_edgelist$year,  # An integer vector with starting point of spell
  t1       = trade_edgelist$year,  # An integer vector with the endpoint of spell
  w        = trade_edgelist$log_value,
  undirected = TRUE,        # undirected network
  dat      = covar,         # Attributes dataset
  idvar    = "iso3",                  
  toavar   = "toa_comp_edu_A05",          # -- !! here just change time-of-adoption !! -- #
  timevar  = "year",
  keep.isolates = TRUE    # Keeping isolates (if there's any)
)
diffnet_trade
plot(diffnet_trade)
summary(diffnet_trade)


# -------------------------------------------------------------------------
# -- 5. get the respective exposure variables and time-of-adoption (toa)
# compute exposure: Netdiffuser automatically identifies whether the input is dynamic or not.
diffnet_trade[["lag_w_expo_trade"]] <- exposure(diffnet_trade, valued = T, lags = 1)
diffnet_trade[["lag_expo_trade"]] <- exposure(diffnet_trade, valued = F, lags = 1)

diffnet_trade[["adopted"]]  <- toa_mat(diffnet_trade)$cumadopt

head(data.frame(diffnet_trade))
tail(data.frame(diffnet_trade))
summary(data.frame(diffnet_trade)$lag_w_expo_trade)


# -----------------------------
# - 4d) spatial proximity -----
# -----------------------------
# - here comes the core: creating the diffnet object
head(capital_distance_edgelist)
dim(capital_distance_edgelist)
diffnet_proximity <- edgelist_to_diffnet(
  edgelist = capital_distance_edgelist[,1:2], # As usual, a two column dataset [,1:2]
  t0       = capital_distance_edgelist$year,  # An integer vector with starting point of spell
  t1       = capital_distance_edgelist$year,  # An integer vector with the endpoint of spell
  w        = capital_distance_edgelist$proximity,
  undirected = TRUE,        # undirected network
  dat      = covar,         # Attributes dataset
  idvar    = "iso3",                  
  toavar   = "toa_comp_edu_A05",          # -- !! here just change time-of-adoption !! -- #
  timevar  = "year",
  keep.isolates = TRUE    # Keeping isolates (if there's any)
)
diffnet_proximity
plot(diffnet_proximity)
summary(diffnet_proximity) # - moran's I seems to be insignificant regarding
                           # - >unweighted< geodesic distance in network.
                           # - Unweighted network density is 100% => it's not a variable
                           # - but see hazard model below with 
                           # - with >exposure< by >weighted< distances!

# -------------------------------------------------------------------------
# -- 5. get the respective exposure variables and time-of-adoption (toa)
# compute exposure: Netdiffuser automatically identifies whether the input is dynamic or not.
diffnet_proximity[["lag_w_expo_proximity"]] <- exposure(diffnet_proximity, valued = T, lags = 1)
diffnet_proximity[["lag_expo_proximity"]] <- exposure(diffnet_proximity, valued = F, lags = 1)

diffnet_proximity[["adopted"]]  <- toa_mat(diffnet_proximity)$cumadopt

diffnet_proximity
summary(data.frame(diffnet_proximity)$lag_expo_proximity)
head(data.frame(diffnet_proximity))

# - save first diffnet object as data as data frame
diff_data_culture <- as.data.frame(diffnet_culture)
head(diff_data_culture)
table(diff_data_culture$adopted) # don't care about n. of cases

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# - 6. create process-time control, define the duration of episode
# -----------------------------------------------------------------
# -----------------------------------------------------------------

diff_data_culture$t <- (diff_data_culture$per - 1880 )
summary(diff_data_culture$t)

# -- create time periods for stepwise PC Rate model
summary(diff_data_culture$t)
diff_data_culture$t0_25 <- ifelse(diff_data_culture$t < 25, 1, 0)
diff_data_culture$t25_49 <- ifelse(diff_data_culture$t >= 25 
                                   & diff_data_culture$t < 50, 1, 0)
diff_data_culture$t50_74 <- ifelse(diff_data_culture$t >= 50 
                                   & diff_data_culture$t < 75, 1, 0)
diff_data_culture$t75_99 <- ifelse(diff_data_culture$t >= 75 
                                   & diff_data_culture$t < 100, 1, 0)
diff_data_culture$t100_130 <- ifelse(diff_data_culture$t >= 100, 1, 0)



# -----------------------------------------------------------
# -- 7. combine the relevant information into one data frame
# -----------------------------------------------------------
# --- merge exposure from other networks --------------------
# -----------------------------------------------------------

diff_data <- diff_data_culture

head(diff_data)
diffnet_colony <- data.frame(diffnet_colony)
diffnet_trade <- data.frame(diffnet_trade)
diffnet_proximity <- data.frame(diffnet_proximity)

# -------------------------------------------------------
# -------------- merge everthing together ---------------
# -------------------------------------------------------
# --- merge id is now "id",  automatically generated in netdiffuseR objects
d <- subset(  diffnet_colony, select = c(id,per,lag_w_expo_colony, lag_expo_colony)  )
diff_data <- merge(diff_data, d,by=c("id","per")) 
d <- subset(  diffnet_trade, select = c(id,per,lag_w_expo_trade, lag_expo_trade)  )
diff_data <- merge(diff_data, d,by=c("id","per")) 
d <- subset(  diffnet_proximity, select = c(id,per,lag_w_expo_proximity, lag_expo_proximity)  )
diff_data <- merge(diff_data, d,by=c("id","per")) 

head(diff_data)

# -------------------------------------------------------
# -------------- include cluster id ---------------
# -------------------------------------------------------
#load cluster id
cluster_id <- read.csv("http://www.barkhof.uni-bremen.de/~mwindzio/clusterID_n.csv", header = TRUE)
head(cluster_id) 
cluster_id <- cluster_id[,c(1,2,5)]

names(cluster_id) <- c("id", "per", "cluster_id")
diff_data <- merge(diff_data, cluster_id,by=c("id","per"))
head(diff_data)


# - distribution of network, range and "density/edge weight" => 164 NAs for 1880
summary(diff_data$lag_w_expo_culture)
summary(diff_data$lag_w_expo_colony)
summary(diff_data$lag_w_expo_trade)
summary(diff_data$lag_w_expo_proximity)

summary(diff_data$adopted) # - create new for each project specific-toa
diff_data$gdp10000 <- diff_data$gdp/10000 # in 10,000 USD

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# - 8. !! estimate discrete-time logistic hazard model !!
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# --- check cases never in risk set
# --- check toa 1880 !!!
head(diff_data)

# Analysis 1: cultural ties network

model1 <- glm(adopted ~ 
              + t0_25 
              + t25_49 
              + t50_74 
              + t75_99 
              + t100_130  
              + existence
              + gdp10000
              + regime
              + lag_w_expo_culture
              + 0,
      dat    = diff_data, 
      subset = (per <= toa), # - extremely important !
      family = binomial(link="logit"))
summary(model1)
exp(coef(model1))

# Analysis 2: cultural ties network + colonial ties network

model2 <- glm(adopted ~ 
              + t0_25 
              + t25_49 
              + t50_74 
              + t75_99 
              + t100_130  
              + existence
              + gdp10000
              + regime
              + lag_w_expo_culture
              + lag_w_expo_colony
              + 0,
              dat    = diff_data, 
              subset = (per <= toa), # - extremely important !
              family = binomial(link="logit"))
summary(model2)
exp(coef(model2))

# Analysis 3: cultural ties network + colonial ties network + trade network

model3 <- glm(adopted ~ 
              + t0_25 
              + t25_49 
              + t50_74 
              + t75_99 
              + t100_130  
              + existence
              + gdp10000
              + regime
              + lag_w_expo_culture
              + lag_w_expo_colony
              + lag_w_expo_trade
              + 0,
              dat    = diff_data, 
              subset = (per <= toa), # - extremely important !
              family = binomial(link="logit"))
summary(model3)
exp(coef(model3))

# Analysis 4: cultural ties network + colonial ties network + trade network + spatial proximity network

model4 <- glm(adopted ~ 
              + t0_25 
              + t25_49 
              + t50_74 
              + t75_99 
              + t100_130  
              + existence
              + gdp10000
              + regime
              + lag_w_expo_culture
              + lag_w_expo_colony
              + lag_w_expo_trade
              + lag_w_expo_proximity
              + 0,
              dat    = diff_data, 
              subset = (per <= toa), # - extremely important !
              family = binomial(link="logit"))
summary(model4)
exp(coef(model4))


# Table for overview - does not include odds ratios or corrected standard errors!!
# please use the table below for the publication

stargazer(model1,model2,model3, model4,
          type = "html", 
          title = "Global Network Diffusion of Compulsory Education",
          dep.var.labels=c("Introduction of Compulsory Schooling"),
          covariate.labels=c("rate t(0-24)", 
                             "rate t(25-49)",
                             "rate t(50-74)",
                             "rate t(75-99)",
                             "rate t(100-130)",
                             "state existed (=1,else=0)", 
                             "GDP per capita / 10000 USD",
                             "democratization" ,
                             "cultural spheres netw.: w. exposure (lag 1 year)",
                             "colonies netw.: exposure ",
                             "trade net: w. exposure (lag 1 year)",
                             "spatial proximity netw.: w. exposure "
                             ),
          out="c:/temp/models_complusory_education.htm") # - create folder, mac users adjust

# ---------------------------------------------
# --- Calculate the  corrected standard errors
# --- the cultural spheres network
# ---------------------------------------------

# Standard error correction: Huber-White standard errors are calculated AFTER the model
# and used to re-calculate the significance

# step 1: calculate corrected standard errors and save output as an object: 


m1 <- coeftest(model1, vcov = vcovCL(model1, type="HC3", cluster=~ cluster_id))
               
m1

m2 <- coeftest(model2, vcov = vcovCL(model2, type="HC3", cluster=~ cluster_id))

m2

m3 <- coeftest(model3, vcov = vcovCL(model3, type="HC3", cluster=~ cluster_id))

m3

m4 <- coeftest(model4, vcov = vcovCL(model4, type="HC3", cluster=~ cluster_id))

m4

# step 2: run stargazer3 with the following specification: 

# 1. odds.rations = T !SPELLING! 

# 2. stargazer3(list(m1, m2)...) this is the output of the coeftest function with the corrected
# standard erors!! NOT THE GLM OUTPUT !!

# 3. origin_model: GLM output. This is nessesary to add the model fit statistics to the table.


# BEWARE of the covariate label order!! All your models need the independent variables
# in the same order. If you have multiple models, add the ALL independent variable names to the function call below under covariate.labels!!
# If one model has less variables, this space will be empty only for that model.

# cheatsheets for starger modification: https://www.jakeruss.com/cheatsheets/stargazer/
# these work with stargazer 3 as well. 

# change the title, the dep.var.labels and the covariate.labels accordingly

stargazer3(list(m1, m2, m3, m4), odds.ratios = T, origin_model = list(model1, model2, model3, model4), type = "html", se = list(NA, NA, NA, NA),
           title = "Diffusion of Compulsory Education - Cultural Spheres Network",
           dep.var.labels=c("Introduction of Compulsory Schooling"),
           covariate.labels=c("rate t(0-24)", 
                              "rate t(25-49)",
                              "rate t(50-74)",
                              "rate t(75-99)",
                              "rate t(100-130)",
                              "state existed (=1,else=0)", 
                              "GDP per capita / 10000 USD",
                              "democratization" ,
                              "cultural spheres netw.: w. exposure (lag 1 year)",
                              "colonies netw.: exposure ",
                              "trade net: w. exposure (lag 1 year)",
                              "spatial proximity netw.: w. exposure "
                              ),
           out="models_final2.htm")

# ---------------------------------------------
# --- some functions of netdiffuseR, here for 
# --- the cultural spheres network
# ---------------------------------------------

s <- 12345
cols <- c("lightblue","green", "blue")
oldpar <- par(no.readonly = T)
coords <- set.seed(s);plot(diffnet_culture, main="culture & education")


bmp(filename = "c:/temp/culture_edu.bmp") # - create folder, mac users adjust
plot_diffnet(diffnet_culture, 
             slices = c(1,50,100,130), layout=coords)
dev.off()

plot_adopters(diffnet_culture, 
              include.legend = FALSE, what = c("adopt", "cumadopt"))
plot_hazard(diffnet_culture, ylim=c(0,1))
plot_infectsuscep(diffnet_culture)

# network threshold: required proportion or number of neighbors that leads you to adopt
plot_threshold(diffnet_culture, undirected = FALSE, vertex.size = 1/5)

# Classify Adopters
diffnet.toa(diffnet_culture)[diffnet_culture$toa==max(diffnet_culture$toa, na.rm = TRUE)] <- NA
out <- classify_adopters(diffnet_culture)
out

# This is one way to combine adopters ad thresholds in one object
round(
  with(out, ftable(toa, thr, dnn=c("Time of Adoption", "Threshold")))/
    nnodes(diffnet_culture[!is.na(diffnet_culture$toa)])*100, digits=2)
ftable(out)

ids <- unique(edgelist_cultural_spheres$ego_id) 
ids <- as.data.frame(ids)

ids1 <- unique(edgelist_cultural_spheres$alter_id) 
ids1 <- as.data.frame(ids1)

colnames(ids1)[1] <- "ids"
ids <- rbind(ids, ids1)
ids <- unique(ids)
ids$ids <- as.character(ids$ids)

ids <- sort(ids)

# Adopter types and threshols combined
thresholds <- cbind(as.data.frame(classify(diffnet_culture)), diffnet_culture$toa, ids)
head(thresholds)

# --- END ----
