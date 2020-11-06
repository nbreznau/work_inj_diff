# Using stargazer with odds ratios

# install.packages("stargazer")
library(stargazer)


# Stargazer has Problems with the standard errors and significances. Using the apply.coef and apply.se functions does not help
# neither does specifying p outside of stargazer and using it with custom.coef. Thankfully someone wrote a function for odds ratios
# stargazer2 can be used like stargazer, it only hast to be loaded into the environment 

# 1. load the stargazer2 function: 

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

# 2. run stargazer2 with odd.ration = T !SPELLING!

stargazer2(list(model_final_1, model_final_2, model_final_3), odd.ratio = T, type = "html", 
          title = "Diffusion of Compulsory Education - Cultural Spheres Network",
          dep.var.labels=c("Introduction of Compulsory Schooling"),
          covariate.labels=c("Weighted Exposure (complete network)",
                             "Weighted Exposure (only links with weight > 1)",
                             "Weighted Exposure (only links with weight > 2)",
                             "Year", 
                             "Year (quadratic)", 
                             "Log of GDP per capita", 
                             "Lower Middle Income",
                             "Upper Middle Income", 
                             "High Income",
                             "Constant"),
          out="models_final.htm")
