library(tidyquant) 
library(tidyverse) 
library(timetk) 
library(broom) 
library(highcharter) 
library(tibbletime) 
library(glue) 
library(scales) 
library(stargazer)
library(ggplot2)
library(zoo) 
library(forecast) 
library(tseries) 
library(tidyr)
library(quantmod)
library(purrr)
library(dplyr)
library(PerformanceAnalytics)
library(vtable)
library(AER)
library(plm)
library(mFilter)
library(dplyr)    
library(readxl)
library(texreg)
library(tidyr)
library(knitr)
library(kableExtra)
#####################################################        
#Data prep    
#####################################################
#Load the data

#Dataset_Philips_Curve <- read.csv(Path) #Uncomment to run as path to dataset is manual each case!
dataset<- pdata.frame(Dataset_Philips_Curve, index = c("Country", "Time"))

#####################################################        
#Data summary   
#####################################################

#Summary table for main statistics

summary_tibble <- dataset %>%
  summarise(
    across(
      c(Inflation, Unemployment.adjusted),
      list(
        N    = ~sum(!is.na(.)),
        Mean = ~mean(., na.rm = TRUE),
        SD   = ~sd(., na.rm = TRUE),
        Min  = ~min(., na.rm = TRUE),
        Max  = ~max(., na.rm = TRUE)
      )
    )
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to  = c("Variable", "Stat"),
    names_sep = "_"
  ) %>%
  tidyr::pivot_wider(names_from = Stat, values_from = value)

summary_tibble


summary_tibble <- summary_tibble |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

stargazer(summary_tibble,
          summary = FALSE,
          type = "latex",
          rownames = FALSE)



#Summary table by country

summary_by_country <- dataset %>%
  group_by(Country) %>%
  summarise(
    across(
      c(Inflation, Unemployment.adjusted),
      list(
        Mean = ~mean(., na.rm = TRUE),
        SD   = ~sd(., na.rm = TRUE),
        Min  = ~min(., na.rm = TRUE),
        Max  = ~max(., na.rm = TRUE)
      )
    )
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

summary_by_country

summary_by_country <- summary_by_country |>
  as.data.frame()
rownames(summary_by_country) <- summary_by_country$Country
summary_by_country$Country <- NULL


stargazer(summary_by_country,
          summary = FALSE,
          type = "latex",
          rownames = TRUE)


#Graph to show mean unemployment differences across countries 

a<-dataset %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(Mean_Unemployment = mean(Unemployment.adjusted, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Country, -Mean_Unemployment), y = Mean_Unemployment, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(
    title    = "Average Unemployment by Country (1999–2019)",
    x        = "Country",
    y        = "Mean Unemployment (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("Mean_Unemployment_Across_Countries.pdf", plot = a, width = 6, height = 4)

#Graph to show mean unemployment differences across countries adjusted for both periods
b<-dataset %>%
  dplyr::mutate(
    Year   = as.numeric(substr(as.character(Time), 1, 4)),  # extracts "1999" from "1999 Q1"
    Period = dplyr::case_when(
      Year < 2008 ~ "Pre-2008",
      Year > 2009 ~ "Post-2009"
    )
  ) %>%
  dplyr::filter(!is.na(Period)) %>%
  dplyr::group_by(Country, Period) %>%
  dplyr::summarise(Mean_Unemployment = mean(Unemployment.adjusted, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(Country, -Mean_Unemployment), y = Mean_Unemployment, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Pre-2008" = "steelblue", "Post-2009" = "tomato")) +
  labs(
    title = "Average Unemployment by Country: Pre-2008 vs Post-2009",
    x     = "Country",
    y     = "Mean Unemployment (%)",
    fill  = "Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Average Unemployment by Country_Pre-2008 vs Post-2009.pdf", plot = b, width = 6, height = 4)
getwd()



#Graph to show mean inflation differences across countries adjusted for both periods
c<-dataset %>%
  dplyr::mutate(
    Year   = as.numeric(substr(as.character(Time), 1, 4)),  # extracts "1999" from "1999 Q1"
    Period = dplyr::case_when(
      Year < 2008 ~ "Pre-2008",
      Year > 2009 ~ "Post-2009"
    )
  ) %>%
  dplyr::filter(!is.na(Period)) %>%
  dplyr::group_by(Country, Period) %>%
  dplyr::summarise(Mean_Inflation = mean(Inflation, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(Country, -Mean_Inflation), y = Mean_Inflation, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Pre-2008" = "darkgreen", "Post-2009" = "orange")) +
  labs(
    title = "Average Inflation by Country: Pre-2008 vs Post-2009",
    x     = "Country",
    y     = "Mean Inflation (%)",
    fill  = "Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Average Inflation by Country_Pre-2008 vs Post-2009.pdf", plot = c, width = 6, height = 4)

#####################################################        
#Data analysis of correlations   
#####################################################


library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Overall correlation (full sample) 
correl_overall <- dataset %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    Overall = round(cor(Inflation, Unemployment.adjusted, use = "complete.obs"), 3),
    .groups = "drop"
  )

#  Overall correlation excluding 2008-2009 
correl_excl <- dataset %>%
  dplyr::mutate(Year = as.numeric(substr(as.character(Time), 1, 4))) %>%
  dplyr::filter(!(Year %in% c(2008, 2009))) %>%        # <-- exclude crisis years
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    Excl.Crisis = round(cor(Inflation, Unemployment.adjusted, use = "complete.obs"), 3),
    .groups = "drop"
  )

# Pre/Post correlations (wide) 
correl_wide <- dataset %>%
  dplyr::mutate(
    Year   = as.numeric(substr(as.character(Time), 1, 4)),
    Period = dplyr::case_when(
      Year < 2008 ~ "Pre-2008",
      Year > 2009 ~ "Post-2009"
    )
  ) %>%
  dplyr::filter(!is.na(Period)) %>%
  dplyr::group_by(Country, Period) %>%
  dplyr::summarise(
    Correlation = round(cor(Inflation, Unemployment.adjusted, use = "complete.obs"), 3),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = Period,
    values_from = Correlation
  ) %>%
  dplyr::select(Country, `Pre-2008`, `Post-2009`)

# Merge all four columns 
correl_final <- correl_overall %>%
  dplyr::left_join(correl_excl,  by = "Country") %>%   # add excl. crisis
  dplyr::left_join(correl_wide,  by = "Country") %>%   # add pre/post
  dplyr::select(Country, Overall, Excl.Crisis, `Pre-2008`, `Post-2009`) %>%
  dplyr::arrange(Country)

#  Export to Latex 
knitr::kable(
  correl_final,
  format    = "latex",
  booktabs  = TRUE,
  digits    = 3,
  caption   = "Correlation Between Inflation and Unemployment by Country and Period",
  label     = "tab:correl",
  col.names = c("Country", "Full Sample", "Excl. 2008-2009", "Pre-2008", "Post-2009"),
  align     = c("l", "c", "c", "c", "c")
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::add_header_above(
    c(" " = 1, "Full Sample" = 2, "By Period" = 2)  # spans 2 + 2
  ) %>%
  kableExtra::column_spec(3, border_right = TRUE)    # separator after excl. column


#####################################################        
#Econometrics part for total sample
#####################################################


#####################################################        
#Regression 1: Pi=beta*U  
#####################################################

regression1 <- plm(Inflation ~ Unemployment.adjusted, 
                    data = dataset,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")


coeftest(regression1, vcov. = vcovDC(regression1))
#####################################################        
#Regression 2: Pi=beta*U+controls  
#####################################################

regression2 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating, 
                   data = dataset,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression2, vcov. = vcovDC(regression2))

#####################################################        
#Regression 3: Pi=beta*Ugap+controls  
#####################################################

#We start by estimating unemployment gap by using HP function

# Safe HP filter function that handles errors
safe_hpfilter <- function(x, freq = 1600) {
  tryCatch({
    clean_x <- x[!is.na(x)]           # remove NAs
    if (length(clean_x) < 8) {        # need minimum observations
      return(rep(NA, length(x)))
    }
    cycle <- hpfilter(clean_x, freq = freq)$cycle
    result <- rep(NA, length(x))
    result[!is.na(x)] <- cycle
    return(result)
  }, error = function(e) {
    return(rep(NA, length(x)))         # return NAs if filter fails
  })
}

# Apply safely by country
dataset <- dataset %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(
    unemp_gap = safe_hpfilter(Unemployment.adjusted, freq = 1600)
  ) %>%
  dplyr::ungroup()

# Reconvert to pdata.frame
dataset <- pdata.frame(dataset, index = c("Country", "Time"))

regression3 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating, 
                   data = dataset,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression3, vcov. = vcovDC(regression3))

#####################################################        
#Regression 4: Pi=beta*U+controls+expectations  
#####################################################
regression4 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating+Inflation.lagged,  
                   data = dataset,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression4, vcov. = vcovDC(regression4))
#####################################################        
#Regression 5: Pi=beta*Ugap+controls+expectations    
#####################################################

regression5 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating+Inflation.lagged,  
                   data = dataset,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression5, vcov. = vcovDC(regression5))



# Calculate Driscoll-Kraay robust standard errors
se1 <- sqrt(diag(vcovDC(regression1)))
se2 <- sqrt(diag(vcovDC(regression2)))
se3 <- sqrt(diag(vcovDC(regression3)))
se4 <- sqrt(diag(vcovDC(regression4)))
se5 <- sqrt(diag(vcovDC(regression5)))

t1 <- coef(regression1) / se1
t2 <- coef(regression2) / se2
t3 <- coef(regression3) / se3
t4 <- coef(regression4) / se4
t5 <- coef(regression5) / se5


stargazer(
  regression1, regression2, regression3, regression5,
  se = list(se1, se2, se3, se5),
  t  = list(t1, t2, t3, t5),
  type = "latex",
  title = "Phillips Curve Results",
  digits = 3,
  covariate.labels = c("Unemployment","Unemployment Gap", "Trade Union Density", 
                       "Globalisation",
                       "Lagged Inflation"),
  dep.var.labels = "Inflation",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  omit.stat = c("f", "ser")
)




#Subdata separation for both separate periods of interest
sub_data <- dataset %>% 
  dplyr::filter(as.numeric(substr(Time, 1, 4)) <= 2007)

sub_datap <- dataset %>% 
  dplyr::filter(as.numeric(substr(Time, 1, 4)) >= 2010)

dataset_excl <- dataset %>%
  dplyr::mutate(Year = as.numeric(substr(as.character(Time), 1, 4))) %>%
  dplyr::filter(!(Year %in% c(2008, 2009)))

#####################################################        
#Econometrics part for pre 2008 sample
#####################################################

#####################################################        
#Regression 1: Pi=beta*U  
#####################################################

regression11 <- plm(Inflation ~ Unemployment.adjusted, 
                   data = sub_data,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")


coeftest(regression11, vcov. = vcovDC(regression11))
#####################################################        
#Regression 2: Pi=beta*U+controls  
#####################################################

regression22 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating, 
                   data = sub_data,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression22, vcov. = vcovDC(regression22))

#####################################################        
#Regression 3: Pi=beta*Ugap+controls  
#####################################################


regression33 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating, 
                   data = sub_data,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression33, vcov. = vcovDC(regression33))

#####################################################        
#Regression 4: Pi=beta*U+controls+expectations  
#####################################################
regression44 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating+Inflation.lagged,  
                   data = sub_data,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression44, vcov. = vcovDC(regression44))
#####################################################        
#Regression 5: Pi=beta*Ugap+controls+expectations    
#####################################################

regression55 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating+Inflation.lagged,  
                   data = sub_data,
                   index = c("Country", "Time"), 
                   model = "within", effect = "twoways")

coeftest(regression55, vcov. = vcovDC(regression55))



# Calculate Driscoll-Kraay robust standard errors for the subsample
se11 <- sqrt(diag(vcovDC(regression11)))
se22 <- sqrt(diag(vcovDC(regression22)))
se33 <- sqrt(diag(vcovDC(regression33)))
se44 <- sqrt(diag(vcovDC(regression44)))
se55 <- sqrt(diag(vcovDC(regression55)))

t11 <- coef(regression111) / se11
t22 <- coef(regression222) / se22
t33 <- coef(regression333) / se33
t44 <- coef(regression444) / se44
t55 <- coef(regression555) / se55


stargazer(
  regression11, regression22, regression33, regression55,
  se = list(se11, se22, se33, se55),
  t  = list(t11, t22, t33, t55),
  type = "latex",
  title = "Phillips Curve Results for subsample 1999-2007",
  digits = 3,
  covariate.labels = c("Unemployment","Unemployment Gap", "Trade Union Density", 
                       "Globalisation",
                       "Lagged Inflation"),
  dep.var.labels = "Inflation",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  omit.stat = c("f", "ser")
)


#####################################################        
#Econometrics part for post 2009 sample
#####################################################


#####################################################        
#Regression 1: Pi=beta*U  
#####################################################

regression111 <- plm(Inflation ~ Unemployment.adjusted, 
                    data = sub_datap,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")


coeftest(regression111, vcov. = vcovDC(regression111))
#####################################################        
#Regression 2: Pi=beta*U+controls  
#####################################################

regression222 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating, 
                    data = sub_datap,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")

coeftest(regression222, vcov. = vcovDC(regression222))

#####################################################        
#Regression 3: Pi=beta*Ugap+controls  
#####################################################


regression333 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating, 
                    data = sub_datap,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")

coeftest(regression333, vcov. = vcovDC(regression333))

#####################################################        
#Regression 4: Pi=beta*U+controls+expectations  
#####################################################
regression444 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating+Inflation.lagged,  
                    data = sub_datap,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")

coeftest(regression444, vcov. = vcovDC(regression444))
#####################################################        
#Regression 5: Pi=beta*Ugap+controls+expectations    
#####################################################

regression555 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating+Inflation.lagged,  
                    data = sub_datap,
                    index = c("Country", "Time"), 
                    model = "within", effect = "twoways")

coeftest(regression555, vcov. = vcovDC(regression555))


# Calculate Driscoll-Kraay robust standard errors for the subsample
se111 <- sqrt(diag(vcovDC(regression111)))
se222 <- sqrt(diag(vcovDC(regression222)))
se333 <- sqrt(diag(vcovDC(regression333)))
se444 <- sqrt(diag(vcovDC(regression444)))
se555 <- sqrt(diag(vcovDC(regression555)))

t111 <- coef(regression111) / se111
t222 <- coef(regression222) / se222
t333 <- coef(regression333) / se333
t444 <- coef(regression444) / se444
t555 <- coef(regression555) / se555


stargazer(
  regression111, regression222, regression333, regression555,
  se = list(se111, se222, se333, se555),
  t  = list(t111, t222, t333, t555),
  type = "latex",
  title = "Phillips Curve Results for subsample 2010-2019",
  digits = 3,
  covariate.labels = c("Unemployment","Unemployment Gap", "Trade Union Density", 
                       "Globalisation",
                       "Lagged Inflation"),
  dep.var.labels = "Inflation",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  omit.stat = c("f", "ser")
)

#####################################################        
#Econometrics part for full sample, but excluded
#####################################################


#####################################################        
#Regression 1: Pi=beta*U  
#####################################################

regression1111 <- plm(Inflation ~ Unemployment.adjusted, 
                     data =dataset_excl,
                     index = c("Country", "Time"), 
                     model = "within", effect = "twoways")


coeftest(regression1111, vcov. = vcovDC(regression1111))
#####################################################        
#Regression 2: Pi=beta*U+controls  
#####################################################

regression2222 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating, 
                     data = dataset_excl,
                     index = c("Country", "Time"), 
                     model = "within", effect = "twoways")

coeftest(regression2222, vcov. = vcovDC(regression2222))

#####################################################        
#Regression 3: Pi=beta*Ugap+controls  
#####################################################


regression3333 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating, 
                     data = dataset_excl,
                     index = c("Country", "Time"), 
                     model = "within", effect = "twoways")

coeftest(regression3333, vcov. = vcovDC(regression3333))

#####################################################        
#Regression 4: Pi=beta*U+controls+expectations  
#####################################################
regression4444 <- plm(Inflation ~ Unemployment.adjusted+Trade.union.density+Globalization.rating+Inflation.lagged,  
                     data = dataset_excl,
                     index = c("Country", "Time"), 
                     model = "within", effect = "twoways")

coeftest(regression4444, vcov. = vcovDC(regression4444))
#####################################################        
#Regression 5: Pi=beta*Ugap+controls+expectations    
#####################################################

regression5555 <- plm(Inflation ~ unemp_gap+Trade.union.density+Globalization.rating+Inflation.lagged,  
                     data = dataset_excl,
                     index = c("Country", "Time"), 
                     model = "within", effect = "twoways")

coeftest(regression5555, vcov. = vcovDC(regression5555))


# Calculate Driscoll-Kraay robust standard errors for the subsample
se1111 <- sqrt(diag(vcovDC(regression1111)))
se2222 <- sqrt(diag(vcovDC(regression2222)))
se3333 <- sqrt(diag(vcovDC(regression3333)))
se4444 <- sqrt(diag(vcovDC(regression4444)))
se5555 <- sqrt(diag(vcovDC(regression5555)))

t1111 <- coef(regression1111) / se1111
t2222 <- coef(regression2222) / se2222
t3333 <- coef(regression3333) / se3333
t4444<- coef(regression4444) / se4444
t5555 <- coef(regression5555) / se5555


stargazer(
  regression1111, regression2222, regression3333, regression5555,
  se = list(se1111, se2222, se3333, se5555),
  t  = list(t1111, t2222, t3333, t5555),
  type = "latex",
  title = "Phillips Curve Results for full sample excluding 2008-2009",
  digits = 3,
  covariate.labels = c("Unemployment","Unemployment Gap", "Trade Union Density", 
                       "Globalisation",
                       "Lagged Inflation"),
  dep.var.labels = "Inflation",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  omit.stat = c("f", "ser")
)





