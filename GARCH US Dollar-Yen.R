# Loading package pacman and use its function p_load for loading all required packages
library(pacman)
p_load(tibbletime, timetk, tidyverse, broom, rugarch)

# Import data set
Yen_Dollar <- read.csv("Yen_Dollar.csv") %>% 
              mutate(date = ymd(date))

# Convert Yen per Dollar object to a tibble
Yen_Dollar <- Yen_Dollar %>% 
              as_tibble()

head(Yen_Dollar)

# Calculate monthly log returns
Yen_Dollar <- Yen_Dollar %>% 
  mutate(return = (log(Yen_Dollar$Yen_per_Dollar) - log(lag(Yen_Dollar$Yen_per_Dollar)))) %>% 
  na.omit() %>% 
  select(-Yen_per_Dollar)

head(Yen_Dollar)

# Plot of Yen per Dollar returns
Yen_per_Dollar_Return <- Yen_Dollar %>% 
  ggplot(aes(x = date, y = return)) +
  geom_line() +
  labs(title = "Yen per Dollar return", x  = "date")

Yen_per_Dollar_Return

## Save plot as pdf file
ggsave("Yen_per_Dollar_return.pdf")

# Auto-Correlation Function and Partial Autocorrelation Function
acf(Yen_Dollar$return)
pacf(Yen_Dollar$return)

# Select autocorrelation model
ar_1 <- arima(Yen_Dollar$return, c(1,0,0))
ar_1

# Select ARMA model
arma_1 <- arima(Yen_Dollar$return, c(1,0,1))
arma_1

# Set up standard GARCH model with mean model = AR(1) and GARCH(1,1)
sgarch_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
mean.model = list(armaOrder = c(1, 0), include.mean = TRUE, archm = FALSE),
distribution.model = "std", start.pars = list(), fixed.pars = list())

# Fit model to Yen per Dollar
sgarch_model <- ugarchfit(sgarch_11, data = Yen_Dollar$return)

# See model result
sgarch_model

coef(sgarch_model)

# Add volatility to Yen per Dollar object
Yen_Dollar <- Yen_Dollar %>% 
              mutate(sGARCH_volatility  = sgarch_model@fit$sigma^2)

# Plot Yen per Dollar volatility from GARCH 11 model
GARCH11_volatility <- Yen_Dollar %>% 
  ggplot(aes(x = date, y = sGARCH_volatility)) +
  geom_line() +
  labs(title = "GARCH 11 model volatility", x  = "date")

GARCH11_volatility

## Save plot as pdf
ggsave("GARCH11_volatility.pdf")

# Set up Exponential GARCH 11 model
egarch_11 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE, archm = FALSE),
  distribution.model = "std", start.pars = list(), fixed.pars = list())

# Fit EGARCH 11 model to Yen per Dollar
egarch_model <- ugarchfit(egarch_11, data = Yen_Dollar$return)

# Check on coefficients of EGARCH 11 model
egarch_model

coef(egarch_model)

# Add volatility of EGARCH 11 model to Yen per Dollar object
Yen_Dollar <- Yen_Dollar %>% 
  mutate(eGARCH_volatility  = egarch_model@fit$sigma^2)
            
head(Yen_Dollar)
   
cor(Yen_Dollar$sGARCH_volatility, Yen_Dollar$eGARCH_volatility)

# Add volatility difference to Yen per Dollar object
Yen_Dollar <- Yen_Dollar %>% 
  mutate(volatility_diff  = Yen_Dollar$sGARCH_volatility - Yen_Dollar$eGARCH_volatility)

# Prepare data for plotting sGARCH and eGARCH volatility 
Yen_Dollar_volatilities <- Yen_Dollar %>% 
  select(date, sGARCH_volatility, eGARCH_volatility) %>%  
  gather(key = "variable", value = "value", -date)

# Plot sGARCH and eGARCH volatility 
GARCH_volatilities <- Yen_Dollar_volatilities %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "blue")) +
  labs(title = "sGARCH and eGARCH volatility", x  = "date")

GARCH_volatilities

## Save plot as pdf file
ggsave("GARCH_volatilities.pdf")

# Scatterplot of sGARCH and eGARCH difference
GARCH_difference <- Yen_Dollar %>% 
  ggplot(aes(x = volatility_diff, y = return)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "sGARCH - eGARCH", x  = "sGARCH - eGARCH")

GARCH_difference

## Save plot as pdf file
ggsave("GARCH_difference.pdf")
