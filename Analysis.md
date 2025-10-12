

## Step 1: Load and Clean data
```{r}
# Load libraries
library(tidyverse)
library(lubridate)
library(mclust)       # For Gaussian Mixture Models
library(changepoint)  # For regime change detection

# Load your dataset
tcb <- read_csv("TCB_2018_2025.csv")

# Inspect
glimpse(tcb)

# Clean and format
tcb <- tcb %>%
  rename(Date = date,
         Open = open,
         High = high,
         Low  = low,
         Close = close,
         Volume = volume.million.,
         PctChange = percent.change
) %>%
  mutate(
    Date = ymd(Date),
    Volume = as.numeric(gsub(",", "", Volume)),
    Return = log(Close / lag(Close)),
    RelVolume = Volume / zoo::rollmean(Volume, 30, fill = NA, align = "right"),
    Volatility = (High - Low) / Open
  ) %>%
  drop_na()
 
```

## Step 2: Create  indicators that highlight unusual trading patterns

```{r}
tcb <- tcb %>%
  mutate(
    VolumeZ = scale(Volume),
    ReturnZ = scale(Return),
    VolatilityZ = scale(Volatility),
    PriceVolumeRatio = abs(ReturnZ) * VolumeZ
  )
```

## Step 3: Fit Bayesian model to detect latent normal vs maniplated regimes using Gaussian Mixture Models
```{r}
# Use standardized features
X <- tcb %>% select(ReturnZ, VolumeZ, VolatilityZ) %>% na.omit()

# Fit a mixture model (Bayesian information used automatically)
gmm_model <- Mclust(X, G = 1:4)  # Try 1–4 clusters

summary(gmm_model)
plot(gmm_model, what = "classification")

# Add cluster assignments and probabilities
tcb$Cluster <- NA
tcb$Cluster[!is.na(tcb$ReturnZ)] <- gmm_model$classification
tcb$ManipProb <- NA
tcb$ManipProb[!is.na(tcb$ReturnZ)] <- apply(gmm_model$z, 1, max)
```
![GMM model](Data/gmm.png)


### Step 4: Find sudden regime shift that might indicate manipulation burst

```{r}
# Detect change points in returns or volume
cp_return <- cpt.meanvar(tcb$Return, method = "PELT")
cp_volume <- cpt.meanvar(tcb$Volume, method = "PELT")

plot(cp_return, main = "Change Points in Returns")
plot(cp_volume, main = "Change Points in Volume")

```

### Step 5: Identify suspicious period by combining mixture model & change point output
```{r}
suspicious <- tcb %>%
  filter(ManipProb > 0.8 | Date %in% cpts(cp_volume) | Date %in% cpts(cp_return))

print(suspicious %>% select(Date, Close, Volume, ManipProb))
```

### Step 6: Visualize
```{r}
library(ggplot2)

ggplot(tcb, aes(Date, Close)) +
  geom_line() +
  geom_point(data = suspicious, aes(y = Close), color = "red", size = 2) +
  labs(title = "TCB Potential Manipulation Points",
       subtitle = "Red = detected anomalies")
```
