
**Project Title**: 

**Author**: Nam Dang

**Start Date**: October 1, 2025

**Duration**: October-December 2025

### Executive Summary

The primary goal of this project is to create a simulation-based model, leveraging techniques such as Markov Chain Monte Carlo (MCMC), Bayesian change-point detection, mixture models, and Hidden Markov Models (HMM) to make a comparisons between normal and abnormal trading behavior, allowing for the identification of potential manipulation.

**Key hypotheses** include detection of classic manipulative behaviors such as pump-and-dump schemes, wash trading, insider-driven trades, and spoofing. The project will operationalize these by analyzing anomalies in price, volume, and order flow dynamics.

**Data inputs** will include VN30 stock tick data (prices and volumes), supplemented by order book imbalance and event information when available. The workflow will involve establishing baseline models to represent expected market behavior, followed by anomaly detection using Bayesian or probabilistic models. Suspicious periods will be flagged when posterior manipulation probabilities exceed defined thresholds, enabling calculation of the percentage of market time under manipulation.
### Introduction

**Background**: The stock market in Vietnam is consider a Frontier Market, which is a country that is more established than the least developed countries but is smaller, less accessible, and riskier than more established markets. Risks of frontier markets include political instability, poor liquidity, inadequate regulation, substandard financial reporting, and large currency fluctuations. I have been investing in Vietnamese market since 2019 till now and it is safe to say that I can confirm why Vietnam's stock market have all the qualities that classified as a Frontier Market. 
All of this leading to the main reason why it is difficult for individual investors to invest in Frontier and even Emerging market: *Stock Manipulation*

**Purpose**: I want to create a model that can detect stock manipulation in a stock market using simulation like Markov Chain Monte Carlo or others. By define the right measurement with applied from what I will learn in this course, I want to be able to identify wherether a country market is currently being manipulate or not. 
### Execution
1. Define Manipulation Hypotheses
Examples of manipulative behaviors:
- Pump-and-dump → sudden spike in volume + price followed by crash.
- Wash trading → artificially inflated volumes without price change.
- Insider-driven trades → abnormal returns around earnings.
- Spoofing/layering → large fake orders that disappear.

$\to$ Create 2 models to compare: normal vs abnormal market behavior.

2.  Choose Modeling Approach
It is still not final but I am consider these models:
- Anomaly Detection Models
- Statistical control models: Compare actual returns/volume vs historical baseline.
Bayesian Change-Point Models 

Bayesian Mixture Models
- Treat observed market behavior as mix of:  
	- Legit trading distribution
	- Manipulative trading distribution

Hidden Markov Models 
- Market has latent states: {Normal, Manipulated}.
- Observations: price jumps, volume spikes, order imbalance.
- HMM learns transition probabilities between states.
- Application: label time periods as “normal” vs “manipulated."

3. Data Requirements
- Daily/hourly OHLC (open, high, low, close).
- Trading volume.
- Order book imbalance (if available).
- News/earnings events (to control for normal shocks).

4. Practical Workflow

- Collect Data: VN30 stock tick data (price + volume). If too complicated, I will pick a stock with high volume that represent the market.
- Preprocess: Remove known macro events (earnings, policy news).
- Baseline Model: Fit ARIMA/GARCH or Bayesian regression to predict expected returns/volume.
- Compare Residuals: Use MCMC-based mixture model or HMM to estimate probability of manipulation.
- Quantify Manipulation %: If posterior manipulation probability > 0.7, label suspicious.
- Compute share of time periods labeled “manipulated.”

### Conclusion
By combining simulation learned in this course with market data, this project will not only assess whether Vietnam’s stock market exhibits signs of manipulation but also provide a replicable framework for other emerging and frontier markets. 

  



