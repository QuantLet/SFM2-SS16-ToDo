<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: SFESimADF

Published in: Statistics of Financial Markets

Description: Simulates Augmented Dickey-Fuller tests for stationary and non-stationary ARMA processes. The simulated process x(t) is x(t) = alpha x(t-1) + beta epsilon(t-1) + epsilon(t), where the error term is a Gaussian White Noise process. Each process is simulated 1000 times. For each simulated process the number of lags (p) included for the ADF test varies between 3 to 11.

Keywords: 
- time series
- ADF
- simulation
- stationary
- explosive
- unit root
- power of test
- level of test
- Augmented Dickey-Fuller

Author: Christoph Schult

Submitted: Mon, May 09 2016 by Christoph Schult

Output: A plot for the power (alpha = 0.9) and the level (alpha = 1) of Augmented Dickey-Fuller tests. The rejection probability for beta = -0.99 (black), beta = -0.90 (red), beta = 0 (blue), beta = -0.90 (green) and beta = -0.99 (magenta) depending on the number of included lags are displayed. In addition two tables for the power and level of the test are printed for the minimum and maximum number of lags (p).

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFM2-SS16-ToDo/master/SFESimADF/SFESimADF.png" alt="Image" />
</div>

