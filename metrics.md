{'mape': 1.6839518638760533, 
'me': -1479964.6872655596, 
'mae': 22528319.319363702, 
'mpe': 1.2670215471453081, 
'rmse': 28393025.145441916, 
'acf1': 0.12815219891038893, 
'corr': 0.10766653332772935, 
'minmax': 0.43070719150698755}

The mean absolute percentage error (MAPE)  168% 1-1.68=-68%accuracy
**is the most common measure used to forecast error, and works best if there are no extremes to the data (and no zeros).**
**has major drawbacks in practical application, and there are many studies on shortcomings and misleading results from MAPE.[4][5]

-It cannot be used if **there are zero values (which sometimes happens for example in demand data) because there would be a division by zero.
-For forecasts which are too low the percentage error cannot exceed 100%, but for **forecasts which are too high there is no upper limit to the percentage error.

ME=bias -1479964
Note that the error is the forecast minus the demand. So **a negative bias means that you undershoot the demand.

Obviously, just with the bias as an indicator of your forecast quality you will never be able to asses its precision. 
But a highly biased forecast is already an indication that something is wrong in the forecast model.
https://supchains.com/article/forecast-kpi-bias-mae-mape-rmse/

MAE 22528319
The mean absolute error (MAE) is the simplest regression error metric to understand. 
We’ll calculate the residual for every data point, taking only the absolute value of each so that negative and positive residuals do not cancel out. 
We then take the average of all these residuals. 
Effectively, MAE describes the typical magnitude of the residuals.

The MAE is also the most intuitive of the metrics since we’re just looking at the absolute difference between the data and the model’s predictions. 
Because we use the absolute value of the residual, the MAE does not indicate underperformance or overperformance of the model 
(whether or not the model under or overshoots actual data). 
Each residual contributes proportionally to the total amount of error, meaning that larger errors will contribute linearly to the overall error. 
Like we’ve said above, a small MAE suggests the model is great at prediction, while **a large MAE suggests that your model may have trouble in certain areas. 
A MAE of 0 means that your model is a perfect predictor of the outputs (but this will almost never happen).

While the MAE is easily interpretable, using the absolute value of the residual often is not as desirable as squaring this difference. 
Depending on how you want your model to treat outliers, or extreme values, in your data, you may want to bring more attention to these outliers or downplay them. 
The issue of outliers can play a major role in which error metric you use.

Mean square error
The mean square error (MSE) is just like the MAE, but squares the difference before summing them all instead of using the absolute value. 

https://www.dataquest.io/blog/understanding-regression-error-metrics/

Both MAE and RMSE are negatively-oriented scores, which means lower values are better.

RMSE has the benefit of penalizing large errors more so can be more appropriate in some cases, for example, if being off by 10 is more than twice as bad as being off by 5. But if being off by 10 is just twice as bad as being off by 5, then MAE is more appropriate.
From an interpretation standpoint, MAE is clearly the winner. RMSE does not describe average error alone and has other implications that are more difficult to tease out and understand.
On the other hand, one distinct advantage of RMSE over MAE is that RMSE avoids the use of taking the absolute value, which is undesirable in many mathematical calculations (not discussed in this article, another time…).

https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d

https://towardsdatascience.com/how-to-select-the-right-evaluation-metric-for-machine-learning-models-part-1-regrression-metrics-3606e25beae0

