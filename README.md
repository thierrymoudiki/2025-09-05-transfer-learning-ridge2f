
# Transfer learning using `ahead::ridge2f` on synthetic stock returns

I pretrain [`ahead::ridge2f`](https://www.mdpi.com/2227-9091/6/1/22), a doubly-constrained Random Vector Functional Link (RVFL) Network on [1000 synthetic stock returns](https://raw.githubusercontent.com/Techtonique/datasets/refs/heads/main/time_series/multivariate/synthetic_stock_returns.csv) using Bayesian Optimization, and test its performance on real market data.

In order to reproduce the results (find model hyperparameters after pretraining, and reproduce the forecasting results), either: 

Run `0-2025-09-07-transfer-learning-stock-returns.Rmd` 

or

Execute the `.R` files starting with a number, in the order in which they appear. 

Results on 4 major European indices: 

```R
[1] "\n=== MEDIAN PERFORMANCE ACROSS ALL SERIES ==="
   Method    Winkler Coverage Interval_Width
1  fgarch 0.05925044     93.5     0.04735842
2  ridge2 0.06024835     94.5     0.04753165
3 rugarch 0.05919827     93.5     0.04753477
```

Results on 10 CAC stocks: 

```R
  Method    Winkler Coverage Interval_Width
1  fgarch 0.09799624 96.42857     0.06746349
2  ridge2 0.09573495 95.71429     0.07500853
3 rugarch 0.09797592 97.14286     0.06758644
```

**level = 80**

```R
   Method    Winkler Coverage Interval_Width
1  fgarch 0.05719710 83.57143     0.03511276
2  ridge2 0.05768439 82.85714     0.04028393
3 rugarch 0.05711513 83.57143     0.03517754

> # Statistical significance testing
> cat("\n=== STATISTICAL COMPARISON ===\n")

=== STATISTICAL COMPARISON ===

> methods <- unique(summary_table$Method)

> for (metric in c("Winkler", "Coverage")) {
+   cat(paste("\n", metric, "comparison:\n"))
+   for (i in 1:(length(methods)-1)) {
+     for (j in (i+1 .... [TRUNCATED] 

 Winkler comparison:
ridge2 vs rugarch: p-value = 0.4019
ridge2 vs fgarch: p-value = 0.3452
rugarch vs fgarch: p-value = 0.3436

 Coverage comparison:
ridge2 vs rugarch: p-value = 0.5599
ridge2 vs fgarch: p-value = 0.7109
rugarch vs fgarch: p-value = 0.3434
```

Results on 29 SP500 stocks: 

```R
   Method    Winkler Coverage Interval_Width
1  fgarch 0.09113443 95.65217     0.07321220
2  ridge2 0.09370551 95.65217     0.07065134
3 rugarch 0.09176901 97.10145     0.07119994

 Winkler comparison:
ridge2 vs rugarch: p-value = 0.4392
ridge2 vs fgarch: p-value = 0.3204
rugarch vs fgarch: p-value = 0.6021

 Coverage comparison:
ridge2 vs rugarch: p-value = 0.0673
ridge2 vs fgarch: p-value = 0.0313 *
rugarch vs fgarch: p-value = 0.8390
```

**level = 80**

```R
   Method    Winkler Coverage Interval_Width
1  fgarch 0.06098937 79.71014     0.03871478
2  ridge2 0.06271718 79.71014     0.04084767
3 rugarch 0.06116365 79.71014     0.03765069

> # Statistical significance testing
> cat("\n=== STATISTICAL COMPARISON ===\n")

=== STATISTICAL COMPARISON ===

> methods <- unique(summary_table$Method)

> for (metric in c("Winkler", "Coverage")) {
+   cat(paste("\n", metric, "comparison:\n"))
+   for (i in 1:(length(methods)-1)) {
+     for (j in (i+1 .... [TRUNCATED] 

 Winkler comparison:
ridge2 vs rugarch: p-value = 0.7437
ridge2 vs fgarch: p-value = 0.6085
rugarch vs fgarch: p-value = 0.8138
```


More details about this model (actually used in an industrial setting):

- [https://thierrymoudiki.github.io/blog/2025/07/01/r/python/ridge2-bayesian](https://thierrymoudiki.github.io/blog/2025/07/01/r/python/ridge2-bayesian)
- [https://www.mdpi.com/2227-9091/6/1/22](https://www.mdpi.com/2227-9091/6/1/22)
- [https://thierrymoudiki.github.io/blog/2024/02/26/python/r/julia/ahead-v0100](https://thierrymoudiki.github.io/blog/2024/02/26/python/r/julia/ahead-v0100)
- [Doc for R](https://docs.techtonique.net/ahead/index.html)
- [Doc for Python](https://docs.techtonique.net/ahead_python/ahead.html#Ridge2Regressor)
