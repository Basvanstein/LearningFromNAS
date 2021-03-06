[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4043005.svg)](https://doi.org/10.5281/zenodo.4043005)

# Neural Network Design: Learning from Neural Architecture Search

Code and results accompanying the Learning from Neural Architecture Search paper accepted at SSCI 2020.




# Apendix: ELA features


| Feature                          | MNIST                  | Fashion                 | CIFAR-10               |
|----------------------------------|------------------------|-------------------------|-------------------------|
| disp.diff_mean_02         | 0.0223 ± 0.0403   | 0.199 ± 0.0736     | 0.0499 ± 0.0459    |
| disp.diff_mean_05         | 0.0223 ± 0.0403   | 0.199 ± 0.0736     | 0.0499 ± 0.0459    |
| disp.ratio_mean_02        | 1 ± 0.00209        | 1.01 ± 0.00381     | 1 ± 0.00238         |
| disp.ratio_mean_05        | 1 ± 0.00209        | 1.01 ± 0.00381     | 1 ± 0.00238         |
| distr.kurtosis                  | \-1.87 ± 0.00648  | \-0.718 ± 0.0843   | 4.9 ± 0.258        |
| distr.skewness                  | \-0.0862 ± 0.0289 | \-0.87 ± 0.0396    | 2.24 ± 0.0404      |
| ic.eps.max                     | 0.00323 ± 0.00127 | 0.00462 ± 0.000634 | 0.00247 ± 0.000431 |
| ic.eps.ratio                   | \-1.36 ± 0.021    | \-1.73 ± 0.0282    | \-2.19 ± 0.0301    |
| ic.eps.s                       | \-1.07 ± 0.00981  | \-1.18 ± 0.0134    | \-1.31 ± 0.0104    |
| ic.h.max                       | 0.823 ± 0.0101    | 0.825 ± 0.00952    | 0.768 ± 0.0105     |
| ic.m0                           | 0.541 ± 0.0135    | 0.599 ± 0.014      | 0.558 ± 0.00953    |
| lin_simple.adj_r2         | 0.436 ± 0.00781   | 0.383 ± 0.0118     | 0.472 ± 0.0117     |
| lin_simple.intercept         | 0.581 ± 0.00636   | 0.617 ± 0.00569    | 0.199 ± 0.00276    |
| lin_w_interact.adj_r2  | 0.327 ± 0.0202    | 0.314 ± 0.0216     | 0.566 ± 0.031      |
| nbc.dist_ratio.coeff_var | 0.132 ± 0.00224   | 0.1 ± 0.00139      | 0.124 ± 0.00206    |
| nbc.nb_fitness.cor          | \-0.43 ± 0.0119   | \-0.461 ± 0.0108   | \-0.306 ± 0.00833  |
| nbc.nn_nb.cor               | 0.411 ± 0.0213    | 0.589 ± 0.0138     | 0.468 ± 0.0145     |
| nbc.nn_nb.mean_ratio     | 0.874 ± 0.00215   | 0.916 ± 0.00104    | 0.889 ± 0.00195    |
| nbc.nn_nb.sd_ratio       | 0.539 ± 0.0126    | 0.626 ± 0.0117     | 0.551 ± 0.0124     |
| quad_simple.adj_r2        | 0.486 ± 0.00733   | 0.497 ± 0.0112     | 0.488 ± 0.012      |

**Table: ELA features for MNIST, CIFAR-10 and Fashion NAS problem instances.**
