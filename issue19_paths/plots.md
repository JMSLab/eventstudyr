
# Smoothest paths for different settings of EventStudy

We construct event studies using different numbers of leads and lags. We
solve for the smoothest path in each case.

## Summary data

A summary csv can be found at [`data_paths.csv`](data_paths.csv). Recall
that the order of the computed polynomial equals `order`+1.

- The solver converges in all 72 cases.
- A computation is successful if the absolute difference between the
  Wald critical value and the Wald optimal value is less than 0.1. There
  were 72 successes.

Order found and number of successes, by outcome:

| yvar       | order | Num_found | Num_success |
|:-----------|------:|----------:|------------:|
| y_smooth_m |     1 |         7 |           7 |
| y_smooth_m |     2 |        18 |          18 |
| y_smooth_m |     3 |        10 |          10 |
| y_smooth_m |     4 |         1 |           1 |
| y_jump_m   |     4 |         3 |           3 |
| y_jump_m   |     5 |        10 |          10 |
| y_jump_m   |     6 |        16 |          16 |
| y_jump_m   |     7 |         6 |           6 |
| y_jump_m   |     8 |         1 |           1 |

## Smooth outcome

### Post=2

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post2_pre2.png) | ![](R/y_smooth_m_post2_pre3.png) | ![](R/y_smooth_m_post2_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post2_pre5.png) | ![](R/y_smooth_m_post2_pre6.png) | ![](R/y_smooth_m_post2_pre7.png) |

### Post=3

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post3_pre2.png) | ![](R/y_smooth_m_post3_pre3.png) | ![](R/y_smooth_m_post3_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post3_pre5.png) | ![](R/y_smooth_m_post3_pre6.png) | ![](R/y_smooth_m_post3_pre7.png) |

### Post=4

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post4_pre2.png) | ![](R/y_smooth_m_post4_pre3.png) | ![](R/y_smooth_m_post4_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post4_pre5.png) | ![](R/y_smooth_m_post4_pre6.png) | ![](R/y_smooth_m_post4_pre7.png) |

### Post=5

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post5_pre2.png) | ![](R/y_smooth_m_post5_pre3.png) | ![](R/y_smooth_m_post5_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post5_pre5.png) | ![](R/y_smooth_m_post5_pre6.png) | ![](R/y_smooth_m_post5_pre7.png) |

### Post=6

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post6_pre2.png) | ![](R/y_smooth_m_post6_pre3.png) | ![](R/y_smooth_m_post6_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post6_pre5.png) | ![](R/y_smooth_m_post6_pre6.png) | ![](R/y_smooth_m_post6_pre7.png) |

### Post=7

| Pre=2                            | Pre=3                            | Pre=4                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post7_pre2.png) | ![](R/y_smooth_m_post7_pre3.png) | ![](R/y_smooth_m_post7_pre4.png) |

| Pre=5                            | Pre=6                            | Pre=7                            |
|----------------------------------|----------------------------------|----------------------------------|
| ![](R/y_smooth_m_post7_pre5.png) | ![](R/y_smooth_m_post7_pre6.png) | ![](R/y_smooth_m_post7_pre7.png) |

## Jump outcome

### Post=2

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post2_pre2.png) | ![](R/y_jump_m_post2_pre3.png) | ![](R/y_jump_m_post2_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post2_pre5.png) | ![](R/y_jump_m_post2_pre6.png) | ![](R/y_jump_m_post2_pre7.png) |

### Post=3

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post3_pre2.png) | ![](R/y_jump_m_post3_pre3.png) | ![](R/y_jump_m_post3_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post3_pre5.png) | ![](R/y_jump_m_post3_pre6.png) | ![](R/y_jump_m_post3_pre7.png) |

### Post=4

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post4_pre2.png) | ![](R/y_jump_m_post4_pre3.png) | ![](R/y_jump_m_post4_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post4_pre5.png) | ![](R/y_jump_m_post4_pre6.png) | ![](R/y_jump_m_post4_pre7.png) |

### Post=5

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post5_pre2.png) | ![](R/y_jump_m_post5_pre3.png) | ![](R/y_jump_m_post5_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post5_pre5.png) | ![](R/y_jump_m_post5_pre6.png) | ![](R/y_jump_m_post5_pre7.png) |

### Post=6

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post6_pre2.png) | ![](R/y_jump_m_post6_pre3.png) | ![](R/y_jump_m_post6_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post6_pre5.png) | ![](R/y_jump_m_post6_pre6.png) | ![](R/y_jump_m_post6_pre7.png) |

### Post=7

| Pre=2                          | Pre=3                          | Pre=4                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post7_pre2.png) | ![](R/y_jump_m_post7_pre3.png) | ![](R/y_jump_m_post7_pre4.png) |

| Pre=5                          | Pre=6                          | Pre=7                          |
|--------------------------------|--------------------------------|--------------------------------|
| ![](R/y_jump_m_post7_pre5.png) | ![](R/y_jump_m_post7_pre6.png) | ![](R/y_jump_m_post7_pre7.png) |
