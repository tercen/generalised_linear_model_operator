# Logistic regression operator

##### Description

The `logistic regression operator` performs a logistic regression on a set of data points.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, response variable (must be a count)
`column`        | numeric/character, fixed effects
`colors`        | numeric/character, random effects on the slopes
`labels`        | numeric/character, random effects on the intercept

Input parameters|.
---|---
`N`        | size (binomial parameter, default is 1)

Output relations|.
---|---
`term`        | numeric, term / category, per row
`estimate`        | numeric, estimate of the effect of a given category, per row
`std.error`        | numeric, standard error of the effect of a given category, per row
`statistic`        | numeric, p-value of the effect of a given category, per row
`p.value`        | numeric, p-value, per row
`neglog_pvalue`        | numeric, -log10(p-value), per row
term, estimate, std.error, statistic, p.value
##### Details

The operator uses the `glmer()` function from the `lme4` R package.

##### See Also

[lm_operator](https://github.com/tercen/lm_operator)

