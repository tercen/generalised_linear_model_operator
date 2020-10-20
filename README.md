# Logistic regression operator

##### Description

The `logistic regression operator` performs a logistic regression on a set of data points.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, response variable (must be a count)
`column`        | character, observations
`colors`        | numeric/character, explanatory variable

Input parameters|.
---|---
`N`        | size (binomial parameter; default is 1)

Output relations|.
---|---
`pval_GROUP`        | numeric, p-value of the effect of a given category, per cell
`coef_GROUP`        | numeric, estimate of the effect of a given category, per cell
`pval_full`        | numeric, p-value of the model versus a null model

##### Details

Details on the computation.

##### See Also

[template_shiny_operator](https://github.com/tercen/template_shiny_operator)
, [template_docker_operator](https://github.com/tercen/template_docker_operator)

