library(tercen)
library(dplyr)
library(lme4)
library(broom)
library(broom.mixed)

ctx <- tercenCtx()

# get input par: size
N <- ctx$op.value('N', as.numeric, 1)
model.family <- ctx$op.value('model.family', as.character, "binomial") # "gaussian", "binomial", "negative.binomial"

# get model formula
random_effects_intercept <- unlist(lapply(
  ctx$labels, function(x) paste0("(1 | `", x, "`)")
))
random_effects_intercept <- paste0(random_effects_intercept, collapse = " + ")
if(random_effects_intercept == "") random_effects_intercept <- NA

fixed_effects <- paste0(paste0("`", unlist(ctx$cnames), "`"), collapse = " + ")

random_effects_slope <- unlist(lapply(
  ctx$colors, function(x) paste0("(", paste0(fixed_effects, collapse = " + ")," | `", x, "`)")
))
random_effects_slope <- paste0(random_effects_slope, collapse = " + ")
if(random_effects_slope == "") random_effects_slope <- NA

eff <- c(fixed_effects, random_effects_intercept, random_effects_slope)
eff <- eff[!is.na(eff)]
effects <- paste(eff, collapse = " + ")

df <- ctx$select(c(ctx$colors, ctx$labels, ".y", ".ri", ".ci"))

if(model.family == "binomial") {
  rng <- range(df$.y, na.rm = TRUE)
  if(rng[2] > 1 | rng[1] < 0) stop("y values should be between 0 and 1.")
}

if(ctx$hasXAxis) {
  df$n <- ctx$select(".x")[[1]]
  df$N <- df$n / df$.y
} else {
  df$N <- N
  df$n <- df$.y
}

df <- df %>% bind_cols(lapply(ctx$cselect(), function(x) {
  x[df$.ci + 1]
}))

if(model.family == "negative.binomial") {
  model_formula <- paste0("df$n ~ ", effects)
} else {
  model_formula <- paste0("cbind(df$n, df$N - df$n) ~ ", effects)
}

do.glmer <- function(df) {
  if(model.family == "negative.binomial") {
    m <- glmer.nb(as.formula(model_formula), data = df)
  } else {
    m <- glmer(as.formula(model_formula), family = "binomial", data = df)
  }
  
  df_out <- broom.mixed::tidy(m) %>%
    filter(effect == "fixed" & term != "(Intercept)") %>%
    select(term, estimate, std.error, statistic, p.value)
  
  return(df_out)
}

df_out <- df %>% group_by(.ri) %>%
  do(do.glmer(.)) %>% 
  mutate(neglog_pvalue = -log10(p.value)) %>%
  ctx$addNamespace() %>%
  ctx$save()
