library(tercen)
library(dplyr)

options("tercen.workflowId" = "0add2df8c4543198d0b9ab7b55003e76")
options("tercen.stepId"     = "7d8db72d-ae03-4eb2-8230-ef97acf227ef")

getOption("tercen.workflowId")
getOption("tercen.stepId")

do.glm <- function(df, N, levels) {
  response <- cbind(df$.y, N - df$.y)
  variable <- df$variable
  nms <- unique(variable)
  
  cnam <- c(paste0("coef_", levels), paste0("pval_", nms), "pval_full")
  df_out <- rep(NA, length(cnam))
  names(df_out) <- cnam
  df_out <- as.data.frame(t(df_out))

  if(length(nms) == 1) return(df_out)

  m <- try(glm(response ~ variable - 1, family = "binomial"))
  if(class(m) == "try-error") return(df_out)
  
  null <- glm(response ~ 1, family = "binomial") # null model
  df_out["pval_full"] <- with(anova(null, m), pchisq(Deviance,Df,lower.tail=FALSE)[2])
  
  mod <- summary(m)$coefficients
  estimate <- mod[, 1]
  names(estimate) <- paste0("coef_", nms)
  df_out[names(estimate)] <- estimate
  
  pvalue <- mod[, 4]
  names(pvalue) <- paste0("pval_", nms)
  df_out[names(pvalue)] <- pvalue

  return(df_out)
}

ctx <- tercenCtx()

# get input par: size
N <- 2
if(!is.null(ctx$op.value('N'))) N <- as.numeric(ctx$op.value('N'))

variable <- ctx$select(ctx$colors[[1]])[[1]]
levels <- unique(variable)

ctx %>% 
  select(.y, .ci, .ri) %>%
  mutate(variable = variable) %>%
  group_by(.ci, .ri) %>%
  do(do.glm(., N, levels)) %>%
  ctx$addNamespace() %>%
  ctx$save()
