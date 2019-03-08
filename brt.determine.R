brt.determineR <- function(train, test, predictors, observation, learningrate, treecomplexity) {
foreach(j = 1:length(treecomplexity), .packages = c("gbm", "dismo", "foreach"), .combine = "rbind") %dopar% {
  alt <- foreach(i = 1:length(learningrate), .packages = c("gbm", "dismo", "foreach")) %dopar% {
    BRT <- tryCatch(gbm.step(data = train, gbm.x = predictors, gbm.y = observation, family = "gaussian", tree.complexity = treecomplexity[j], learning.rate = learningrate[i], bag.fraction = 0.5), error = function(e) NA, verbose = F)}
  if(sum(sapply(alt, is.null)) > 0) {
    if(sum(sapply(alt, is.null)) == 5) {
      TREES <- rep(NA, 5)
      DEVI <- rep(NA, 5)
    } else {
      TREES <- rep(NA, 5)
      DEVI <- rep(NA, 5)
      NUMS <- which(!sapply(alt, is.null))
      TREES[NUMS] <- sapply(NUMS, function(k) alt[[k]]$gbm.call$best.trees)
      PREDICT <- lapply(NUMS, function(m) predict.gbm(alt[[m]], test, n.trees = alt[[m]]$gbm.call$best.trees, type = "response"))
      DEVI[NUMS] <- sapply(PREDICT, function(n) calc.deviance(test[, observation], pred = n, calc.mean = T, family = "gaussian"))
    } } else {
      TREES <- sapply(1:5, function(k) alt[[k]]$gbm.call$best.trees)
      PREDICT <- lapply(1:length(alt), function(m) predict.gbm(alt[[m]], test, n.trees = alt[[m]]$gbm.call$best.trees, type = "response"))
      DEVI <- sapply(PREDICT, function(n) calc.deviance(test[, observation], pred = n, calc.mean = T, family = "gaussian")) }
  cbind.data.frame(tc = treecomplexity[j], lr = learningrate, devi = DEVI, trees = TREES)
}
}