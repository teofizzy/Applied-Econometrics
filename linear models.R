##Calculating confidence intervals
mean.pred.intervals <- function(x, y, pred.x) {
    n <- length(y) # Find sample size
    lm.model <- lm(y ~ x) # Fit linear model
    y.fitted <- lm.model$fitted.values # Extract the fitted values of y
    
    # Coefficients of the linear model, beta0 and beta1
    b0 <- lm.model$coefficients[1]
    b1 <- lm.model$coefficients[2]
    
    pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)
    
    # Find SSE and MSE
    sse <- sum((y - y.fitted)^2)
    mse <- sse / (n - 2)
    
    t.val <- qt(0.975, n - 2) # Critical value of t
    
    mean.se.fit <- (1 / n + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the mean estimate
    pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the prediction
    
    # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
    mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
    mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)
    
    # Prediction Upper and Lower Confidence limits at 95% Confidence
    pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se.fit)
    pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se.fit)
    
    # Beta 1 Upper and Lower Confidence limits at 95% Confidence
    b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
    b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
    
    # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
    upper <- data.frame(rbind(round(mean.conf.upper, 2), round(pred.conf.upper, 2), round(b1.conf.upper, 2)))
    lower <- data.frame(rbind(round(mean.conf.lower, 2), round(pred.conf.lower, 2), round(b1.conf.lower, 2)))
    fit <- data.frame(rbind(round(pred.y, 2), round(pred.y, 2), round(b1, 2)))
    
    # Collect all into data.frame and rename columns
    results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction', 'Coefficient'))
    colnames(results) <- c('Lower', 'Upper', 'Fit')
    
    return(results)
}

mean.pred.intervals(cars$speed, cars$dist, new.dat)

##Confidence interval around a linear regression line
reg.conf.intervals <- function(x, y) {
    n <- length(y) # Find length of y to use as sample size
    lm.model <- lm(y ~ x) # Fit linear model
    
    # Extract fitted coefficients from model object
    b0 <- lm.model$coefficients[1]
    b1 <- lm.model$coefficients[2]
    
    # Find SSE and MSE
    sse <- sum((y - lm.model$fitted.values)^2)
    mse <- sse / (n - 2)
    
    t.val <- qt(0.975, n - 2) # Calculate critical t-value
    
    # Fit linear model with extracted coefficients
    x_new <- 1:max(x)
    y.fit <- b1 * x_new + b0
    
    # Find the standard error of the regression line
    se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    
    # Fit a new linear model that extends past the given data points (for plotting)
    x_new2 <- 1:max(x + 100)
    y.fit2 <- b1 * x_new2 + b0
    
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y.fit2 + t.val * se)
    slope.lower <- suppressWarnings(y.fit2 - t.val * se)
    
    # Collect the computed confidence bands into a data.frame and name the colums
    bands <- data.frame(cbind(slope.lower, slope.upper))
    colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
    
    # Plot the fitted linear regression line and the computed confidence bands
    plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
    lines(y.fit2, col = 'black', lwd = 2)
    lines(bands[1], col = 'blue', lty = 2, lwd = 2)
    lines(bands[2], col = 'blue', lty = 2, lwd = 2)
    
    return(bands)
}

conf.intervals <- reg.conf.intervals(cars$speed, cars$dist)