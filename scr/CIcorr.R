CIcorr <- function(alpha, corr, s, n) {
        # Computes a confidence interval for a (partial) Pearson correlation
        # Arguments: 
        #   alpha: alpha value for 1-alpha confidence
        #   corr:  sample value of (partial) correlation 
        #   s:     number of control variables
        #   n:     sample size
        # Returns:
        #   confidence interval
        z <- qnorm(1 - alpha/2)
        se <- sqrt(1/((n - s - 3)))
        zr <- log((1 + corr)/(1 - corr))/2
        LL0 <- zr - z*se
        UL0 <- zr + z*se
        LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1)
        UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1)
        CI <- c(LL, UL)
        return(CI)
}
