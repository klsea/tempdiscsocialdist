td_wide_to_long <- function(data) {
  # converts td from wide to long format
  # data is a data frame with all of your data
  d1 <- gather(data[c(1, 6:7, grep('SC3', colnames(data)):grep('SC5', colnames(data)))], 
               domain, propChoice, SC3:SC5)
  d1$Age <- d1$Q5; d1$Q5 <- NULL
  
  d1$domain[which(d1$domain == 'SC3')] <- 'Money'
  d1$domain[which(d1$domain == 'SC4')] <- 'Health'
  d1$domain[which(d1$domain == 'SC5')] <- 'Social'
  
  d1$domain <- factor(d1$domain, levels = c('Money', 'Health', 'Social'))
  
  d1$Race <- factor(d1$Q6); d1$Q6 <- NULL
  levels(d1$Race)[levels(d1$Race) == 1] <- 'White/Caucasian'
  levels(d1$Race)[levels(d1$Race) == 2] <- 'Black/African American'
  levels(d1$Race)[levels(d1$Race) == 4] <- 'Hispanic/Latinx'
  
  d1$Race <- factor(d1$Race, levels = c('White/Caucasian', 'Black/African American', 'Hispanic/Latinx'))
  
  return(d1)
}

