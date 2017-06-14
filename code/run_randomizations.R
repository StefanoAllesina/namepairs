library(data.table) # for counting pairs
library(tidyverse) # for data wrangling and visualization

# count isonymous pairs in each department for each region and scientific field
count_pairs_dt <- function(dt){
  dt <- dt[, .(np = .N), by = .(last_id, idd, region, sector)]
  dt <- dt[,.(idd, region, sector, npairs = np * (np - 1) / 2)]
  dt <- dt[,.(npairs = as.integer(sum(npairs))), by = c("idd", "region", "sector")]
  return(dt)
}
count_pairs_region_sector <- function(dt){
  by_dept <- count_pairs_dt(dt)
  by_sector <- by_dept[order(sector), .(totpairs = as.integer(sum(npairs))), by = sector]
  by_region <- by_dept[order(region), .(totpairs =  as.integer(sum(npairs))), by = region]
  return(list(field = by_sector, region = by_region))
}

# main function, returns a list of results by region and by field
randomize_compute_pval <- function(data_file = "../data/ita_2000.csv",
                                   randomization = "nation", # either "nation", "city", or  "field"
                                   nrand = 500){
  # fix sample function so that it can deal with data of length 1
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  # read the data and select the relevant columns
  a <- read_csv(data_file) %>% select(last_id, sector, city_id, region, institution_id)
  # set unique names to -1 so that they can be excluded from the counting
  z <- table(a$last_id)
  tozero <- as.integer(names(z[z == 1]))
  a$last_id[a$last_id %in% tozero]  <- -1
  # add department -> combination of sector and institution
  a$idd <- as.integer(interaction(a$institution_id, a$sector))
  # decide the type of randomization
  if (randomization == "nation") a$randomize <- "1"
  if (randomization == "city") a$randomize <- as.character(factor(a$city_id))
  if (randomization == "field") a$randomize <- as.character(factor(a$sector))
  # transform last_id to numeric otherwise data.table complains
  a$last_id <- as.numeric(a$last_id)
  # build the data table
  dt <- data.table(a)
  # data struture to store observed and expected, along with standard deviation and p-value
  expected <- count_pairs_region_sector(dt[last_id > -1,])
  expected$field$mean <- 0
  expected$field$sd <- 0
  expected$field$pvalue <- 0
  expected$region$mean <- 0
  expected$region$sd <- 0
  expected$region$pvalue <- 0
  # now randomize
  for(i in 1:nrand){
    if (i %% 1000 == 0) print(i)
    dt[, last_id := resample(last_id), by = randomize]
    randomized <- count_pairs_region_sector(dt[last_id > -1,])
    
    expected$field$mean <- expected$field$mean + randomized$field$totpairs
    expected$field$sd <- expected$field$sd + randomized$field$totpairs^2
    expected$field$pvalue <- expected$field$pvalue + 1 * (randomized$field$totpairs >= expected$field$totpairs)
    
    expected$region$mean <- expected$region$mean + randomized$region$totpairs
    expected$region$sd <- expected$region$sd + randomized$region$totpairs^2
    expected$region$pvalue <- expected$region$pvalue + 1 * (randomized$region$totpairs >= expected$region$totpairs)
  }
  # normalize
  expected$field$mean <- expected$field$mean / nrand
  expected$field$sd <- expected$field$sd / nrand
  expected$field$sd <- sqrt(expected$field$sd - expected$field$mean^2)
  expected$field$pvalue <- expected$field$pvalue / nrand
  
  expected$region$mean <- expected$region$mean / nrand
  expected$region$sd <- expected$region$sd / nrand
  expected$region$sd <- sqrt(expected$region$sd - expected$region$mean^2)
  expected$region$pvalue <- expected$region$pvalue / nrand
  
  return(expected)
}

visualize_results <- function(randomization_results){
  field_results <- randomization_results$field
  region_results <- randomization_results$region
  field_results <- field_results %>% mutate(by = "field") %>% rename(x = sector)
  region_results <- region_results %>% mutate(by = "region") %>% rename(x = region)
  # Bonferroni correction: is the observed number of pairs significantly higher
  # than expected by chance?
  field_results <- field_results %>% mutate(significant = pvalue < 0.05 / nrow(field_results))
  region_results <- region_results %>% mutate(significant = pvalue < 0.05 / nrow(region_results))
  res <- rbind(field_results, region_results)
  pl <- ggplot(data = res, aes(x = x, 
                               y = totpairs / mean, 
                               alpha = significant, 
                               fill = by, colour = by)) + 
    geom_col(position = "identity") + 
    geom_hline(yintercept = 1, linetype = 2) + 
    facet_wrap(~by, ncol = 1, scales = "free") + 
    theme_bw() + theme(legend.position = "none")  + 
    scale_y_continuous("Observed/Expected") + 
    scale_x_discrete("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(pl)
}
