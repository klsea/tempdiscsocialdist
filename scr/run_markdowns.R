# Run markdown files for both samples in  tempdiscsocialdist data set
# 7.2.20 KLS 

library(here)

for (sample in 1:2) {
  # rmarkdown::render(here::here('doc', '00_Demo_data.Rmd'),
  #                   output_file = paste0('00_Demo_data_S', sample, '.html'),
  #                   output_dir = here::here('doc'))
  # rmarkdown::render(here::here('doc', '01_temp_disc.Rmd'),
  #                   output_file = paste0('01_temp_disc_S', sample, '.html'),
  #                   output_dir = here::here('doc'))
  rmarkdown::render(here::here('doc', '02_social_dist.Rmd'),
                    output_file = paste0('02_social_dist_S', sample, '.html'),
                    output_dir = here::here('doc'))
}

