library(rsconnect)

setwd("//share.univie.ac.at/maripoldata/5_Research/WP1/Collected Data/3_working data/shiny_literature")


# rsconnect::setAccountInfo(name='e3vxkr-arnelanglet',
#                           token='844EEF0ED7E9406AC05CB482FA55969A',
#                           secret='y6y0y07vdQErhDVAQUPpOjHWg8dOsA3+48/gO+1w')
# 
# rsconnect::deployApp()

rsconnect::setAccountInfo(name='erc-maripoldata',
                          token='D090979DF213A0A8BDCE03DEB05B49C2',
                          secret='yeQ7/SJGn/K9XnKEyO+PYK0dEvGzueLGCDsLC/2J')

rsconnect::deployApp(
  appName = "literature_dashboard", 
  account = "erc-maripoldata")

