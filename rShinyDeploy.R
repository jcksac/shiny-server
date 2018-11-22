install.packages("rsconnect")
install.packages("shiny")
library(rsconnect)

rsconnect::setAccountInfo(name='jacksonsac', token='FB57003B98D40B524CC622592F2A3300', secret='UtnpWtLPCwnfqfxU6c4a2gCItwavuBUYmloUcHyB')
			 
			 
rsconnect::deployApp('/Users/richardjackson/Dropbox/Jackson SAC/Projects/R Shiny/Jeremy')
rsconnect::deployApp('/Users/richardjackson/Dropbox/Jackson SAC/Projects/R Shiny/BenSV')

