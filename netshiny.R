#netshiny.R

#devtools::install_github('rstudio/shinyapps')
library("shiny")
library("shinyapps")


shinyapps::setAccountInfo(name='bangarang', 
                          token='6F3FC1A905B80C49616C0B24DEC7E993', 
                          secret='YRbdkKXocHafJvIovrXpJrZAQV5a/NWiv3d5FYEY')

runApp()

deployApp(appName="nets")
#deployApp(appName="season2")
