install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='highspeedtwo', token='9D1BD61FCE84F072071C7274A5D7CFE1', secret='PC5eAB52wTtRqokRIgDxWrWj3xP5PBzf5AZVyhMb')

rsconnect::deployApp("~/Desktop/pollution_app")
