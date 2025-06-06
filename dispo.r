library(disdat)

groups<-disPo("CAN")

# don't really understand why all these values are the same. they all seem to be in canada
# no matter what region I assign it to
groups_x<-groups["x"]
groups_y<-groups["y"]

x<-c(groups_x)
y<-c(groups_y)
