
# ++++++++++++++++++++++
# Sonia's Chi^2 Request
# ++++++++++++++++++++++

# Setup ----
dat = read.csv("/Users/hasansulaeman/Desktop/Book1.csv", header=TRUE) 
dat$p.value = 0
temp = data.frame(matrix(nrow = 2, ncol = 2))
# Clean up the NA values before starting the function
dat = na.omit(dat)

# The function ----
for (i in 1:nrow(dat)) {
  temp[1,1] = dat[i,2]
  temp[2,1] = dat[i,3]
  temp[1,2] = dat[i,4]
  temp[2,2] = dat[i,5]
  dat[i,]$p.value = chisq.test(temp)$p.value
}
