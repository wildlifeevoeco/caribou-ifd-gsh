library(segmented)

### 2009
x <- yravglocs2009$idrank #These are my ranked ID values 
y <- yravglocs2009$val #These are my nearest neighbour distances

lin.mod <- lm(y~x) #Plot linear model
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=14) #Run segmented function on it, psi=estimated breakpoint
summary(segmented.mod) #The summary will tell you the actual breakpoint

plot(x,y, pch=16) #If you want to plot the values and the breakpoint
plot(segmented.mod, add=T)

fit <- numeric(length(x)) * NA # This is required if you want to plot it using ggplot
fit[complete.cases(rowSums(cbind(y, x)))] <- broken.line(segmented.mod)$fit

yravglocs2009$fit <- fit
yravglocs2009$breakpoint <- 15.903

ggplot(yravglocs2009, aes(x = idrank, y = val)) + 
  geom_point() +
  geom_line(aes(x = idrank, y = fit, group = idrank < breakpoint), color = 'blue')