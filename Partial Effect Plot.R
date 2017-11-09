
# -------------------------
# Plotting Partial Effects
# -------------------------

# What are partial effects, and why would we want to plot them?
# The relationship observed when plotting the outcome variable
# against some explanatory variable may be obscured by the effects
# of other explanatory variables not observed in the plot.

# A partial effects plot

# Say we want to plot the partial effect of some variable.
# First, let's consider the following model, where we regress.
# mpg on hp, wt, and cyl in the mtcars dataset.
lm1 <- lm(mpg ~ hp + wt + cyl, data = mtcars)

# If we want to plot the partial effect of, let's say, hp on mpg,
# we need to (1) regress mpg on all covariates except hp:

lm2 <- lm(mpg ~ wt + cyl, data = mtcars)

# and (2), regress hp on the other variables included in the right-
# hand side of the model:

lm3 <- lm(hp ~ wt + cyl, data = mtcars)

# We then plot the residuals from lm2 against the residuals from
# lm3, along with confidence intervals.

x <- resid(lm3)
y <- resid(lm2)
windows()
plot(x,y,type="n")
part <- lm(y~x)
wx = par("usr")[1:2]
new.x = seq(wx[1],wx[2],len=100)
pred = predict(part, new=data.frame(x=new.x), interval="conf")
lines(new.x,pred[,"fit"],lwd=2)
lines(new.x,pred[,"lwr"],lty=3)
lines(new.x,pred[,"upr"],lty=3)
points(x,y,pch=16,col="steelblue")

# ------------------------------------
# Partial Effect Plot function (peplot)
# ------------------------------------
peplot <- function(mod,var,ci=.95, plot_points = "n",
                   xlab=var,ylab=names(mod[12]$model)[1],
                   main="Partial Effect Plot",
                   pe_lty=1,pe_lwd=3,pe_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   pch_col="black",pch_ty=19,
                   ylim=c(min(pred[,"lwr"]),max(pred[,"upr"]))){
  modDat <- mod[12]$model
  modDat1 <- modDat[,-1]
  modDat2 <- modDat[,which(names(modDat)!=var)]
  x <- resid(lm(modDat1[,var] ~., data=modDat1[,which(names(modDat1)!=var)]))
  y <- resid(lm(modDat2[,1] ~ ., modDat2[,-1]))
  plot(x,y,type=plot_points,xlab=xlab,ylab=ylab,
       ylim=ylim,col=pch_col,pch=pch_ty,
       main=main)
  part <- lm(y~x)
  wx <- par("usr")[1:2]
  new.x <- seq(wx[1],wx[2],len=100)
  pred <- predict(part, new=data.frame(x=new.x), interval="conf",
                  level = ci)
  lines(new.x,pred[,"fit"],lwd=pe_lwd,lty=pe_lty,col=pe_col)
  lines(new.x,pred[,"lwr"],lwd=ci_lwd,lty=ci_lty,col=ci_col)
  lines(new.x,pred[,"upr"],lwd=ci_lwd,lty=ci_lty,col=ci_col)
}
windows()
peplot(lm1,var="hp")