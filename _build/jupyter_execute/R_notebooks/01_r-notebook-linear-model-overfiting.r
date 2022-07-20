set.seed(123)
n = 1000

p = n
X<- matrix(rnorm(n*p), n, p)
Y<- rnorm(n)

print("p/n is")
print(p/n)
print("R2 is")
print(summary(lm(Y~X))$r.squared)
print("Adjusted R2 is")
print(summary(lm(Y~X))$adj.r.squared)


print(summary(lm(Y~X)))


set.seed(123)
n = 1000

p = n/2
X<- matrix(rnorm(n*p), n, p)
Y<- rnorm(n)

print("p/n is")
print(p/n)
print("R2 is")
print(summary(lm(Y~X))$r.squared)
print("Adjusted R2 is")
print(summary(lm(Y~X))$adj.r.squared)



set.seed(123)
n = 1000

p = .05*n
X<- matrix(rnorm(n*p), n, p)
Y<- rnorm(n)

print("p/n is")
print(p/n)
print("R2 is")
print(summary(lm(Y~X))$r.squared)
print("Adjusted R2 is")
print(summary(lm(Y~X))$adj.r.squared)






