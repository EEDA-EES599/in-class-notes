# area of circle over area of square is pi / 4
# if we could compute that ratio, we could multiply it by 4 to get the value of pi

# we'll run this 100000 times
N = 100000

# the side of the square will be [-1, 1]
R = 1
# generate N uniform x coordinates within the square
x = runif(N, min= -R, max= R)

#generate N uniform y coordinates within the square
y = runif(N, min= -R, max= R)

# the point will be inside the circle
# find this out by working out area of cirle over area of square
# where the former is the number of points that satisfy x^2 + y^2 <= 1
is.inside = (x^2 + y^2) <= R^2
pi.estimate = 4 * sum(is.inside) / N
pi.estimate

plot.new()
plot.window(xlim = 1.1 * R * c(-1, 1), ylim = 1.1 * R * c(-1, 1))
points(x[ is.inside], y[ is.inside], pch = '.', col = "blue")
points(x[!is.inside], y[!is.inside], pch = '.', col = "red")