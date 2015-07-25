f <- function() {}

f(1)

formals(f)
formals(f) <- alist(x=, y=3)
formals(f)

f(1)
f(1,2)
f(1,2,3)

body(f)
body(f) <- quote(x + y)
body(f)

f

f(1)
f(1,5)

## the following function prints in the same way as the created function above
gg <- function(x,y)
  x + y
