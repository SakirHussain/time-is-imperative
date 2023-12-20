r :: Double
r = 5.0
ar :: Double
ar = pi * r^2

--notice order need not be maintianed in haskell due to 
--immutability
y :: Integer
y = x*2

x :: Integer
x = 10

area :: Floating a => a -> a
area r = pi * r^2

double :: Num a => a -> a
double x = x*2

quad :: Num a => a -> a
quad x = double(double x)

sqr :: Num a => a -> a
sqr x = x^2

half :: Fractional a => a -> a
half x = x/2

--Define a function that subtracts 12 from half its argument.
subtwl :: Fractional a => a -> a
subtwl a = (a/2) - 12

-- Write a function to calculate the volume of a cylinder. 
-- The volume of a cylinder is the area of the base, 
-- which is a circle multiplied by the height.

areacir :: Floating a => a -> a
areacir r = pi * (r^2)
areacyl :: Floating a => a -> a -> a
areacyl r h = areacir r * h

heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where
    s = (a + b + c) / 2

areaTriangleTrig :: Floating a => a -> a -> a -> a
areaTriangleTrig  a b c = c * height / 2   -- use trigonometry
    where
    cosa   = (b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c)
    sina   = sqrt (1 - cosa ^ 2)
    height = b * sina