module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair

point::(Double, Double) -> Point
point (xx, yy) = Point{x=xx, y=yy}

-- The origin
origin::Point
origin = Point{x=0, y=0}

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (x0, y0) = Rectangle origin (point (x0, y0))

base::Rectangle -> Double
base (Rectangle (Point first _) (Point second _)) = second - first

height::Rectangle -> Double
height (Rectangle (Point _ first) (Point _ second)) = second - first

-- Circle from radius
circle::Double -> Circle
circle r = Circle origin r

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift  (Point { x = xCord, y = yCord }) (x1, x2)= Point { x = xCord + x1, y = yCord + x2 }
   
instance Shift Rectangle where
   shift  (Rectangle (Point x0 y0) (Point x1 y1)) (x2, y2) = Rectangle (Point (x0 + x2) (y0 + y2)) (Point (x1 + x2) (y1 + y2))
instance Shift Circle where
   shift  (Circle (Point {x=x0, y=y0}) n) (x1, y1) = (Circle (Point {x=(x0 + x1), y=( y0 + y1)}) n)

-- Define the Surface class
   
class Surface a where
   surface::a -> Double

instance Surface Rectangle where
    surface rec = (base rec) * (height rec)
instance Surface Circle where
    surface (Circle _ r) = pi * r * r


