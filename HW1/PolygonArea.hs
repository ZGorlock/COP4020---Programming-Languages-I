module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea []         = error "list is empty"
computeArea [_]        = error "one point is not a polygon"
computeArea [_,_]      = error "two points are not a polygon"
computeArea (x:xs) = (1 / 2) * computeAreaHelper ([x] ++ xs ++ [x])

computeAreaHelper :: [(Double,Double)] -> Double
computeAreaHelper []                   = 0
computeAreaHelper [_]                  = 0
computeAreaHelper ((x1,y1):(x2,y2):xs) =
    det (x1,y1) (x2,y2) + computeAreaHelper ((x2,y2):xs)

det :: (Double, Double) -> (Double, Double) -> Double
det (a,c) (b,d) = (a * d) - (c * b)