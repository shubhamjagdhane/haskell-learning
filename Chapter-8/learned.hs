applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

{-
applyTimes 5 (+5) 50 => O/P 75
applyTimes 5 (+5) 50 = (+5) . applyTimes (5-1) (+5) $ 50
evaluation           = (+5) . applyTimes 4 (+5) 55

applyTimes 4 (+5) 55 = (+5) . applyTimes (4-1) (+5) $ 55
evaluation           = (+5) . applyTimes 3 (+5) 60

applyTimes 3 (+5) 60 = (+5) . applyTimes (3-1) (+5) $ 60
evaluation           = (+5) . applyTimes 2 (+5) 65

applyTimes 2 (+5) 65 = (+5) . applyTimes (2-1) (+5) $ 65
evaluation           = (+5) . applyTimes 1 (+5) 70

applyTimes 1 (+5) 70 = (+5) . applyTimes 0 (+5) $ 70
evaluation           = (+5) . 70

evaluation = 75
-}



f :: Bool -> Maybe Integer
f False = Just 0
f _ = Nothing
