inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)


aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right True) = 1
aEntero (Right False) = 0