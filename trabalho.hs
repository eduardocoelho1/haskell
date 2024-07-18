pertence :: (Eq t) => t -> [t] -> Bool
pertence _ [] = False
pertence e (c : r)
  | e == c = True
  | otherwise = pertence e r
-- 6
removerRepetidos :: (Eq t) => [t] -> [t]
removerRepetidos [] = []
removerRepetidos (c : r)
  | pertence c r = removerRepetidos r
  | otherwise    = c : removerRepetidos r
-- 13
uniao :: (Eq t) => [t] -> [t] -> [t]
uniao l [] = l
uniao l (c:r)
  | pertence c l = uniao l r
  | otherwise    = uniao (l ++ [c]) r
-- 14
interseccao :: (Eq t) => [t] -> [t] -> [t]
interseccao [] l = []
interseccao (c:r) l
  | pertence c l = c : interseccao r l
  | otherwise    = interseccao r l
-- 22
rodarEsquerda :: (Integral t) => t -> [t] -> [t]
rodarEsquerda n (c:r)
  | n > 0     = rodarEsquerda (n-1) (r++[c])
  | otherwise = c:r
igual (c:r)
  | c == 97  = "Sim"
  | otherwise = "Não"
