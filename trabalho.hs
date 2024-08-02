import Char
-- funções auxiliares
pertence :: (Eq t) => [t] -> t -> Bool
pertence [] _ = False
pertence (c:r) e
    | e == c    = True
    | otherwise = pertence r e

aplica_em_todos :: (a -> b) -> [a] -> [b]
aplica_em_todos f []    = []
aplica_em_todos f (c:r) = f c : aplica_em_todos f r

ultimo :: (Eq t) => [t] -> t
ultimo (c:r)
    | r == []      = c
    | otherwise = ultimo r

compara :: (Ord t) => t -> [t] -> (t -> t -> Bool) -> Bool
compara _ [] func = False
compara t (c:r) func = (func t c)

somatorio :: (Real t) => [t] -> t
somatorio [] = 0
somatorio (c:r) = c + somatorio r

media :: [Double] -> Double
media l = somatorio l / comprimento l
    where comprimento :: (Real t) => [t] -> t
          comprimento []    = 0
          comprimento (c:r) = 1 + comprimento r

-- 3
concatena :: [t] -> [t] -> [t]
concatena (a:b) c
    | not (null b) = a : concatena b c
    | otherwise    = a : c

-- 6
remove_repetidos :: (Eq t) => [t] -> [t]
remove_repetidos [] = []
remove_repetidos (c:r) = c : remove_repetidos(remove_repetidos' c r)
        where   remove_repetidos' _ [] = []
                remove_repetidos' t (c:r)
                    | c == t = remove_repetidos' t r
                    | otherwise = c : remove_repetidos' t r

-- 9
inverte :: [t] -> [t]
inverte (c:r)
    | not(null r) = inverte r ++ [c]
    | otherwise   = [c]

-- 12
intercala :: (Eq t) => [t] -> [t] -> [t]
intercala l1 [] = l1
intercala [] l2 = l2
intercala l1@(c1:r1) l2@(c2:r2) = c1 : c2 : intercala r1 r2

-- 15
mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool 
mesmos_elementos l1 l2 = (mesmos_elementos' l1 l2) && (mesmos_elementos' l2 l1)
    where   mesmos_elementos' :: (Eq t) => [t] -> [t] -> Bool
            mesmos_elementos' [] _ = True
            mesmos_elementos' (c:r) l
                | pertence l c = mesmos_elementos' r l
                | otherwise    = False

-- 18
ordenado :: (Real t) => [t] -> Bool
ordenado [_] = True
ordenado (c1:c2:r)
    | c1 > c2   = False
    | otherwise = ordenado (c2:r)

-- 21 
picos :: (Ord t) => [t] -> [t]
picos [] = []
picos l@(x:y) = picos'((ultimo l) : l ++ [x])
    where picos' (a:b:[]) = []
          picos' (a:b:c:d)
              | b > a && b > c = b:picos'(b:c:d)
              | otherwise      = picos'(b:c:d)

-- 24
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas l = aplica_em_todos Char.toUpper l

-- 27
variancia :: [Double] -> Double
variancia l@(c:r) = var l
    where var :: [Double] -> Double
          var [] = 0
          var (c:r) = (c - media l) ^ 2 / comprimento l + var r
              where comprimento :: (Real t) => [t] -> t
                    comprimento [] = 0
                    comprimento (c:r) = 1 + comprimento r

-- 30
encontra_zero :: (Integral t) => [t] -> [t]
encontra_zero [] = []
encontra_zero (c:r)
	| c == 0 = r
	| otherwise = encontra_zero r
	
achou_zero :: (Integral t) => [t] -> Bool
achou_zero [] = False
achou_zero (c:r)
	| c == 0 = True
	| otherwise = achou_zero r

separa :: (Integral t) => [t] -> [[t]]
separa [] = [[]]
separa l1@(c:r)
	| c /= 0 && achou_zero l1 = [c:separa' r] ++ separa (encontra_zero l1)
	| c == 0 = [[]] ++ separa (encontra_zero l1)
	| otherwise = [c:separa' r]
		where	separa' [] = []
			separa' (c:r)
				| c /= 0 = c:separa' r
				| otherwise = []

-- 33
soma_digitos :: (Integral t) => t -> t
soma_digitos x
    | x < 10    = x
    | otherwise = mod x 10 + soma_digitos (div x 10)

-- 36
quadrado_perfeito :: (Integral t) => t -> Bool
quadrado_perfeito 1 = True
quadrado_perfeito x = quadrado_perfeito' (div x 2) (-1)
    where quadrado_perfeito' chute anterior
            | chute*chute == x  = True
            | chute == anterior = False
            | otherwise = quadrado_perfeito' (div (chute + div x chute) 2) chute