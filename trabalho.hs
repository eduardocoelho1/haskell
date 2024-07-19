-- funções auxiliares
pertence :: (Eq t) => t -> [t] -> Bool
pertence _ [] = False
pertence e (c:r)
    | e == c = True
    | otherwise = pertence e r

-- 1
unica_ocorrencia :: (Eq t) => t -> [t] -> Bool
unica_ocorrencia _ [] = False
unica_ocorrencia e (c:r)
    | e == c    = not(pertence e r)
    | otherwise = unica_ocorrencia e r

-- 2
maiores_que :: (Real t) => t -> [t] -> [t]
maiores_que _ [] = []
maiores_que n (c:r)
    | c > n     = c : maiores_que n r
    | otherwise = maiores_que n r

-- 3
concatena :: [t] -> [t] -> [t]
concatena (a:b) c
    | not (null b) = a : concatena b c
    | otherwise    = a : c

-- 4
remove :: (Eq t) => t  -> [t] -> [t]
remove e (c:r)
    | e /= c    = c : remove e r
    | otherwise = r

-- 5
remover_ultimo :: (Eq t) => [t] -> [t]
remover_ultimo (c:r)
    | r /= []   = c : remover_ultimo r
    | otherwise = []

-- 6 (contém erro: remove à esquerda)
remover_repetidos :: (Eq t) => [t] -> [t]
remover_repetidos [] = []
remover_repetidos (c:r)
    | pertence c r = remover_repetidos r
    | otherwise    = c : remover_repetidos r

-- 7 (contém erro: altera a ordem)

maiores :: (Integral t) => Int -> [t] -> [t]
maiores n l
    | n > 1     = (maior l) : (maiores (n-1) (remove (maior l) l))
    | otherwise = [maior l]
    where maior :: (Integral t) => [t] -> t
          maior [c] = c
          maior (c1:c2:r)
            | c1 >= c2  = maior (c1:r)
            | otherwise = maior (c2:r)

-- 8
gera_sequencia :: (Integral t) => t -> [t]
gera_sequencia n
    | n > 1  = gera_sequencia (n-1) ++ [n,-n]
    | n == 1 = [1, -1]

-- 9
inverte :: [t] -> [t]
inverte (c:r)
    | not(null r) = inverte r ++ [c]
    | otherwise   = [c]

-- 10

-- 11
somatorio :: (Real t) => [t] -> t
somatorio [] = 0
somatorio (c:r) = c + somatorio r

-- 12
intercala :: (Eq t) => [t] -> [t] -> [t]
intercala l1 [] = l1
intercala [] l2 = l2
intercala l1@(c1:r1) l2@(c2:r2) = c1 : c2 : intercala r1 r2

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

-- 16
sequencia :: (Integral t) => t -> t -> [t]
sequencia n m
    | n > 0     = m : sequencia (n-1) (m+1)
    | otherwise = []

-- 17
insere_ordenado :: (Real t) => [t] -> t -> [t]
insere_ordenado [] n = [n]
insere_ordenado (c:r) n
    | n < c     = n:c:r
    | otherwise = c : insere_ordenado r n

-- 18
ordenado :: (Real t) => [t] -> Bool
ordenado [_] = True
ordenado (c1:c2:r)
    | c1 > c2   = False
    | otherwise = ordenado (c2:r)

-- 22
rodar_esquerda :: (Integral t) => t -> [t] -> [t]
rodar_esquerda n (c:r)
    | n > 0     = rodar_esquerda (n-1) (r++[c])
    | otherwise = c:r

-- 24
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas (c:r)
    | fromEnum c >= 97 && fromEnum c <= 122 = toEnum (fromEnum c - 32) : todas_maiusculas r
    | otherwise                             = c : todas_maiusculas r

-- 26
media :: [Double] -> Double
media l = somatorio l / comprimento l
    where comprimento :: (Real t) => [t] -> t
          comprimento [] = 0
          comprimento (c:r) = 1 + comprimento r

-- 27
variancia :: [Double] -> Double
variancia l@(c:r) = var l
    where var :: [Double] -> Double
          var [] = 0
          var (c:r) = (c - media l) ^ 2 / comprimento l + var r
              where comprimento :: (Real t) => [t] -> t
                    comprimento [] = 0
                    comprimento (c:r) = 1 + comprimento r

-- 29 (lista de char vira string)
seleciona :: [t] -> [Int] -> [t]
seleciona l [] = []
seleciona l (x:y) = seleciona_um l x : (seleciona l y)
    where seleciona_um :: [t] -> Int -> t
          seleciona_um (c:r) n
            | n > 1     = seleciona_um r (n-1)
            | otherwise = c
