module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> a) -> (Float -> Float -> a) -> (Expr -> a -> Expr -> a -> a)  -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> Expr -> a
recrExpr fCons fRang fSum fRes fMul fDiv e  = case e of 
                                  Const a   -> fCons a 
                                  Rango a b -> fRang a b
                                  Suma a b  -> fSum a (rec a) b (rec b)
                                  Resta a b -> fRes a (rec a) b (rec b)
                                  Mult a b  -> fMul a (rec a) b (rec b)
                                  Div a b   -> fDiv a (rec a) b  (rec b)
                                  where
                                    rec     = recrExpr fCons fRang fSum fRes fMul fDiv 

foldExpr :: (Float-> a) -> (Float -> Float -> a) -> (a -> a -> a)  -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr fCons fRang fSum fRes fMul fDiv e  = case e of
                                  Const a   -> fCons a
                                  Rango a b -> fRang a b 
                                  Suma a b  -> fSum (rec a) (rec b)
                                  Resta a b -> fRes (rec a) (rec b)
                                  Mult a b  -> fMul (rec a) (rec b)
                                  Div a b   -> fDiv (rec a) (rec b)
                                  where 
                                    rec     = foldExpr fCons fRang fSum fRes fMul fDiv
   

--eval :: Expr -> G Float
--eval = foldExpr (id) (\a b -> dameUno (a, b)) (+) (-) (*) (/)
-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.


actualizarGen :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
actualizarGen op ga gb gen0 =
  let (a, gen1) = ga gen0
      (b, gen2) = gb gen1
  in (op a b, gen2)


eval :: Expr -> G Float
eval = foldExpr
  (,)                
  (\a b -> dameUno (a, b))
  (actualizarGen (+))
  (actualizarGen (-))
  (actualizarGen (*))
  (actualizarGen (/))
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g  = (histograma m (rango95 xs) xs, g')
              where
                (xs, g') = muestra f n g
-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n e = armarHistograma m n (eval e)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.

opBinaria :: (Expr -> Bool) -> (Expr -> Bool) -> String -> Expr -> String -> Expr -> String -> String
opBinaria fIzq fDer op e1 s1 e2 s2 =
  maybeParen (fIzq e1) s1 ++ " " ++ op ++ " " ++ maybeParen (fDer e2) s2
mostrar :: Expr -> String
mostrar = recrExpr
  show
  (\a b -> show a ++ "~" ++ show b)
  (opBinaria (\e -> not (esLiteral e || esSuma e))
             (\e -> not (esLiteral e || esSuma e)) "+")
  (opBinaria (not . esLiteral)
             (not . esLiteral) "-")
  (opBinaria (\e -> not (esLiteral e || esMult e))
             (\e -> not (esLiteral e || esMult e)) "*")
  (opBinaria (not . esLiteral)
             (not . esLiteral) "/")
  where
    esLiteral e = case constructor e of
      CEConst -> True
      CERango -> True
      _       -> False
    
    esSuma e = constructor e == CESuma
    
    esResta e = constructor e == CEResta
    
    esMult e = constructor e == CEMult
    
    esDiv e = constructor e == CEDiv
      
data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)
-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv
-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
