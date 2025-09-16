module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 9 "incierticalc" ~?= "incierticalc",
      alinearDerecha 7 "VP" ~?= "     VP"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 2 (+ 5) [1, 2, 3] ~?= [1, 2, 8],
      actualizarElem 2 (\x -> x - 2) [5, 6, 7] ~?= [5, 6, 5],
      actualizarElem 0 (* 0) [9] ~?= [0]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      length (casilleros (vacio 4 (0, 8))) ~?= 6
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casCantidad (casilleros (agregar 10 h0) !! 4) ~?= 1,
          let h1 = agregar 1 (agregar 1 (agregar 4 h0))
          in casCantidad (casilleros h1 !! 1) ~?= 2
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 4 (1, 5) [] ~?= vacio 4 (1, 5),
      histograma 3 (0, 9) [1, 2, 2, 8, 9] ~?= agregar 9 (agregar 8 (agregar 2 (agregar 2 (agregar 1 (vacio 3 (0, 9))))))    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casCantidad (casilleros (agregar 3 (vacio 3 (0, 6))) !! 2) ~?= 1,
      casPorcentaje (casilleros (agregar 3 (vacio 3 (0, 6))) !! 2) ~?= 100.0,
      let h = histograma 3 (0, 6) [0, 1, 2, 3, 4, 5]
      in sum (map casCantidad (casilleros h)) ~?= 6    ]

testsRecr :: Test
testsRecr =
  test
    [
      let expr1 = Suma (Const 1) (Rango 2 3)
          contadorCons _ = 1
          contadorRang _ _ = 1
          contadorSum _ r1 _ r2 = 1 + r1 + r2
          contadorRes _ r1 _ r2 = 1 + r1 + r2
          contadorMul _ r1 _ r2 = 1 + r1 + r2
          contadorDiv _ r1 _ r2 = 1 + r1 + r2
      in recrExpr contadorCons contadorRang contadorSum contadorRes contadorMul contadorDiv expr1 ~?= 3,
      let expr2 = Mult (Const 2) (Suma (Const 1) (Const 3))
      in recrExpr (\_ -> 1) (\_ _ -> 1) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) expr2 ~?= 5,
      let expr3 = Resta (Suma (Const 1) (Const 2)) (Div (Const 10) (Const 5))
      in recrExpr (\_ -> 1) (\_ _ -> 1) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) (\_ r1 _ r2 -> 1 + r1 + r2) expr3 ~?= 7

    ]

testsFold :: Test
testsFold =
  test
    [ 
      let expr1 = Mult (Const 2) (Suma (Const 1) (Const 3))
          fCons _ = 1
          fRang _ _ = 1
          fSum a b = 1 + a + b
          fRes a b = 1 + a + b
          fMul a b = 1 + a + b
          fDiv a b = 1 + a + b
      in foldExpr fCons fRang fSum fRes fMul fDiv expr1 ~?= 5,
      let expr2 = Suma (Const 1) (Suma (Const 2) (Const 3))
      in foldExpr (\_ -> 1) (\_ _ -> 1) (\a b -> 1 + a + b) (\a b -> 1 + a + b) (\a b -> 1 + a + b) (\a b -> 1 + a + b) expr2 ~?= 5,
      let expr3 = Div (Mult (Const 2) (Const 3)) (Const 4)
      in foldExpr (\_ -> 1) (\_ _ -> 1) (\a b -> 1 + a + b) (\a b -> 1 + a + b) (\a b -> 1 + a + b) (\a b -> 1 + a + b) expr3 ~?= 5
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Mult (Rango 2 4) (Const 3)) genFijo) ~?= 9.0,
      fst (eval (Mult (Const 2) (Const 3)) genFijo) ~?= 6.0,
      fst (eval (Div (Const 10) (Const 2)) genFijo) ~?= 5.0,
      fst (eval (Suma (Const 1) (Suma (Const 2) (Const 3))) genFijo) ~?= 6.0
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [
      let (h, _) = armarHistograma 3 3 (dameUno (2, 4)) genFijo
      in casCantidad (casilleros h !! 2) ~?= 3,
      let (h2, _) = armarHistograma 4 5 (dameUno (0, 10)) (genNormalConSemilla 1)
      in sum (map casCantidad (casilleros h2)) ~?= 5,
      let (h3, _) = armarHistograma 2 6 (dameUno (5, 5)) genFijo 
      in casCantidad (casilleros h3 !! 2) ~?= 6
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [
      let (h, _) = evalHistograma 3 3 (Const 5) genFijo
      in casCantidad (casilleros h !! 2) ~?= 3,
      let (h2, _) = evalHistograma 4 5 (Const 2) (genNormalConSemilla 2)
      in sum (map casCantidad (casilleros h2)) ~?= 5,
      let (h3, _) = evalHistograma 2 4 (Suma (Const 1) (Const 1)) genFijo
      in sum (map casCantidad (casilleros h3)) ~?= 4
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
