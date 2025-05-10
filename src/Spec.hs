module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


hamburguesaMegaTesteadora :: Hamburguesa
hamburguesaMegaTesteadora {precioBase = 100, ingredientes= [Pan,Carne,Cheddar,Carne,Pan]}

hamburguesaMegaTesteadoraVeggie :: Hamburguesa
hamburguesaMegaTesteadora {precioBase = 100, ingredientes= [PatiVegano, PatiVegano, Pan,Cheddar,Pan]}


correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4
        it "siiii" $ do
            reemplazarIngredientePorOtro Carne PatiVegano hamburguesaMegaTesteadora `shouldBe` hamburguesaMegaTesteadoraVeggie