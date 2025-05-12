module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


hamburguesaVacia = Hamburguesa 0 []


correrTests :: IO ()
correrTests = hspec $ do
    focus testsParteI


testsParteI =
    describe "Tests Parte I: Hamburguesas" $ do
        describe "agregarIngrediente" $ do
            it "Agrega correctamente los ingredientes a una hamburguesa." $ do
                Pan `elem` ingredientes (agregarIngrediente Pan hamburguesaVacia) `shouldBe` True
        describe "agrandar" $ do
            it "Dada una hamburguesa vacía, agrega carne." $ do
                Carne `elem` ingredientes (agrandar hamburguesaVacia) `shouldBe` True
            it "Dada una hamburguesa con carne, agrega otra carne." $ do
                length (filter (==Carne) (ingredientes (agrandar (Hamburguesa 20 [Carne])))) `shouldBe` 2
            it "Dada una hamburguesa con pollo, agrega otro pollo." $ do
                length (filter (==Pollo) (ingredientes (agrandar (Hamburguesa 10 [Pollo])))) `shouldBe` 2
            it "Dada una hamburguesa con un pati vegano, agrega otro pati vegano." $ do
                length (filter (==PatiVegano) (ingredientes (agrandar (Hamburguesa 10 [PatiVegano])))) `shouldBe` 2
        describe "descuento" $ do
            it "Dada una hamburguesa de precio 10 y un descuento del %30, devuelve una hamburguesa de precio 7." $ do 
                descuento 30 (Hamburguesa 10 []) `shouldBe` Hamburguesa 7 []
        describe "pdepBurguer" $ do
            it "La Pdep Burguer tiene la siguiente lista de ingredientes: [Cheddar, Panceta, Carne, Carne, Pan, Carne, Cheddar, Pan]" $ do
                ingredientes pdepBurguer `shouldBe` [Cheddar, Panceta, Carne, Carne, Pan, Carne, Cheddar, Pan]
            it "El precio final de Pdep Burguer es de 110" $ do
                precioFinal pdepBurguer `shouldBe` 110