module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
import GHC.RTS.Flags (ProfFlags(descrSelector))


hamburguesaVacia = Hamburguesa 0 []


correrTests :: IO ()
correrTests = hspec $ do
    testsParteI
    testsParteII
    focus testsParteIII

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
            it "Dada una hamburguesa de precio 10 y un descuento del 30%, devuelve una hamburguesa de precio 7." $ do 
                descuento 30 (Hamburguesa 10 []) `shouldBe` Hamburguesa 7 []
            it "La cuarto de libra debe tener un precio base de 20." $ do
                precioBase cuartoDeLibra `shouldBe` 20
        describe "pdepBurguer" $ do
            it "La Pdep Burguer tiene la siguiente lista de ingredientes: [Cheddar, Panceta, Carne, Carne, Pan, Carne, Cheddar, Pan]" $ do
                ingredientes pdepBurguer `shouldBe` [Cheddar, Panceta, Carne, Carne, Pan, Carne, Cheddar, Pan]
            it "El precio final de Pdep Burguer es de 110" $ do
                precioFinal pdepBurguer `shouldBe` 110
testsParteII =
    describe "Tests Parte II: Algunas hamburguesas más" $ do
        describe "dobleCuarto" $ do
            it "La doble cuarto de libra tiene la siguiente lista de ingredientes: [Cheddar, Carne, Pan, Carne, Cheddar, Pan]" $ do
                ingredientes dobleCuarto `shouldBe` [Cheddar, Carne, Pan, Carne, Cheddar, Pan]
            it "El precio final de la doble cuarto de libra debe ser 84." $ do
                precioFinal dobleCuarto `shouldBe` 84
        describe "bigPdep" $ do
            it "La BigPdep debe tener la siguiente lista de ingredientes: [Curry, Cheddar, Carne, Pan, Carne, Cheddar, Pan]." $ do
                ingredientes bigPdep `shouldBe` [Curry, Cheddar, Carne, Pan, Carne, Cheddar, Pan]
            it "El precio final de la BigPdep debe ser 89." $ do
                precioFinal bigPdep `shouldBe` 89
        describe "delDia" $ do
            it "Dada una hamburguesa, añade papas y realiza un descuento del 30% sobre el precio base." $ do
                delDia (Hamburguesa 10 [Pan, Carne, Cheddar, Pan]) `shouldBe` Hamburguesa 7 [Papas, Carne, Cheddar, Pan]

testsParteIII =
    describe "Tests Parte III: Algunos cambios más." $ do
        describe "hacerVeggie" $ do
            it "Dada una hamburguesa con carne o pollo, reemplaza dichos ingredientes por un número igual de patis veganas. Además, cambia la panceta y el cheddar por panceta de tofu y queso de almendras respectivamente." $ do
                hacerVeggie (Hamburguesa 0 [Pan, Carne, Pollo, Cheddar, Panceta, Pan]) `shouldBe` Hamburguesa 0 [PatiVegano, PatiVegano, QuesoDeAlmendras, PancetaDeTofu, Pan, Pan]
            it "Dada una hamburguesa vacía, devuelve otra vacía." $ do
                hacerVeggie (Hamburguesa 0 [])  `shouldBe` Hamburguesa 0 []
        --describe "reemplazarUnIngredientePorOtro" $ do
            --it "test" $ do
                --reemplazarIngredientePorOtro Carne Pollo (Hamburguesa 0 [Carne]) `shouldBe` Hamburguesa 0 [Pollo]
        describe "cambiarPanDePati" $ do
            it "Cambia el pan de una hamburguesa por pan integral." $ do
                cambiarPandePati (Hamburguesa 0 [Pan, Pan, Pan, Pan, Pan, Papas, Pan, Pan, Pan, Pan, Pan]) `shouldBe` Hamburguesa 0 [PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, PanIntegral, Papas]
            it "Si la hamburguesa no tiene pan, no cambia nada." $ do
                cambiarPandePati (Hamburguesa 0 [Cheddar, Carne, Papas]) `shouldBe` Hamburguesa 0 [Cheddar, Carne, Papas]
        describe "dobleCuartoVegano" $ do
            it "El Doble Cuarto Vegano tiene la siguiente lista de ingredientes: [PatiVegano, PatiVegano, QuesoDeAlmendras, PanIntegral, PanIntegral]." $ do
                ingredientes dobleCuartoVeggie `shouldBe` [PatiVegano, PatiVegano, QuesoDeAlmendras, QuesoDeAlmendras, PanIntegral, PanIntegral]