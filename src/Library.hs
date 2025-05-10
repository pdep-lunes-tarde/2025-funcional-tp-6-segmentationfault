module Library where
import PdePreludat
import GHC.IO.Windows.Handle (HANDLE)
import GHC.Generics (prec)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | Carne `elem` ingredientes hamburguesa = hamburguesa {precioBase = precioBase hamburguesa + 20, ingredientes = Carne : ingredientes hamburguesa }
    | Pollo `elem` ingredientes hamburguesa = hamburguesa {precioBase = precioBase hamburguesa + 10, ingredientes = Pollo : ingredientes hamburguesa}
    | otherwise = hamburguesa { precioBase = precioBase hamburguesa + 20, ingredientes = Carne : ingredientes hamburguesa }

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamb =
    hamb {precioBase = precioBase hamb + precioIngrediente ingrediente, ingredientes = ingrediente : ingredientes hamb}

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento des hamb = hamb{precioBase = precioBase hamb - (precioBase hamb * des / 100)}

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

pdepBurguer :: Hamburguesa
pdepBurguer = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

-- dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agrandar $ cuartoDeLibra



