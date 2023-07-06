module SparseArray where

data Value a = Null | Value a
  deriving (Eq,Read,Show)
data SparseArray a = Vacio | Nodo (Value a) (SparseArray a) (SparseArray a)
  deriving (Eq,Read,Show)

-- Función num2bin: recibe un Int y devuelve una lista con los dígitos de su representación en binario --
num2bin :: Int -> [Bool]
num2bin 0 = [False]
num2bin n = num2binAux n
  where num2binAux :: Int -> [Bool]
        num2binAux 0 = []
        num2binAux n = (num2binAux (div n 2))++[(mod n 2)==1]

-- Función newSparseArray: devuelve un SparseArray vacío --
newSparseArray :: Eq a => SparseArray a
newSparseArray = Vacio

-- Función set: recibe un SparseArray, una posición y un elemento y cambia el valor del SparseArray de dicha posición --
set :: Eq a => SparseArray a -> Int -> a -> SparseArray a
set arbol numero y = setAux arbol (num2bin numero) y
  where
   setAux :: Eq a => SparseArray a -> [Bool] -> a -> SparseArray a
   setAux Vacio [] y = Nodo (Value y) Vacio Vacio
   setAux Vacio (x:xs) y
    | (x==True) = Nodo Null Vacio (setAux (Vacio) xs y)
    | (x==False) = Nodo Null (setAux (Vacio) xs y) Vacio
   setAux (Nodo n izquierda derecha) [] y = Nodo (Value y) izquierda derecha
   setAux (Nodo n izquierda derecha) (x:xs) y
    | (x==True) = Nodo n izquierda (setAux (derecha) xs y)
    | (x==False) = Nodo n (setAux (izquierda) xs y) derecha

-- Función get: recibe un SparseArray y una posición y devuelve el elemento del SparseArray en dicha posición --
get :: Eq a => SparseArray a -> Int -> (Value a)
get arbol numero = getAux arbol (num2bin numero)
 where
  getAux :: Eq a => SparseArray a -> [Bool] -> (Value a)
  getAux Vacio _ = Null
  getAux (Nodo n izquierda derecha) [] = n
  getAux (Nodo n izquierda derecha) (x:xs)
   | (x==True) = getAux derecha xs
   | (x==False) = getAux izquierda xs

-- Función delete: recibe un SparseArray y una posición y devuelve el SparseArray resultado de eliminar dicha posición --
--                 También elimina todos los nodos vacíos que resulten de la eliminación                               --
delete :: Eq a => SparseArray a -> Int -> SparseArray a
delete arbol numero = deleteAux arbol (num2bin numero)
 where
  deleteAux :: Eq a => SparseArray a -> [Bool] -> SparseArray a
  deleteAux Vacio _ = Vacio
  deleteAux (Nodo n izquierda derecha) []
   | (izquierda==Vacio)&&(derecha==Vacio) = Vacio
   | otherwise = Nodo Null izquierda derecha
  deleteAux (Nodo n izquierda derecha) (x:xs)
   | (x==True) = comprobar (Nodo n izquierda (deleteAux derecha xs))
   | (x==False) = comprobar (Nodo n (deleteAux izquierda xs) derecha)
    where
     comprobar :: Eq a => SparseArray a -> SparseArray a
     comprobar Vacio = Vacio
     comprobar (Nodo n izquierda derecha)
      | ((n==Null)&&(izquierda==Vacio)&&(derecha==Vacio)) = Vacio
      | otherwise = Nodo n izquierda derecha