module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 (Red a) = if (a <= 255) && (a >= 0) then True else False
prob7 (Green a) = if (a <= 255) && (a >= 0) then True else False
prob7 (Blue a) = if (a <= 255) && (a >= 0) then True else False

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 (Color x y z) (Red xi) = (Color (x+xi) y z)
prob8 (Color x y z) (Green yi) = (Color x (y+yi) z)
prob8 (Color x y z) (Blue zi) = (Color x y (z+zi))

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 (Red a) = a
prob9 (Blue a) = a
prob9 (Green a) = a

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color x y z) | (x > y) && (x > z) = Just (Red x)
                     | (y > x) && (y > z) = Just (Green x)
                     | (z > y) && (z > x) = Just (Blue x)
                     | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 (Tree left el right) = el + 
		(if isJust left then (prob11 $ fromJust left) else 0) + 
		(if isJust right then (prob11 $ fromJust right) else 0)

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 = checkTree

checkTree :: Ord a => Tree a -> Bool
checkTree tree = checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
checkRight Nothing x = True
checkRight (Just tree) parent = root tree >= parent && checkTree tree

checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
checkLeft Nothing x = True
checkLeft (Just tree) parent = root tree < parent && checkTree tree

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing

prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 a tree = hasValue a (Just tree)

hasValue :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
hasValue a Nothing = Nothing
hasValue a (Just tree)
  | a > root tree = hasValue a (right tree)
  | a < root tree = hasValue a (left tree)
  | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 tree = myTraverse tree (getNodesCountInTree tree) 

myTraverse tree num = 
  Tree (do
     maybeLeftTree <- left tree
     return (myTraverse maybeLeftTree (num - 1))
  ) 
  num 
  (do 
    maybeRightTree <- right tree
    return (myTraverse maybeRightTree (getNumberForRightTree tree num))
  )

getNumberForRightTree tree num = case left tree of
    Just leftSubTree -> num - (getNodesCountInTree leftSubTree + 1)
    Nothing -> num - 1

getNodesCountInTree tree = 1 + maybe 0 getNodesCountInTree (left tree) + maybe 0 getNodesCountInTree (right tree)

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree leftRotation $ tree & right
    where
        leftRotation rightSubTree = rightSubTree { left = Just oldRoot }
            where
                oldRoot = tree { right = rightSubTree & left }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)

prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rightRotation $ tree & left
    where
        rightRotation leftSubTree = leftSubTree { right = Just oldRoot }
            where
                oldRoot = tree { left = leftSubTree & right }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
