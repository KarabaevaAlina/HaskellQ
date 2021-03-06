module Part4 where

-- | 8 tasks:
-- * Functor for Parser
-- * Applicative for Parser
-- * Monad for Parser
-- * Alternative for Parser
-- * Functor for Cont
-- * Applicative for Cont
-- * Monad for Cont
-- * ??? for Cont

import Part4.Types

import Control.Applicative
import Control.Monad (msum)
import Data.Maybe (maybeToList, fromJust)
import Data.Char (intToDigit, isSpace, isDigit)
import Text.Read (readMaybe)
import Data.List (isInfixOf, dropWhileEnd, elemIndex)

------------------------------------------------------------
-- PROBLEM #33
--
-- Написать экземпляр класса Functor для Parser
-- (удовлетворяющий законам)
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap func (Parser parseFunc) = Parser $ (map (\(str, input) -> (str, func input))) . parseFunc

------------------------------------------------------------
-- PROBLEM #34
--
-- Написать экземпляр класса Applicative для Parser
-- (удовлетворяющий законам)
instance Applicative Parser where
------------------------------------------------------------
-- PROBLEM #35
--
-- Написать экземпляр класса Alternative для Parser
-- (удовлетворяющий законам)
instance Alternative Parser where
------------------------------------------------------------
-- PROBLEM #36
--
-- Написать экземпляр класса Monad для Parser
-- (удовлетворяющий законам)
instance Monad Parser where

------------------------------------------------------------
-- PROBLEM #37
--
-- Написать экземпляр класса Functor для Foo
-- (удовлетворяющий законам)
instance Functor (Foo r) where
------------------------------------------------------------
-- PROBLEM #38
--
-- Написать экземпляр класса Applicative для Foo
-- (удовлетворяющий законам)
instance Applicative (Foo r) where
------------------------------------------------------------
-- PROBLEM #39
--
-- Написать экземпляр класса Monad для Foo
-- (удовлетворяющий законам)
instance Monad (Foo r) where

------------------------------------------------------------
-- PROBLEM #40
--
-- Написать парсер для выражений вида
--
--   <var> := <number>
--
-- Здесь вместо <var> может быть произвольное имя
-- переменной (начинается со строчной буквы, состоит из
-- букв, цифр и знаков подчеркивания), вместо <number> -
-- произвольное целое число. Между именем переменной и
-- присваиванием ":=" и между присваиванием ":=" и числом
-- может быть нуль или больше пробелов
--
-- В качестве результата парсер должен вернуть пару
-- (имя переменной, присваиваемое число)
prob40 :: Parser (String, Integer)
prob40 = error "Implement me!"
