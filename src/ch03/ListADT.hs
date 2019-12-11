-- file: ch03/ListADT.hs
import Data.Maybe

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil


fromCons (Cons x xs) = x:(fromCons xs)
fromCons Nil = []


-- 请仿造 Java 示例，定义一种只需要一个构造器的树类型。
-- 不要使用 Empty 构造器，而是用 Maybe 表示节点的子节点。

data Tree a =
  Node a
  (Maybe (Tree a))
  (Maybe (Tree a))

emptyTree = Nothing
simpleTree1 = Node "one node" Nothing Nothing
simpleTree2 = Node "parent"
  (Just ((Node "left child") Nothing Nothing))
  (Just ((Node "right child") Nothing Nothing))
