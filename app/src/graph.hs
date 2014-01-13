import Data.Map
import Data.Maybe

data Color = White | Grey | Black deriving (Enum, Show)
data Dist a = Infinity | Dist Integer deriving (Show, Eq)
data Vertex a = Vertex {dist :: (Dist a), pi :: Integer, color :: Color, id :: String} deriving (Show)

makeVertices = Prelude.map (\x -> (Vertex Infinity 0 White x))

vertices = makeVertices ["r", "s", "t", "u", "v", "w", "x", "y"]

adjList = fromList [("r", ["s", "v"]),
                    ("s", ["r", "w"]),
                    ("t", ["w", "x", "u"]),
                    ("u", ["t", "x", "y"]),
                    ("v", ["r"]),
                    ("w", ["s", "t", "x"]),
                    ("x", ["w", "t", "u", "y"]),
                    ("y", ["x", "u"])]

main = do
  print adjList
