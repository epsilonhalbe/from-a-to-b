{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Graph where

-- import qualified Data.Text as Tx
import Data.Text (Text)
import Data.Function (on)
import Data.Sequence (Seq((:|>)), (|>) )
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Id a
  = Id { unId :: Word }
  deriving (Show, Eq, Ord)

newtype Length =
  Length { unLength :: Word }
  deriving (Show, Eq, Ord)

instance Semigroup Length where
  (Length x) <> (Length y) = Length (x + y)

data Node
  = Node
  { label :: Text
  , nodeId :: Id Node
  }
  deriving (Show, Eq)

instance Ord Node where
  compare = compare `on` nodeId

data Neighbour
  = Neighbour
  { neighbour :: Node
  , distance :: Length
  }
  deriving (Show, Eq)

instance Ord Neighbour where
  compare = compare `on` distance

data Path
  = Path
  { path :: Seq Node
  , len :: Length
  }
  deriving (Show, Eq)

instance Ord Path where
  compare = compare `on` len

newtype Reds = Reds { reds :: Map Node Yellows }

newtype Yellows = Yellows { yellows :: Set Neighbour }

newtype Greens = Greens { greens :: Set Path }

start :: Node -> Greens
start node =
  Greens
    $ Set.singleton
    $ Path { path = Seq.singleton node
           , len = Length 0
           }

neighbours :: Node -> Reds -> Maybe (Yellows, Reds)
neighbours n Reds{..} = do
  nbh <- n `Map.lookup` reds
  pure (nbh, Reds $ n `Map.delete` reds)

shortestPath :: Reds -> Node -> Node -> Maybe Path
shortestPath g from to = do
  let gg = start from
  (yy, rr) <- from `neighbours` g
  shortestPath' from to rr yy gg

shortestPath' :: Node -> Node -> Reds -> Yellows -> Greens -> Maybe Path
shortestPath' node to rr Yellows{..} g@Greens{..} =
  if node == to
     then Set.lookupMin $ Set.filter (endsWith to) greens
     else do
       (nbh@Neighbour{..}, y) <- Set.minView yellows
       let greens' = makeGreen node nbh g
       (yellows', reds') <- tellNeighbours distance neighbour (Yellows y) rr
       shortestPath' neighbour to reds' yellows' greens'

tellNeighbours :: Length -> Node -> Yellows -> Reds -> Maybe (Yellows, Reds)
tellNeighbours dist next Yellows{..} Reds{..} = do
  Yellows{yellows = yellows'} <- next `Map.lookup` reds
  let updDistance nbh@Neighbour{distance} = nbh{distance = dist <> distance}
  pure ( Yellows $ yellows `Set.union` (Set.map updDistance yellows')
       , Reds $ Map.withoutKeys reds (Set.map neighbour yellows')
       )

makeGreen :: Node -> Neighbour -> Greens -> Greens
makeGreen node Neighbour{..} Greens{..} =
  let updPath Path{..} =
        Path{ path = path |> neighbour
            , len = len <> distance
            }
      greens' = Set.map updPath
              $ Set.filter (endsWith node) greens
   in Greens $ greens `Set.union` greens'



endsWith :: Node -> Path -> Bool
endsWith node Path{path} =
  case path of
    _ :|> n -> n == node
    _ -> False


