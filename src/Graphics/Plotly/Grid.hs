{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Grids of subplots at the type level. A figure's type says what grid it
-- was made with, as a list of rows of 'CellSpec's, so that placing a trace
-- in a cell that does not exist, or that holds the wrong kind of subplot,
-- is a type error. "Graphics.Plotly.Subplots" makes and fills the grids.
--
-- @
-- type G = '[ '[ 'Span 1 2 XY, 'Blank ], '[ PlotY2, Domain ] ]
-- @
--
-- is a grid of two rows: an xy subplot across the first, and below it an
-- xy subplot with a secondary y axis beside a cell for domain traces (pies,
-- …).
module Graphics.Plotly.Grid
  ( -- * Grids
    CellSpec (..),
    FigureGrid (..),
    XY,
    Scene,
    Geo,
    Polar,
    Ternary,
    TileMap,
    Smith,
    Domain,
    Uniform,

    -- * Checks
    ValidGrid,
    KindAt,
    CheckCell,
    CheckSecondaryY,
    CanPlace,
    CheckRow,
    CheckCol,
    KindMatches,

    -- ** The checks, as type families
    check,
    natInt,
    CheckShape',
    CheckSpecs',
    CheckCover',
    Positions,
    CheckCell',
    CheckSecondaryY',
    CanPlace',
    CheckRow',
    CheckCol',
    KindFits',

    -- * Reflection
    KnownGrid (..),
    KnownSubplotKind (..),
    SpecInfo (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Type.Bool (Not, type (&&))
import Data.Type.Equality ((:~:) (..), type (==))
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (CmpNat, KnownNat, Nat, Symbol, natVal, type (+), type (-))
import Graphics.Plotly.TraceType

-- | What a cell of a grid holds, used as a kind.
data CellSpec
  = -- | A subplot of this kind.
    Plot SubplotKind
  | -- | An xy subplot with a secondary y axis on the right.
    PlotY2
  | -- | A subplot spanning this many rows and columns, from its own cell
    -- towards higher rows and columns. The other cells it covers must be
    -- 'Blank'.
    Span Nat Nat CellSpec
  | -- | No subplot: empty, or covered by a span.
    Blank

-- | Whether a figure has a grid of subplots, and which: the index of
-- 'Graphics.Plotly.Core.Figure'.
data FigureGrid
  = NoGrid
  | Grid [[CellSpec]]

-- | An xy subplot: x and y axes.
type XY = 'Plot 'SubplotXY

-- | A 3D scene.
type Scene = 'Plot 'SubplotScene

-- | A geo map.
type Geo = 'Plot 'SubplotGeo

-- | A polar subplot.
type Polar = 'Plot 'SubplotPolar

-- | A ternary subplot.
type Ternary = 'Plot 'SubplotTernary

-- | A tile map (@map@), named so as not to clash with "Data.Map".
type TileMap = 'Plot 'SubplotMap

-- | A Smith chart.
type Smith = 'Plot 'SubplotSmith

-- | A cell for traces placed by their @domain@: pies, sunbursts, tables, ….
type Domain = 'Plot 'SubplotDomain

-- | @r@ rows of @c@ cells, each @s@: @Uniform 2 3 XY@.
type family Uniform (r :: Nat) (c :: Nat) (s :: CellSpec) :: [[CellSpec]] where
  Uniform r c s = Replicate r (Replicate c s)

type family Replicate (n :: Nat) (x :: k) :: [k] where
  Replicate 0 _ = '[]
  Replicate n x = x ': Replicate (n - 1) x

-- * Lists

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

-- | The item at a position from 1, if there is one.
type family At (xs :: [k]) (n :: Nat) :: Maybe k where
  At _ 0 = 'Nothing
  At '[] _ = 'Nothing
  At (x ': _) 1 = 'Just x
  At (_ ': xs) n = At xs (n - 1)

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs

-- | The number of columns: the length of the first row.
type family Width (g :: [[CellSpec]]) :: Nat where
  Width '[] = 0
  Width (row ': _) = Length row

-- * Messages

type Position r c = 'Text "(" ':<>: 'ShowType r ':<>: 'Text ", " ':<>: 'ShowType c ':<>: 'Text ")"

type Size g = 'ShowType (Length g) ':<>: 'Text " by " ':<>: 'ShowType (Width g)

type family KindName (k :: SubplotKind) :: Symbol where
  KindName 'SubplotXY = "xy"
  KindName 'SubplotScene = "scene"
  KindName 'SubplotGeo = "geo"
  KindName 'SubplotPolar = "polar"
  KindName 'SubplotTernary = "ternary"
  KindName 'SubplotMap = "map"
  KindName 'SubplotSmith = "smith"
  KindName 'SubplotDomain = "domain"
  KindName 'SubplotNone = "none"

-- * Validity

-- | The grid is well formed: at least one row and one column, rows of the
-- same length, no cell of kind 'SubplotNone', every span at least 1 by 1
-- around a subplot, and no span reaching outside the grid or over another
-- cell.
--
-- Each check is an equality with @'()@, which a malformed grid makes a
-- 'TypeError' saying what is wrong.
type ValidGrid (g :: [[CellSpec]]) = (CheckShape' g ~ '(), CheckSpecs' g ~ '(), CheckCover' g (Positions g 1) ~ '())

-- | Both checks pass.
type family Both (a :: ()) (b :: ()) :: () where
  Both '() '() = '()

-- | At least one row and column, and no ragged rows.
type family CheckShape' (g :: [[CellSpec]]) :: () where
  CheckShape' '[] = TypeError ('Text "plotlyhs: a grid needs at least one row")
  CheckShape' ('[] ': _) = TypeError ('Text "plotlyhs: a grid needs at least one column")
  CheckShape' (row ': rows) = CheckRows' (Length row) rows 2

type family CheckRows' (n :: Nat) (rows :: [[CellSpec]]) (i :: Nat) :: () where
  CheckRows' _ '[] _ = '()
  CheckRows' n (row ': rows) i =
    Both
      ( If'
          (Length row == n)
          '()
          ( TypeError
              ( 'Text "plotlyhs: row "
                  ':<>: 'ShowType i
                  ':<>: 'Text " of the grid has "
                  ':<>: 'ShowType (Length row)
                  ':<>: 'Text " cells, but row 1 has "
                  ':<>: 'ShowType n
              )
          )
      )
      (CheckRows' n rows (i + 1))

-- | No cell of kind 'SubplotNone', and every span at least 1 by 1 around a
-- subplot.
type family CheckSpecs' (g :: [[CellSpec]]) :: () where
  CheckSpecs' '[] = '()
  CheckSpecs' (row ': rows) = Both (CheckRowSpecs' row) (CheckSpecs' rows)

type family CheckRowSpecs' (row :: [CellSpec]) :: () where
  CheckRowSpecs' '[] = '()
  CheckRowSpecs' (s ': ss) = Both (CheckSpec' s) (CheckRowSpecs' ss)

type family CheckSpec' (s :: CellSpec) :: () where
  CheckSpec' ('Plot 'SubplotNone) = TypeError ('Text "plotlyhs: a cell cannot hold a subplot of kind SubplotNone")
  CheckSpec' ('Span 0 _ _) = TypeError ('Text "plotlyhs: a span must cover at least one row")
  CheckSpec' ('Span _ 0 _) = TypeError ('Text "plotlyhs: a span must cover at least one column")
  CheckSpec' ('Span _ _ 'Blank) = TypeError ('Text "plotlyhs: a span must hold a subplot, not Blank")
  CheckSpec' ('Span _ _ ('Span _ _ _)) = TypeError ('Text "plotlyhs: a span cannot hold another span")
  CheckSpec' ('Span _ _ s) = CheckSpec' s
  CheckSpec' _ = '()

-- | The positions each cell covers, row by row from row @r@.
type family Positions (g :: [[CellSpec]]) (r :: Nat) :: [(Nat, Nat)] where
  Positions '[] _ = '[]
  Positions (row ': rows) r = RowPositions row r 1 ++ Positions rows (r + 1)

type family RowPositions (row :: [CellSpec]) (r :: Nat) (c :: Nat) :: [(Nat, Nat)] where
  RowPositions '[] _ _ = '[]
  RowPositions (s ': ss) r c = Covers s r c ++ RowPositions ss r (c + 1)

type family Covers (s :: CellSpec) (r :: Nat) (c :: Nat) :: [(Nat, Nat)] where
  Covers 'Blank _ _ = '[]
  Covers ('Span h w _) r c = Rect r c h w
  Covers _ r c = '[ '(r, c)]

type family Rect (r :: Nat) (c :: Nat) (h :: Nat) (w :: Nat) :: [(Nat, Nat)] where
  Rect _ _ 0 _ = '[]
  Rect r c h w = Run r c w ++ Rect (r + 1) c (h - 1) w

type family Run (r :: Nat) (c :: Nat) (w :: Nat) :: [(Nat, Nat)] where
  Run _ _ 0 = '[]
  Run r c w = '(r, c) ': Run r (c + 1) (w - 1)

-- | Every covered position is inside the grid and covered once.
type family CheckCover' (g :: [[CellSpec]]) (ps :: [(Nat, Nat)]) :: () where
  CheckCover' _ '[] = '()
  CheckCover' g ('(r, c) ': ps) =
    Both
      ( If'
          (Inside g r c)
          (If' (Elem '(r, c) ps) (TypeError ('Text "plotlyhs: two cells of the grid cover " ':<>: Position r c ':<>: 'Text "; a cell under a span must be Blank")) '())
          (TypeError ('Text "plotlyhs: a span reaches " ':<>: Position r c ':<>: 'Text ", outside the " ':<>: Size g ':<>: 'Text " grid"))
      )
      (CheckCover' g ps)

type family Inside (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) :: Bool where
  Inside g r c = Between r (Length g) && Between c (Width g)

type family Between (i :: Nat) (n :: Nat) :: Bool where
  Between 0 _ = 'False
  Between i n = Not (CmpNat i n == 'GT)

-- | 'Data.Type.Bool.If', without evaluating the branch not taken (which
-- may be a 'TypeError').
type family If' (cond :: Bool) (yes :: k) (no :: k) :: k where
  If' 'True yes _ = yes
  If' 'False _ no = no

-- * Cells

-- | The spec at a row and column, counted from 1, if they are inside the
-- grid.
type family Lookup (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) :: Maybe CellSpec where
  Lookup g r c = LookupIn (At g r) c

type family LookupIn (row :: Maybe [CellSpec]) (c :: Nat) :: Maybe CellSpec where
  LookupIn 'Nothing _ = 'Nothing
  LookupIn ('Just row) c = At row c

type Outside g r c = 'Text "plotlyhs: cell " ':<>: Position r c ':<>: 'Text " is outside the " ':<>: Size g ':<>: 'Text " grid (rows and columns count from 1)"

type BlankCell r c = 'Text "plotlyhs: cell " ':<>: Position r c ':<>: 'Text " of the grid is Blank: empty, or covered by a span"

type NoSecondaryY r c = 'Text "plotlyhs: cell " ':<>: Position r c ':<>: 'Text " has no secondary y axis; make it PlotY2"

-- | The kind of subplot in the cell at a row and column. It does not
-- reduce for a cell that is outside the grid or 'Blank'; 'CheckCell' says
-- why.
type family KindAt (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) :: SubplotKind where
  KindAt g r c = KindOf (Lookup g r c)

type family KindOf (s :: Maybe CellSpec) :: SubplotKind where
  KindOf ('Just s) = SpecKind s

type family SpecKind (s :: CellSpec) :: SubplotKind where
  SpecKind ('Plot k) = k
  SpecKind 'PlotY2 = 'SubplotXY
  SpecKind ('Span _ _ s) = SpecKind s

type family HasY2 (s :: CellSpec) :: Bool where
  HasY2 'PlotY2 = 'True
  HasY2 ('Span _ _ s) = HasY2 s
  HasY2 _ = 'False

-- | There is a subplot at the row and column.
type CheckCell g r c = CheckCell' g r c ~ '()

-- | There is a subplot with a secondary y axis at the row and column.
type CheckSecondaryY g r c = CheckSecondaryY' g r c ~ '()

-- | A trace of type @t@ can go in the cell at a row and column, on its
-- secondary y axis for 'True'.
type CanPlace g r c t y2 = CanPlace' g r c t y2 ~ '()

-- | The row is in the grid.
type CheckRow g r = CheckRow' g r ~ '()

-- | The column is in the grid.
type CheckCol g c = CheckCol' g c ~ '()

-- | 'CheckCell', as a type family.
type family CheckCell' (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) :: () where
  CheckCell' g r c = CellIs' g r c 'False (Lookup g r c)

-- | 'CheckSecondaryY', as a type family.
type family CheckSecondaryY' (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) :: () where
  CheckSecondaryY' g r c = CellIs' g r c 'True (Lookup g r c)

type family CellIs' (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) (y2 :: Bool) (s :: Maybe CellSpec) :: () where
  CellIs' g r c _ 'Nothing = TypeError (Outside g r c)
  CellIs' _ r c _ ('Just 'Blank) = TypeError (BlankCell r c)
  CellIs' _ r c 'True ('Just s) = If' (HasY2 s) '() (TypeError (NoSecondaryY r c))
  CellIs' _ _ _ 'False _ = '()

-- | 'CanPlace', as a type family.
type family CanPlace' (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) (t :: TraceType) (y2 :: Bool) :: () where
  CanPlace' g r c t y2 = PlaceOn' g r c t y2 (Lookup g r c)

type family PlaceOn' (g :: [[CellSpec]]) (r :: Nat) (c :: Nat) (t :: TraceType) (y2 :: Bool) (s :: Maybe CellSpec) :: () where
  PlaceOn' g r c _ _ 'Nothing = TypeError (Outside g r c)
  PlaceOn' _ r c _ _ ('Just 'Blank) = TypeError (BlankCell r c)
  PlaceOn' _ r c t 'True ('Just s) = If' (HasY2 s) (KindFits' t 'SubplotXY ('Text "cell " ':<>: Position r c)) (TypeError (NoSecondaryY r c))
  PlaceOn' _ r c t 'False ('Just s) = KindFits' t (SpecKind s) ('Text "cell " ':<>: Position r c)

-- | 'CheckRow', as a type family.
type family CheckRow' (g :: [[CellSpec]]) (r :: Nat) :: () where
  CheckRow' g r =
    If'
      (Between r (Length g))
      '()
      (TypeError ('Text "plotlyhs: row " ':<>: 'ShowType r ':<>: 'Text " is outside the " ':<>: Size g ':<>: 'Text " grid (rows count from 1)"))

-- | 'CheckCol', as a type family.
type family CheckCol' (g :: [[CellSpec]]) (c :: Nat) :: () where
  CheckCol' g c =
    If'
      (Between c (Width g))
      '()
      (TypeError ('Text "plotlyhs: column " ':<>: 'ShowType c ':<>: 'Text " is outside the " ':<>: Size g ':<>: 'Text " grid (columns count from 1)"))

-- | A trace of type @t@ can be drawn on a subplot of kind @k@.
type KindMatches (t :: TraceType) (k :: SubplotKind) = KindFits' t k ('Text "the cell") ~ '()

-- | A trace of type @t@ can be drawn on a subplot of kind @k@, which the
-- message says is where.
type family KindFits' (t :: TraceType) (k :: SubplotKind) (where_ :: ErrorMessage) :: () where
  KindFits' t k where_ =
    If'
      (TraceSubplotKind t == k)
      '()
      ( TypeError
          ( 'Text "plotlyhs: a "
              ':<>: 'Text (TraceName t)
              ':<>: 'Text " trace needs a "
              ':<>: 'Text (KindName (TraceSubplotKind t))
              ':<>: 'Text " subplot, but "
              ':<>: where_
              ':<>: 'Text " is "
              ':<>: 'Text (KindName k)
          )
      )

-- | Pass a check, given that it reduces to @'()@: @check \@(CanPlace' g r c t
-- 'False) x@ is @x@. It uses the check's evidence, so the check is not a
-- redundant constraint, and code compiled with @-fdefer-type-errors@
-- throws the check's type error here.
check :: forall (ok :: ()) a. (ok ~ '()) => a -> a
check x = case Refl :: ok :~: '() of Refl -> x
{-# NOINLINE check #-}

-- | The value of a type-level number.
natInt :: forall n. (KnownNat n) => Int
natInt = fromInteger (natVal (Proxy @n))

-- * Reflection

-- | A cell's spec, as a value.
data SpecInfo = SpecInfo
  { specKind :: SubplotKind,
    specSecondaryY :: Bool,
    specRowspan :: Int,
    specColspan :: Int
  }
  deriving stock (Eq, Show)

-- | Subplot kinds known at compile time.
class KnownSubplotKind (k :: SubplotKind) where
  subplotKindVal :: SubplotKind

instance KnownSubplotKind 'SubplotXY where subplotKindVal = SubplotXY

instance KnownSubplotKind 'SubplotScene where subplotKindVal = SubplotScene

instance KnownSubplotKind 'SubplotGeo where subplotKindVal = SubplotGeo

instance KnownSubplotKind 'SubplotPolar where subplotKindVal = SubplotPolar

instance KnownSubplotKind 'SubplotTernary where subplotKindVal = SubplotTernary

instance KnownSubplotKind 'SubplotMap where subplotKindVal = SubplotMap

instance KnownSubplotKind 'SubplotSmith where subplotKindVal = SubplotSmith

instance KnownSubplotKind 'SubplotDomain where subplotKindVal = SubplotDomain

instance KnownSubplotKind 'SubplotNone where subplotKindVal = SubplotNone

class KnownCellSpec (s :: CellSpec) where
  cellSpecVal :: Maybe SpecInfo

instance (KnownSubplotKind k) => KnownCellSpec ('Plot k) where
  cellSpecVal = Just (SpecInfo (subplotKindVal @k) False 1 1)

instance KnownCellSpec 'PlotY2 where
  cellSpecVal = Just (SpecInfo SubplotXY True 1 1)

instance (KnownNat h, KnownNat w, KnownCellSpec s) => KnownCellSpec ('Span h w s) where
  cellSpecVal = (\i -> i {specRowspan = natInt @h, specColspan = natInt @w}) <$> cellSpecVal @s

instance KnownCellSpec 'Blank where
  cellSpecVal = Nothing

class KnownRow (row :: [CellSpec]) where
  rowVal :: [Maybe SpecInfo]

instance KnownRow '[] where
  rowVal = []

instance (KnownCellSpec s, KnownRow ss) => KnownRow (s ': ss) where
  rowVal = cellSpecVal @s : rowVal @ss

-- | Grids known at compile time.
class KnownGrid (g :: [[CellSpec]]) where
  -- | The grid's cells, a list per row from row 1; 'Nothing' for 'Blank'.
  gridVal :: [[Maybe SpecInfo]]

instance KnownGrid '[] where
  gridVal = []

instance (KnownRow row, KnownGrid rows) => KnownGrid (row ': rows) where
  gridVal = rowVal @row : gridVal @rows
