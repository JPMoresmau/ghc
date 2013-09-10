-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.PackageIndex (
  -- * Package index data type
  PackageIndex(..),

  -- * Creating an index
  fromList,
  mkPackageIndex,
  
  -- * Updates
  merge,
  
    -- * Queries
  -- ** Precise lookups
  lookupInstalledPackageId)
  
where
 
import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List as List
         (sort
         , groupBy, sortBy, nubBy, deleteFirstsBy )
import Data.Monoid (Monoid(..))
import Data.Version

import Distribution.Package
         ( PackageName(..), Package(..), packageName, packageVersion
         
         , InstalledPackageId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, installedPackageId )
import Distribution.Utils

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- Packages are uniquely identified in by their 'InstalledPackageId', they can
-- also be effeciently looked up by package name or by name and version.
--
data PackageIndex = PackageIndex
  -- The primary index. Each InstalledPackageInfo record is uniquely identified
  -- by its InstalledPackageId.
  --
  !(Map InstalledPackageId InstalledPackageInfo)

  -- This auxillary index maps package names (case-sensitively) to all the
  -- versions and instances of that package. This allows us to find all
  -- versions satisfying a dependency.
  --
  -- It is a three-level index. The first level is the package name,
  -- the second is the package version and the final level is instances
  -- of the same package version. These are unique by InstalledPackageId
  -- and are kept in preference order.
  --
  !(Map PackageName (Map Version [InstalledPackageInfo]))

  deriving (Show, Read)

instance Monoid PackageIndex where
  mempty  = PackageIndex Map.empty Map.empty
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: PackageIndex -> Bool
invariant (PackageIndex pids pnames) =
     map installedPackageId (Map.elems pids)
  == sort
     [ assert pinstOk (installedPackageId pinst)
     | (pname, pvers)  <- Map.toList pnames
     , let pversOk = not (Map.null pvers)
     , (pver,  pinsts) <- assert pversOk $ Map.toList pvers
     , let pinsts'  = sortBy (comparing installedPackageId) pinsts
           pinstsOk = all (\g -> length g == 1)
                          (groupBy (equating installedPackageId) pinsts')
     , pinst           <- assert pinstsOk $ pinsts'
     , let pinstOk = packageName    pinst == pname
                  && packageVersion pinst == pver
     ]


--
-- * Internal helpers
--

mkPackageIndex :: Map InstalledPackageId InstalledPackageInfo
               -> Map PackageName (Map Version [InstalledPackageInfo])
               -> PackageIndex
mkPackageIndex pids pnames = assert (invariant index) index
  where index = PackageIndex pids pnames


--
-- * Construction
--

-- | Build an index out of a bunch of packages.
--
-- If there are duplicates by 'InstalledPackageId' then later ones mask earlier
-- ones.
--
fromList :: [InstalledPackageInfo] -> PackageIndex
fromList pkgs = mkPackageIndex pids pnames
  where
    pids      = Map.fromList [ (installedPackageId pkg, pkg) | pkg <- pkgs ]
    pnames    =
      Map.fromList
        [ (packageName (head pkgsN), pvers)
        | pkgsN <- groupBy (equating  packageName)
                 . sortBy  (comparing packageId)
                 $ pkgs
        , let pvers =
                Map.fromList
                [ (packageVersion (head pkgsNV),
                   nubBy (equating installedPackageId) (reverse pkgsNV))
                | pkgsNV <- groupBy (equating packageVersion) pkgsN
                ]
        ]

--
-- * Updates
--

-- | Merge two indexes.
--
-- Packages from the second mask packages from the first if they have the exact
-- same 'InstalledPackageId'.
--
-- For packages with the same source 'PackageId', packages from the second are
-- \"preferred\" over those from the first. Being preferred means they are top
-- result when we do a lookup by source 'PackageId'. This is the mechanism we
-- use to prefer user packages over global packages.
--
merge :: PackageIndex -> PackageIndex -> PackageIndex
merge (PackageIndex pids1 pnames1) (PackageIndex pids2 pnames2) =
  mkPackageIndex (Map.union pids1 pids2)
                 (Map.unionWith (Map.unionWith mergeBuckets) pnames1 pnames2)
  where
    -- Packages in the second list mask those in the first, however preferred
    -- packages go first in the list.
    mergeBuckets xs ys = ys ++ (xs \\ ys)
    (\\) = deleteFirstsBy (equating installedPackageId)
        
--
-- * Lookups
--

-- | Does a lookup by source package id (name & version).
--
-- Since multiple package DBs mask each other by 'InstalledPackageId',
-- then we get back at most one package.
--
lookupInstalledPackageId :: PackageIndex -> InstalledPackageId
                         -> Maybe InstalledPackageInfo
lookupInstalledPackageId (PackageIndex pids _) pid = Map.lookup pid pids