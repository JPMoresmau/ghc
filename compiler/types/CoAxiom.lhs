%
% (c) The University of Glasgow 2012
%

\begin{code}

{-# LANGUAGE GADTs, ScopedTypeVariables #-}

-- | Module for coercion axioms, used to represent type family instances
-- and newtypes

module CoAxiom (
       Branched, Unbranched, BranchIndex, BranchList(..),
       toBranchList, fromBranchList,
       toBranchedList, toUnbranchedList,
       brListLength, brListNth, brListMap, brListFoldr, brListMapM,
       brListFoldlM_, brListZipWith,

       CoAxiom(..), CoAxBranch(..), 

       toBranchedAxiom, toUnbranchedAxiom,
       coAxiomName, coAxiomArity, coAxiomBranches,
       coAxiomTyCon, isImplicitCoAxiom, coAxiomNumPats,
       coAxiomNthBranch, coAxiomSingleBranch_maybe, coAxiomRole,
       coAxiomSingleBranch, coAxBranchTyVars, coAxBranchRoles,
       coAxBranchLHS, coAxBranchRHS, coAxBranchSpan, coAxBranchIncomps,
       placeHolderIncomps,

       Role(..), pprFullRole
       ) where 

import {-# SOURCE #-} TypeRep ( Type )
import {-# SOURCE #-} TyCon ( TyCon )
import Outputable
import FastString
import Name
import Unique
import Var
import Util
import Binary
import BasicTypes
import Data.Typeable ( Typeable )
import SrcLoc
import qualified Data.Data as Data

#include "HsVersions.h"

\end{code}

Note [Coercion axiom branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to allow type family instance groups, an axiom needs to contain an
ordered list of alternatives, called branches. The kind of the coercion built
from an axiom is determined by which index is used when building the coercion
from the axiom.

For example, consider the axiom derived from the following declaration:

type instance where
  F [Int] = Bool
  F [a]   = Double
  F (a b) = Char

This will give rise to this axiom:

axF :: {                                           F [Int] ~ Bool
       ; forall (a :: *).                          F [a]   ~ Double
       ; forall (k :: BOX) (a :: k -> *) (b :: k). F (a b) ~ Char
       }

The axiom is used with the AxiomInstCo constructor of Coercion. If we wish
to have a coercion showing that F (Maybe Int) ~ Char, it will look like

axF[2] <*> <Maybe> <Int> :: F (Maybe Int) ~ Char
-- or, written using concrete-ish syntax --
AxiomInstCo axF 2 [Refl *, Refl Maybe, Refl Int]

Note that the index is 0-based.

For type-checking, it is also necessary to check that no previous pattern
can unify with the supplied arguments. After all, it is possible that some
of the type arguments are lambda-bound type variables whose instantiation may
cause an earlier match among the branches. We wish to prohibit this behavior,
so the type checker rules out the choice of a branch where a previous branch
can unify. See also [Branched instance checking] in FamInstEnv.hs.

For example, the following is malformed, where 'a' is a lambda-bound type
variable:

axF[2] <*> <a> <Bool> :: F (a Bool) ~ Char

Why? Because a might be instantiated with [], meaning that branch 1 should
apply, not branch 2. This is a vital consistency check; without it, we could
derive Int ~ Bool, and that is a Bad Thing.

Note [Branched axioms]
~~~~~~~~~~~~~~~~~~~~~~~
Although a CoAxiom has the capacity to store many branches, in certain cases,
we want only one. These cases are in data/newtype family instances, newtype
coercions, and type family instances declared with "type instance ...", not
"type instance where". Furthermore, these unbranched axioms are used in a
variety of places throughout GHC, and it would difficult to generalize all of
that code to deal with branched axioms, especially when the code can be sure
of the fact that an axiom is indeed a singleton. At the same time, it seems
dangerous to assume singlehood in various places through GHC.

The solution to this is to label a CoAxiom with a phantom type variable
declaring whether it is known to be a singleton or not. The list of branches
is stored using a special form of list, declared below, that ensures that the
type variable is accurate.

As of this writing (Dec 2012), it would not be appropriate to use a promoted
type as the phantom type, so we use empty datatypes. We wish to have GHC
remain compilable with GHC 7.2.1. If you are revising this code and GHC no
longer needs to remain compatible with GHC 7.2.x, then please update this
code to use promoted types.


%************************************************************************
%*                                                                      *
                    Branch lists
%*                                                                      *
%************************************************************************

\begin{code}
type BranchIndex = Int  -- The index of the branch in the list of branches
                        -- Counting from zero

-- the phantom type labels
data Unbranched deriving Typeable
data Branched deriving Typeable

data BranchList a br where
  FirstBranch :: a -> BranchList a br
  NextBranch :: a -> BranchList a br -> BranchList a Branched

-- convert to/from lists
toBranchList :: [a] -> BranchList a Branched
toBranchList [] = pprPanic "toBranchList" empty
toBranchList [b] = FirstBranch b
toBranchList (h:t) = NextBranch h (toBranchList t)

fromBranchList :: BranchList a br -> [a]
fromBranchList (FirstBranch b) = [b]
fromBranchList (NextBranch h t) = h : (fromBranchList t)

-- convert from any BranchList to a Branched BranchList
toBranchedList :: BranchList a br -> BranchList a Branched
toBranchedList (FirstBranch b) = FirstBranch b
toBranchedList (NextBranch h t) = NextBranch h t

-- convert from any BranchList to an Unbranched BranchList
toUnbranchedList :: BranchList a br -> BranchList a Unbranched
toUnbranchedList (FirstBranch b) = FirstBranch b
toUnbranchedList _ = pprPanic "toUnbranchedList" empty

-- length
brListLength :: BranchList a br -> Int
brListLength (FirstBranch _) = 1
brListLength (NextBranch _ t) = 1 + brListLength t

-- lookup
brListNth :: BranchList a br -> BranchIndex -> a
brListNth (FirstBranch b) 0 = b
brListNth (NextBranch h _) 0 = h
brListNth (NextBranch _ t) n = brListNth t (n-1)
brListNth _ _ = pprPanic "brListNth" empty

-- map, fold
brListMap :: (a -> b) -> BranchList a br -> [b]
brListMap f (FirstBranch b) = [f b]
brListMap f (NextBranch h t) = f h : (brListMap f t)

brListFoldr :: (a -> b -> b) -> b -> BranchList a br -> b
brListFoldr f x (FirstBranch b) = f b x
brListFoldr f x (NextBranch h t) = f h (brListFoldr f x t)

brListMapM :: Monad m => (a -> m b) -> BranchList a br -> m [b]
brListMapM f (FirstBranch b) = f b >>= \fb -> return [fb]
brListMapM f (NextBranch h t) = do { fh <- f h
                                   ; ft <- brListMapM f t
                                   ; return (fh : ft) }

brListFoldlM_ :: forall a b m br. Monad m
              => (a -> b -> m a) -> a -> BranchList b br -> m ()
brListFoldlM_ f z brs = do { _ <- go z brs
                           ; return () }
  where go :: forall br'. Monad m => a -> BranchList b br' -> m a
        go acc (FirstBranch b)  = f acc b
        go acc (NextBranch h t) = do { fh <- f acc h
                                     ; go fh t }

-- zipWith
brListZipWith :: (a -> b -> c) -> BranchList a br1 -> BranchList b br2 -> [c]
brListZipWith f (FirstBranch a) (FirstBranch b) = [f a b]
brListZipWith f (FirstBranch a) (NextBranch b _) = [f a b]
brListZipWith f (NextBranch a _) (FirstBranch b) = [f a b]
brListZipWith f (NextBranch a ta) (NextBranch b tb) = f a b : brListZipWith f ta tb

-- pretty-printing

instance Outputable a => Outputable (BranchList a br) where
  ppr = ppr . fromBranchList
\end{code}

%************************************************************************
%*                                                                      *
                    Coercion axioms
%*                                                                      *
%************************************************************************

Note [Storing compatibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During axiom application, we need to be aware of which branches are compatible
with which others. The full explanation is in Note [Compatibility] in
FamInstEnv. (The code is placed there to avoid a dependency from CoAxiom on
the unification algorithm.) Although we could theoretically compute
compatibility on the fly, this is silly, so we store it in a CoAxiom.

Specifically, each branch refers to all other branches with which it is
incompatible. This list might well be empty, and it will always be for the
first branch of any axiom.

CoAxBranches that do not (yet) belong to a CoAxiom should have a panic thunk
stored in cab_incomps. The incompatibilities are properly a property of the
axiom as a whole, and they are computed only when the final axiom is built.

During serialization, the list is converted into a list of the indices
of the branches.

\begin{code}
-- | A 'CoAxiom' is a \"coercion constructor\", i.e. a named equality axiom.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data CoAxiom br
  = CoAxiom                   -- Type equality axiom.
    { co_ax_unique   :: Unique        -- unique identifier
    , co_ax_name     :: Name          -- name for pretty-printing
    , co_ax_role     :: Role          -- role of the axiom's equality
    , co_ax_tc       :: TyCon         -- the head of the LHS patterns
    , co_ax_branches :: BranchList CoAxBranch br
                                      -- the branches that form this axiom
    , co_ax_implicit :: Bool          -- True <=> the axiom is "implicit"
                                      -- See Note [Implicit axioms]
         -- INVARIANT: co_ax_implicit == True implies length co_ax_branches == 1.
    }
  deriving Typeable

data CoAxBranch
  = CoAxBranch
    { cab_loc      :: SrcSpan       -- Location of the defining equation
                                    -- See Note [CoAxiom locations]
    , cab_tvs      :: [TyVar]       -- Bound type variables; not necessarily fresh
                                    -- See Note [CoAxBranch type variables]
    , cab_roles    :: [Role]        -- See Note [CoAxBranch roles]
    , cab_lhs      :: [Type]        -- Type patterns to match against
    , cab_rhs      :: Type          -- Right-hand side of the equality
    , cab_incomps  :: [CoAxBranch]  -- The previous incompatible branches
                                    -- See Note [Storing compatibility]
    }
  deriving Typeable

toBranchedAxiom :: CoAxiom br -> CoAxiom Branched
toBranchedAxiom (CoAxiom unique name role tc branches implicit)
  = CoAxiom unique name role tc (toBranchedList branches) implicit

toUnbranchedAxiom :: CoAxiom br -> CoAxiom Unbranched
toUnbranchedAxiom (CoAxiom unique name role tc branches implicit)
  = CoAxiom unique name role tc (toUnbranchedList branches) implicit

coAxiomNumPats :: CoAxiom br -> Int
coAxiomNumPats = length . coAxBranchLHS . (flip coAxiomNthBranch 0)

coAxiomNthBranch :: CoAxiom br -> BranchIndex -> CoAxBranch
coAxiomNthBranch (CoAxiom { co_ax_branches = bs }) index
  = brListNth bs index

coAxiomArity :: CoAxiom br -> BranchIndex -> Arity
coAxiomArity ax index
  = length $ cab_tvs $ coAxiomNthBranch ax index

coAxiomName :: CoAxiom br -> Name
coAxiomName = co_ax_name

coAxiomRole :: CoAxiom br -> Role
coAxiomRole = co_ax_role

coAxiomBranches :: CoAxiom br -> BranchList CoAxBranch br
coAxiomBranches = co_ax_branches

coAxiomSingleBranch_maybe :: CoAxiom br -> Maybe CoAxBranch
coAxiomSingleBranch_maybe (CoAxiom { co_ax_branches = branches })
  | FirstBranch br <- branches
  = Just br
  | otherwise
  = Nothing

coAxiomSingleBranch :: CoAxiom Unbranched -> CoAxBranch
coAxiomSingleBranch (CoAxiom { co_ax_branches = FirstBranch br }) = br

coAxiomTyCon :: CoAxiom br -> TyCon
coAxiomTyCon = co_ax_tc

coAxBranchTyVars :: CoAxBranch -> [TyVar]
coAxBranchTyVars = cab_tvs

coAxBranchLHS :: CoAxBranch -> [Type]
coAxBranchLHS = cab_lhs

coAxBranchRHS :: CoAxBranch -> Type
coAxBranchRHS = cab_rhs

coAxBranchRoles :: CoAxBranch -> [Role]
coAxBranchRoles = cab_roles

coAxBranchSpan :: CoAxBranch -> SrcSpan
coAxBranchSpan = cab_loc

isImplicitCoAxiom :: CoAxiom br -> Bool
isImplicitCoAxiom = co_ax_implicit

coAxBranchIncomps :: CoAxBranch -> [CoAxBranch]
coAxBranchIncomps = cab_incomps

placeHolderIncomps :: [CoAxBranch]
placeHolderIncomps = panic "placeHolderIncomps"

\end{code}

Note [CoAxBranch type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of a CoAxBranch of an associated type-family instance, 
we use the *same* type variables (where possible) as the
enclosing class or instance.  Consider
   class C a b where
     type F x b 
     type F [y] b = ...     -- Second param must be b

   instance C Int [z] where
     type F Int [z] = ...   -- Second param must be [z]

In the CoAxBranch in the instance decl (F Int [z]) we use the
same 'z', so that it's easy to check that that type is the same
as that in the instance header.  

Similarly in the CoAxBranch for the default decl for F in the
class decl, we use the same 'b' to make the same check easy.

So, unlike FamInsts, there is no expectation that the cab_tvs
are fresh wrt each other, or any other CoAxBranch.

Note [CoAxBranch roles]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

  newtype Age = MkAge Int
  newtype Wrap a = MkWrap a

  convert :: Wrap Age -> Int
  convert (MkWrap (MkAge i)) = i

We want this to compile to:

  NTCo:Wrap :: forall a. Wrap a ~R a
  NTCo:Age  :: Age ~R Int
  convert = \x -> x |> (NTCo:Wrap[0] NTCo:Age[0])

But, note that NTCo:Age is at role R. Thus, we need to be able to pass
coercions at role R into axioms. However, we don't *always* want to be able to
do this, as it would be disastrous with type families. The solution is to
annotate the arguments to the axiom with roles, much like we annotate tycon
tyvars. Where do these roles get set? Newtype axioms inherit their roles from
the newtype tycon; family axioms are all at role N.

Note [CoAxiom locations]
~~~~~~~~~~~~~~~~~~~~~~~~
The source location of a CoAxiom is stored in two places in the
datatype tree. 
  * The first is in the location info buried in the Name of the
    CoAxiom. This span includes all of the branches of a branched
    CoAxiom.
  * The second is in the cab_loc fields of the CoAxBranches.  

In the case of a single branch, we can extract the source location of
the branch from the name of the CoAxiom. In other cases, we need an
explicit SrcSpan to correctly store the location of the equation
giving rise to the FamInstBranch.

Note [Implicit axioms]
~~~~~~~~~~~~~~~~~~~~~~
See also Note [Implicit TyThings] in HscTypes
* A CoAxiom arising from data/type family instances is not "implicit".
  That is, it has its own IfaceAxiom declaration in an interface file

* The CoAxiom arising from a newtype declaration *is* "implicit".
  That is, it does not have its own IfaceAxiom declaration in an
  interface file; instead the CoAxiom is generated by type-checking
  the newtype declaration

\begin{code}
instance Eq (CoAxiom br) where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord (CoAxiom br) where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b

instance Uniquable (CoAxiom br) where
    getUnique = co_ax_unique

instance Outputable (CoAxiom br) where
    ppr = ppr . getName

instance NamedThing (CoAxiom br) where
    getName = co_ax_name

instance Typeable br => Data.Data (CoAxiom br) where
    -- don't traverse?
    toConstr _   = abstractConstr "CoAxiom"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "CoAxiom"
\end{code}

%************************************************************************
%*                                                                      *
                    Roles
%*                                                                      *
%************************************************************************

This is defined here to avoid circular dependencies.

\begin{code}

-- See Note [Roles] in Coercion
-- defined here to avoid cyclic dependency with Coercion
data Role = Nominal | Representational | Phantom
  deriving (Eq, Data.Data, Data.Typeable)

pprFullRole :: Role -> SDoc
pprFullRole Nominal          = ptext (sLit "Nominal")
pprFullRole Representational = ptext (sLit "Representational")
pprFullRole Phantom          = ptext (sLit "Phantom")

instance Outputable Role where
  ppr Nominal          = char 'N'
  ppr Representational = char 'R'
  ppr Phantom          = char 'P'

instance Binary Role where
  put_ bh Nominal          = putByte bh 1
  put_ bh Representational = putByte bh 2
  put_ bh Phantom          = putByte bh 3

  get bh = do tag <- getByte bh
              case tag of 1 -> return Nominal
                          2 -> return Representational
                          3 -> return Phantom
                          _ -> panic ("get Role " ++ show tag)

\end{code}