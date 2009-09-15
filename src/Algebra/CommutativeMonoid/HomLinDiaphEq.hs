-- Homogeneous Linear Diaphantine Equation solver
--
-- Copyright (C) 2009 John D. Ramsdell
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- |
-- Module      : Algebra.CommutativeMonoid.HomLinDiaphEq
-- Copyright   : (C) 2009 John D. Ramsdell
-- License     : GPL
--
-- Homogeneous Linear Diaphantine Equation solver.
--
-- The solver uses the algorithm of Contejean and Devie as specified
-- by David Papp and Bela Vizari in \"Effective Solutions of Linear
-- Diophantine Equation Systems with an Application to Chemistry\",
-- Rutcor Research Report RRR 28-2004, September, 2004,
-- <http://rutcor.rutgers.edu/pub/rrr/reports2004/28_2004.ps>, after
-- modification so as to ensure every basis vector is considered.
--
-- The algorithm for systems of homogeneous linear Diophantine
-- equations follows.  Let e[k] be the kth basis vector for 1 <= k <=
-- n.  To find the minimal, non-negative solutions M to the system of
-- equations sum(i=1,n,a[i]*v[i]) = 0, the algorithm of Contejean and
-- Devie is:
--
--  1. [init] A := {e[k] | 1 <= k <= n}; M := {}
--
--  2. [new minimal results] M := M + {a in A | a is a solution}
--
--  3. [unnecessary branches] A := {a in A | all m in M : some
--     1 <= k <= n : m[k] < a[k]}
--
--  4. [test] If A = {}, stop
--
--  5. [breadth-first search] A := {a + e[k] | a in A, 1 <= k <= n,
-- \<sum(i=1,n,a[i]*v[i]),v[k]> \< 0}; go to step 2

module Algebra.CommutativeMonoid.HomLinDiaphEq (homLinDiaphEq) where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as S

{-- Debugging hack
import System.IO.Unsafe

z :: Show a => a -> b -> b
z x y = seq (unsafePerformIO (print x)) y
--}

type Vector a = Array Int a

vector :: Int -> [a] -> Vector a
vector n elems =
    listArray (0, n - 1) elems

-- | The 'homLinDiaphEq' function takes a list of integers that
-- specifies a homogeneous linear Diophantine equation, and returns
-- the equation's minimal, non-negative solutions.
homLinDiaphEq :: [Int] -> [[Int]]
homLinDiaphEq [] = []
homLinDiaphEq v =
    newMinimalResults (vector n v) (basis n) S.empty
    where n = length v

-- Construct the basis vectors for an n-dimensional space
basis :: Int -> Set (Vector Int)
basis n =
    foldl (flip S.insert) S.empty
              [ z // [(k, 1)] |
                k <- indices z ]
    where z = vector n $ replicate n 0

-- The main loop has been reorganized to ensure every basis vector is
-- considered.  The breadth-first search step is now the last step.

-- Add elements of a that solve the equation to m and the output
newMinimalResults :: Vector Int -> Set (Vector Int) ->
                     Set (Vector Int) -> [[Int]]
newMinimalResults v a m =
    loop m (S.toList a)         -- Test each element in a
    where
      loop m [] =
          nextSearch v a m      -- Generate new a and try again
      loop m (x:xs)
           | prod v x == 0 && S.notMember x m =
               elems x:loop (S.insert x m) xs -- Answer found
           | otherwise =
               loop m xs

-- Generate the next set of test vectors--if there aren't any, your done
nextSearch :: Vector Int -> Set (Vector Int) ->
              Set (Vector Int) -> [[Int]]
nextSearch v a m =
    if S.null a' then
        []
    else
        newMinimalResults v (breadthFirstSearch v a') m
    where
      a' = unnecessaryBranches a m

-- Remove unnecessary branches.  A test vector is not necessary if all
-- of its elements are greater than or equal to the elements of some
-- minimal solution.
unnecessaryBranches :: Set (Vector Int) -> Set (Vector Int) -> Set (Vector Int)
unnecessaryBranches a m =
    S.filter f a
    where
      f x = all (g x) (S.toList m)
      g x y = not (lessEq y x)

-- Compare vectors element-wise.
lessEq :: Vector Int -> Vector Int -> Bool
lessEq x y =
    all (\i-> x!i <= y!i) (indices x)

-- Breadth-first search using the algorithm of Contejean and Devie
breadthFirstSearch :: Vector Int -> Set (Vector Int) -> Set (Vector Int)
breadthFirstSearch v a =
    S.fold f S.empty a
    where
      f x acc =
          foldl (flip S.insert) acc
            [ x // [(k, x!k + 1)] |
              k <- indices x,
              prod v x * v!k < 0 ] -- Contejean-Devie contribution

-- Inner product
prod :: Vector Int -> Vector Int -> Int
prod x y =
    sum [ x!i * y!i | i <- indices x ]
