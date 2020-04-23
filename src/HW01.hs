module HW01 where

{-

Name:

Jace Laquerre

Collaboration Statement:

I discussed the ideas of case statments and recursive calls
with Chris McCabe for problems 2 & 3

Collaboration on high-level ideas and approach on assignments is
encouraged. Copying someone else's work is not allowed. Copying solutions
from an online source is not allowed. Any collaboration or online
resources, even if used only a small amount, must be declared in your
assignment when you submit it. For example: “I discussed high-level
strategies for solving problem 2 and 5 with Alex; I found this
stackoverflow post (<link>) helpful while working on problem 3.” Students
caught copying work are eligible for immediate failure of the course and
disciplinary action by the University. All academic integrity misconduct
will be treated according to UVM's Code of Academic Integrity.

-}

import Util.Testing

import Data.Char

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ========== --
-- ASSIGNMENT --
-- ========== --

-- For your reference, here are three functions written in Haskell:
-- `factorial`, `dropCommonPrefix` and `swapFirstTwoChars`. Examples are
-- written with [XN] in front for “example N”, and homework problems are marked
-- with [EN] in front for “exercise N”. Your job is to fill in all function
-- definitions which currently are `error "TODO"`.

----------
-- [X1] --
----------

-- Here is the factorial function in Haskell, E.g.:
--
--     factorial 4
--     ==
--     4 * 3 * 2 * 1 * 1
--     ==
--     24
--
-- Note the syntax for function calls. In math and most other languages you
-- would write `factorial(x) = ...`  for defining factorial, and `factorial(5)`
-- for calling the function with the argument 5. However, in Haskell we write
-- `factorial x = ...` for defining factorial, and `factorial 5` (note the
-- space in between) for calling the function with the argument 5. That is,
-- instead of parentheses for function definitions and function calls, we just
-- separate function names by arguments. For multiple arguments, this
-- generalizes to `f x y z` in Haskell for the usual syntax of `f(x, y, z)`
--
-- the name  the type
-- of the    of the
-- function  argument
-- ↓↓↓↓↓↓    ↓↓↓
factorial :: Int -> Int
--        ↑↑        ↑↑↑
--        type      the return
--        decl.     type
--
--        name of the
--        argument
--        to the
--        function
--        ↓
factorial n =
-- ↑↑↑↑↑↑
-- the name of the function
--
-- (Note the following must be indented by at least one space for Haskell to
-- accept it; Haskell is whitespace sensitive.)
--
-- test whether or not the input `n` is less than or equal to zero
  if n <= 0
    -- if n <= 0 then return 1
    then 1
    -- if n > 0 then...
    else
      -- recursively call factorial on (n - 1)
      -- give a name to the result of the recursive call: `rc`
      let rc = factorial (n - 1)
      -- multiply n with the result of the recursive call (`rc`)
      in n * rc

-- Here is the test suite for factorial
testFactorial :: Test
testFactorial = Test1
  -- the example or exercise
  ( "X1"
  , "factorial"
  , factorial
  --      ↓ expected output for first test
  , [ (0, 1)
  --   ↑ test input for first test
  --   ↓ test input for second test
    , (1, 1)
  --      ↑ expected output for second test
    , (2, 2)
    , (3, 6)
    , (4, 24)
    , (5, 120)
    ]
  )

----------
-- [X2] --
----------

-- Here is a function that drops a common prefix between two lists, E.g.:
--
--     dropCommonPrefix [1, 2, 3, 4, 5, 6] [1, 2, 3, 11, 12, 13]
--     ==
--     ([4, 5, 6], [11, 12, 13])
--
--
--                                    the return type:
--                  first function    a pair of lists
--                  argument type     of integers
--                  ↓↓↓↓↓             ↓↓↓↓↓↓↓↓↓↓↓↓↓↓
dropCommonPrefix :: [Int] -> [Int] -> ([Int], [Int])
--                           ↑↑↑↑↑
--                           second function
--                           argument type
--               name for
--               first argument
--               ↓↓
dropCommonPrefix xs ys =
  --                ↑↑
  --                name for
  --                second argument
  -- find out if `xs` is the empty list or a cons cell
  case xs of
    -- `xs` is the empty list; there is no common prefix, so return `xs` (=[])
    -- and `ys` as is.
    [] -> ([], ys)
    -- `xs` is a cons cell
    -- - `x` is bound to the first element in the list `xs`.
    -- - `xs'` is bound to the remainder of the list `xs`: `xs` without the
    --    first element `x`.
    x:xs' ->
      -- find out if `ys` is the empty list or a cons cell
      case ys of
      -- `ys` is the empty list; there is no common prefix, so return `xs`
      -- (=x:xs') and `ys` (=[]) as is
      [] -> (x:xs', ys)
      -- `ys` is a cons cell
      -- - `y` is bound to the first element in the list `ys`.
      -- - `ys'` is bound to the remained of the list `ys`: `ys` without the
      --   first element `y`.
      y:ys' ->
        -- test if `x` and `y` are equal; if so we want to drop them from the
        -- result lists.
        if x == y
          -- `x` is equal to `y`, so don't include them in the result; just
          -- call `dropCommonPrefix` recursively on `xs'` and `ys'`.
          then dropCommonPrefix xs' ys'
          -- `x` is not equal to `y`; we are done dropping common prefix
          -- elements, so just return a pair of `xs` and `ys` as is.
          --
          --   construct a new list
          --   with `x` as the first element
          --   and `xs'` as the tail (rest)
          --   of the list
          --   ↓↓↓↓↓↓
          else (x:xs', y:ys')
          --           ↑↑↑↑↑
          --           construct a new list
          --           with `y` as the first element
          --           and `ys'` as the tail (rest)
          --           of the list


testDropCommonPrefix :: Test
testDropCommonPrefix = Test2
  ( "X2"
  , "dropCommonPrefix"
  , dropCommonPrefix
  --               test input
  --               second arg
  --               ↓↓↓↓↓↓↓↓↓↓
  , [ (([1, 2, 3], [1, 2, 10]), ([3], [10]))
  --    ↑↑↑↑↑↑↑↑↑                ↑↑↑↑↑↑↑↑↑
  --    test input               expected
  --    first arg                output
    , (([1, 2, 3], [4, 5, 6]), ([1, 2, 3], [4, 5, 6]))
    ]
  )

----------
-- [X3] --
----------

-- Here is a function that swaps the first two characters in a string, and
-- leaves the string alone if it is shorter than two characters.

--                   A string is
--                   just a list
--                   of chars
--                   ↓↓↓↓↓↓
swapFirstTwoChars :: [Char] -> [Char]
--                the name of the
--                function parameter
--                ↓↓
swapFirstTwoChars cs =
  -- find out if the string is empty, or has at least one character
  case cs of
    -- cs is empty; return an empty string (i.e., the empty list)
    [] -> []
    -- cs has at least one character, which we have bound to the variable `c1`;
    -- the rest of the string (without the first character) is bound to `cs'`.
    c1:cs' ->
      -- find out if the rest of the string (`cs'`) is empty, or has at least
      -- one character
      case cs' of
        -- cs' is empty; return a string with only one character inside: `c1`
        [] -> c1:[]
        -- cs' has at least one character, which we have bound to the variable
        -- `c2`; the rest of the string (the original string without its first
        -- two characters) is bound to `cs''`.
        c2:cs'' ->
          -- return a new string which has `c2` at the front, `c1` as the
          -- second character, and the rest of the string (`cs''`)
          c2:c1:cs''

testSwapFirstTwoChars :: Test
testSwapFirstTwoChars = Test1
  ( "X3"
  , "swapFirstTwoChars"
  , swapFirstTwoChars
  , [ ("hello", "ehllo")
    , ("Walter", "aWlter")
    , ("Haskell", "aHskell")
    ]
  )

---------------
-- [E1]: ★☆☆ --
---------------

-- Your first exercise: define a function which cubes a number: multiplies a
-- number by its self three times. In math:
--
--     multiply(x) == xxx == x³
--
-- Feel free to implement with two multiplication operations.
--
-- Tests are provided, but please write tests of your own (at least two).

cubed :: Int -> Int
cubed x =
  if x == 0
    then 0
    else x * x * x

testCubed :: Test
testCubed = Test1
  ( "E1"
  , "cubed"
  , cubed
  , [ (2, 8)
    , (3, 27)
    , (4, 64)
    , (7, 343)
    , (12, 1728)
    ]
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Your second exercise: define a function which returns the common prefix of
-- two lists of numbers. In math:
--
--     commonPrefix(xs,ys)
--     ==
--     the longest zs such that xs = zs++xs' and ys = zs++ys' for some xs' and
--     ys'
--
-- I suggest looking at the implementation of `X2` earlier in this file, which
-- drops the common prefix of two lists.
--
-- Tests are provided, but please write tests of your own (at least two).

commonPrefix :: [Int] -> [Int] -> [Int]
commonPrefix xs ys =
  case xs of
    [] -> []
    x:xs' ->
      case ys of
      [] -> []
      y:ys' ->
        if x == y
          then x:commonPrefix xs' ys'
          else []



testCommonPrefix :: Test
testCommonPrefix = Test2
  ( "E2"
  , "commonPrefix"
  , commonPrefix
  , [ (([1, 2, 3], [1, 2, 10]), [1, 2])
    , (([1, 2, 3], [4, 5, 6]), [])
    , (([1, 2, 3, 4], [1, 2, 3, 1]), [1, 2, 3])
    , (([1, 2, 4, 6, 9], [1, 2, 4, 6, 7]), [1, 2, 4, 6])
    , (([1, 4, 5, 5], [1, 7]), [1])
    ]
  )

---------------
-- [E3]: ★★★ --
---------------

-- Your third exercise: define a function which turns a string into “sarcasm
-- case”. Sarcasm case is the process of capitalizing every character, starting
-- with the first letter not capitalized. Examples (also included later as
-- tests):
--
--     sarcasmCase("hello") == "hElLo"
--     sarcasmCase("haskell is now my favorite language") == "hAsKeLl iS NoW My fAvOrItE LaNgUaGe"
--
-- You should use the `toLower` function which has type:
--
--     toUpper :: Char -> Char
--
-- and which converts a letter to uppercase if it is a lowercase latin
-- character [a..z], and returns the input unmodified otherwise.
--
-- You should define this function by recursion, starting with a case analysis
-- on `cs`, testing if it is the empty list or a cons cell. In the recursive
-- case, you will want to do another case analysis, testing if it is the empty
-- list or a cons cell. In the last case you will do a recursive call to
-- `sarcamCase` with the tail of the tail of the initial list. It will look
-- something like this:
--
--     case cs of
--       [] -> ...(1)
--       c1:cs' -> case cs' of
--         [] -> ...(2)
--         c2:cs'' -> ...(3)
--
--  (1) is the case where the string is empty, so just return the empty list
--  (2) is the case where the string is a single character, so just return the
--      character in a single character string
--  (3) is the case where the string is at least two characters long, and `c1`
--      and `c2` are bound to the first two characters of the list. You will
--      want to call `toUpper` on `c2` to convert it to upper case in
--      constructing the final list. The recursive call is `sarcasmCase cs''`
--      which will convert the tail of the list to sarcasm case. Return as the
--      result the result of the recursive call, with c1 and the capitalized
--      version of c2 in front, like so:
--
--          c1 : <capitalzied c2> : <sarcasm case of cs''>
--
--             A string in Haskell
--             is just a list of
--             characters
--             ↓↓↓↓↓↓
sarcasmCase :: [Char] -> [Char]
sarcasmCase cs =
  case cs of
    [] -> []
    c1:cs' -> case cs' of
      [] -> c1:[]
      c2:cs'' -> c1 : (toUpper c2) : (sarcasmCase cs'')

testSarcasmCase :: Test
testSarcasmCase = Test1
  ( "E3"
  , "sarcasmCase"
  , sarcasmCase
  , [ ("hello", "hElLo")
    , ("haskell is now my favorite language", "hAsKeLl iS NoW My fAvOrItE LaNgUaGe")
    , ("this class is cool", "tHiS ClAsS Is cOoL")
    , ("walter", "wAlTeR")
    ]
  )

main :: IO ()
main = runTests
  [ testFactorial
  , testDropCommonPrefix
  , testSwapFirstTwoChars
  , testCubed
  , testCommonPrefix
  , testSarcasmCase
  ]
