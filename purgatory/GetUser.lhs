{-
 * Copyright (c) 2008 - 2014 Gabor Greif
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 -}

This is a literate Haskell file.

> {-# LANGUAGE OverloadedLists, TypeFamilies #-}

> import Data.List
> import Test.QuickCheck
> import GHC.Exts
> 
> digits :: Bool -> Int -> [Char] -> [Char]
> digits _ 0 acc = '0' : acc
> digits True 1 acc = acc
> digits False 1 acc = '1' : acc
> digits dr n acc = digits dr (n `quot` 2) $ digits False (n `rem` 2) acc
> 
> dist :: Int -> [Char] -> [Char]
> dist 0 [] = "S"
> dist 0 acc = acc
> dist 1 acc = let r = dist 0 acc in 's' : digits True (length r) r
> dist n acc = dist (n - 1) $ dist 1 acc
> 
> takeLast n ss = drop excess ss
>                   where excess = length ss - n
> 
> test = takeLast 40 $ dist 20 []
> 

Now we need a decoder.

It takes a prefix of an encoded string and returns the
string length together with the number of accesses needed.

> pref' :: [Char] -> (Int, Int)
> pref' "S" = (1, 1)
> pref' ('s':rest) = decode 1 1 1 rest
>   where

Decode regular digits after 's'.

>     decode walk acc n ('0':rest) = decode (walk + 1) (acc * 2) (n + 1) rest
>     decode walk acc n ('1':rest) = decode (walk + 1) (acc * 2 + 1) (n + 1) rest
>     decode walk acc n _ = (walk + acc, n + 1)
> 
> pref' (_:rest) = (1 + res, 1 + n)
>   where (res, n) = pref' rest
> 

> pref :: [Char] -> Int
> pref = fst . pref'


Here come the accompanying tests.

> testcase = dist 10000 []
> testcase20 = dist 20 []
> accessPatterns20 = [(suff, pref' suff) | suff <- tails testcase20, (not . null) suff]
> accesses20 = reverse $ map (snd . snd) accessPatterns20
> 

And a QuickCheck property.

> identityProp n' = n == pref arr
>     where n = abs n' `rem` testcaseLength + 1
>           arr = takeLast n testcase
>           testcaseLength = length testcase
> 

And some more exhaustive ones.

> 
> deepCheck p = quickCheckWith (stdArgs {maxSuccess = 5000}) p
> 

Can we have a speculative random access read first and when that fails
continue with the linear one?

Here is the mapping for the first (speculative) offset

> --offs 'S' = 1
> offs '0' = 1
> offs '1' = 6
> offs 's' = 0

> specul "S" = (1, 1)
> specul (d:rest) | 'S' <- rest !! offs d = (offs d + 2, 2)
> specul all@('s':rest) = pref' all
> specul (d:rest) = (val + offs d + 2, 2 + n)
>   where (val, n) = pref' $ drop (offs d + 1) rest

Handcrafted for now:

> specTest = dist 3 "s0sS"

> accessPatternsSpec = [(suff, specul suff) | suff <- tails specTest, (not . null) suff]
> accessesSpec = reverse $ map (snd . snd) accessPatternsSpec
> accessesSpecSeq = reverse $ map (fst . snd) accessPatternsSpec


Now a decoder for a 3-bit alphabet:
- 'S'        --> full-stop
- 's'        --> stop, start (both unvalued)
- '0' .. '3' --> two-bits
- 'x'        --> two-bits encoding 01 but also being start and stop (both valued)
- 'y'        --> two-bits as start (valued 10) and unvalued stop

Special rules:
- 's' followed by '0' is three bits 100,
- 's', 'x', 'y' followed by any stop has fixed value (special full-stop)

> pref3 "S" = 1
> pref3 ('s':'0':rest) = d3code 2 4 rest
> pref3 ('s':'x':rest) = d3code 2 1 rest
> pref3 ('s':'y':_) = 4
> pref3 ('s':rest) = d3code 1 0 rest
> pref3 ('x':'S':_) = 2
> pref3 ('y':'x':_) = 3
> pref3 ('x':rest) = d3code 1 1 rest
> pref3 ('y':rest) = d3code 1 2 rest
> pref3 (_:rest) = 1 + pref3 rest

Decode regular digits after 's'.

> d3code walk acc ('0':rest) = d3code (walk + 1) (acc * 4) rest
> d3code walk acc ('1':rest) = d3code (walk + 1) (acc * 4 + 1) rest
> d3code walk acc ('2':rest) = d3code (walk + 1) (acc * 4 + 2) rest
> d3code walk acc ('3':rest) = d3code (walk + 1) (acc * 4 + 3) rest
> d3code walk acc ('x':_) = walk + 1 + acc * 4 + 1
> d3code walk acc _ = walk + acc

Hand-made testcase:

> t3 = di3t 1221 "s1x0syxS"
> t3length = length t3

> di3its :: Int -> [Char] -> [Char]
> di3its 0 acc = '0' : acc
> di3its 1 acc = '1' : acc
> di3its 2 acc = '2' : acc
> di3its 3 ('s':eff@('x':_)) = eff
> di3its 3 acc = '3' : acc
> di3its 4 acc = 's' : '0' : acc
> di3its n acc = di3its (n `quot` 4) $ di3its (n `rem` 4) acc
> 
> di3t :: Int -> [Char] -> [Char]
> di3t 0 [] = "S"
> di3t 0 acc = acc
> di3t 1 acc = close follow
>                where close ('1':rest) = 's' : 'x' : rest
>                      close ('2':rest) = 'y' : rest
>                      close all@('s':_) = all
>                      close all = 's' : all
>                      follow = di3its (length r) r
>                      r = di3t 0 acc
> di3t n acc = di3t (n - 1) $ di3t 1 acc


A similar property

> i3Prop n' = n > 0 && n <= t3length ==> n == pref3 arr
>     where n = n' `rem` t3length + 1
>           arr = takeLast n t3

-######################################################################
Here comes a Haskell port of the Omega program tests/WayMarking64.prg #
-######################################################################

Note: the Omega program is the benchmark, and should be changed
      when tweaking the algorithm.

> data TwoBits = OO | OI | IO | II deriving Show

> data Mark = FullStop | Stop TwoBits | Digit TwoBits deriving Show

> data Way = Arrived | Step Mark Way deriving Show

> infixr 5 `Step`

> instance IsList Way where
>   type Item Way = Mark
>   fromList [] = Arrived
>   fromList (x:xs) = Step x $ fromList xs
>   toList Arrived = []
>   toList (m `Step` w) = m : toList w

> etalon = "qrs101qrs30qrs13qrs3rsS"
> -- acc:   55555544455444443332221

> w1,w2,w3,w4,w5,w6,w7,w9,w12,w13,w14,w15 :: Way
> w1 = [FullStop]
> w2 = [Stop OI, FullStop]
> w3 = [Stop IO, Stop OI, FullStop]
> w4 = [Digit II, Stop IO, Stop OI, FullStop]
> w5 = [Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w6 = [Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w7 = [Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w9 = [Digit OI, Digit II, Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w12 = [Stop II, Stop IO, Stop OI, Digit OI, Digit II, Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w13 = [Digit OO, Stop II, Stop IO, Stop OI, Digit OI, Digit II, Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w14 = [Digit II, Digit OO, Stop II, Stop IO, Stop OI, Digit OI, Digit II, Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]
> w15 = [Stop OI, Digit II, Digit OO, Stop II, Stop IO, Stop OI, Digit OI, Digit II, Stop II, Stop IO, Stop OI, Digit II, Stop IO, Stop OI, FullStop]

> countSteps :: Way -> Int
> countSteps way@(_ `Step` _) = howmanysteps 0 way
>   where howmanysteps 0 [] = 0
>         howmanysteps corr [FullStop] = corr + 1
>         howmanysteps corr (Stop OI `Step` way) = pickupMarks (corr + 1) way 0
>         howmanysteps corr (Stop IO `Step` Stop OI `Step` way) = pickupMarks (corr + 2) way 0
>         howmanysteps corr (Stop II `Step` Stop IO `Step` Stop OI `Step` way) = pickupMarks (corr + 3) way 0
>         howmanysteps corr (Digit _ `Step` _ `Step` _ `Step` more) = howmanysteps (corr + 3) more
>         pickupMarks corr [FullStop] acc = corr + 1
>         pickupMarks corr (Stop _ `Step` _) acc = corr + acc
>         pickupMarks corr (Digit OO `Step` more) acc = pickupMarks (corr + 1) more $ 4 * acc
>         pickupMarks corr (Digit OI `Step` more) acc = pickupMarks (corr + 1) more $ 4 * acc + 1
>         pickupMarks corr (Digit IO `Step` more) acc = pickupMarks (corr + 1) more $ 4 * acc + 2
>         pickupMarks corr (Digit II `Step` more) acc = pickupMarks (corr + 1) more $ 4 * acc + 3

> l2w = foldr (Step . c2m) Arrived
>   where c2m 'S' = FullStop
>         c2m 's' = Stop OI
>         c2m 'r' = Stop IO
>         c2m 'q' = Stop II
>         c2m '0' = Digit OO
>         c2m '1' = Digit OI
>         c2m '2' = Digit IO
>         c2m '3' = Digit II


> foldw :: (Mark -> b -> b) -> b -> Way -> b
> foldw _ b [] = b
> foldw f b (m `Step` ms) = f m $ foldw f b ms


> w2l = foldw ((:) . m2c) []
>   where m2c FullStop = 'S'
>         m2c (Stop OI) = 's'
>         m2c (Stop IO) = 'r'
>         m2c (Stop II) = 'q'
>         m2c (Digit OO) = '0'
>         m2c (Digit OI) = '1'
>         m2c (Digit IO) = '2'
>         m2c (Digit II) = '3'


> waylen = foldw (\_ n->n+1) 0

> etalon' = l2w etalon

> countAlongTheWay = foldw tupled [(0,[])]
>   where tupled m l@((_,w):_) = (countSteps (m `Step` w),(m `Step` w)) : l


-- builds a way of length at least 'min'
-- maintains the invariant, that the way starts with 'Stop II'
--

> construct :: Int -> Way -> Way
> construct min acc@(Stop II `Step` _) = if len < min
>                                        then construct min $ (Stop II `Step` Stop IO `Step` Stop OI `Step` prepend len acc)
>                                        else acc
>   where len = countSteps acc
>         prepend 0 acc = acc
>         prepend n acc | n `mod` 4 == 0 = prepend (n `div` 4) (Digit OO `Step` acc)
>         prepend n acc | n `mod` 4 == 1 = prepend (n `div` 4) (Digit OI `Step` acc)
>         prepend n acc | n `mod` 4 == 2 = prepend (n `div` 4) (Digit IO `Step` acc)
>         prepend n acc | n `mod` 4 == 3 = prepend (n `div` 4) (Digit II `Step` acc)


> isDescending l = snd $ foldr (\n (m, b)->(n, b && n == m + 1)) (-1, True) l

> isCorrect min = (isDescending . map fst . countAlongTheWay . construct min) etalon'

