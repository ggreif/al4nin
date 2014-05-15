

{-# LANGUAGE DataKinds #-}


-- Metadata Arrays (MAs)
-- | Header | El m ... El n-1 | padding_opt
-- where the Header might be another (smaller) MA
-- General rule: to each range El x .. El x+k
--   there are a few (shared) bits in the Header
--   k may be different for each MA
--   the position of the Header bits can be calculated
--     from the address of (El x) by masking and shifting
--   the alignment of the MA is determined by the maskings


-- Examples: Chunky-lists for unboxed 32-bit data (head-strict, tail-lazy)
--   Header: 30 bits = 15x2 {thunk, empty, head-head, head-lazy}
--   El: 15x32bits
-- GC can normalize empty's

data {-kind-} Area = Meta

data MetadataArray = MA { mask2header :: Nat, density :: Nat, a :: *, extractor :: a -> Nat -> a }

-- means: zero out mask2header bits of the address to get the Header address
--        take the LSB mask2header bits and shift to the right by density bits
--        to extract the metadata, then feed that chunk of metadata into the
--        extractor, also passing the LSB density bits.

-- TODO: 1) use ST monad to implemente mutable heap populated by such MAs
--       2) encode MetadataArray at the type level, using classes to support extraction
