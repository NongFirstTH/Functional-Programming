import System.Random

nRandomRs :: (RandomGen g, UniformRange a, Integral n)
    => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs (minVal,maxVal) n gen =
  let (val, gen') = uniformR (minVal, maxVal) gen
      (rest, gen'') = nRandomRs (minVal, maxVal) (n-1) gen'
  in (val:rest, gen'')