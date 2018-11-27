module Main where

import           Isumi.Math.FFT.Bruun
import           Isumi.Math.FFT.MatLab

import           Control.Monad.Random.Class
import           Criterion.Main
import           Criterion.Types
import           Data.Complex
import qualified Data.Vector.Unboxed         as UV
import Control.DeepSeq(NFData)

main :: IO ()
main = do
  realInputs <- sampleInput
  complexInputs <- sampleInputC
  defaultMainWith config [
      bgroup "real input" [
        bgroup "fftDirect" $
          genBenchs (\xs -> fftDirect $ UV.map (:+0) xs) realInputs
        ,
        bgroup "fftBruun" $
          genBenchs fftBruun realInputs
      ],
      bgroup "complex input" [
        bgroup "fftDirect" $
          genBenchs fftDirect complexInputs
        ,
        bgroup "fftBruun" $
          genBenchs fftBruunC complexInputs
      ]
    ]

config = defaultConfig
  { reportFile = Just "dist/bench_report.html"
  }

genBenchs :: (UV.Unbox a, NFData b)
          => (UV.Vector a -> b) -> [UV.Vector a] -> [Benchmark]
genBenchs f = fmap (\xs -> bench (show . UV.length $ xs) $ nf f xs)

sampleInputC :: MonadRandom m => m [UV.Vector (Complex Double)]
sampleInputC = do
  reals <- sampleInput
  comps <- sampleInput
  pure $ zipWith (UV.zipWith (:+)) reals comps

sampleInput :: MonadRandom m => m [UV.Vector Double]
sampleInput = do
  flip traverse [0..11] $ \e -> do
    xs <- take (2^e) <$> getRandoms
    pure $ UV.fromList xs

