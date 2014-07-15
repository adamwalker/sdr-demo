module Coeffs where

import Data.Complex
import Foreign.C.Types

{-
  In Octave:
  pkg load signal
  a = [1 1 0 0]
  f = [0 0.08 0.125 1]
  remez(50, f, a)
-}

coeffsRFDecim :: [Complex CDouble]
coeffsRFDecim = [

   0.0252935,
   0.0077179,
   0.0076296,
   0.0065592,
   0.0044613,
   0.0014128,
  -0.0024132,
  -0.0067080,
  -0.0110463,
  -0.0149643,
  -0.0179032,
  -0.0193486,
  -0.0188277,
  -0.0159704,
  -0.0105863,
  -0.0026480,
   0.0076515,
   0.0199119,
   0.0335666,
   0.0478808,
   0.0620109,
   0.0751344,
   0.0863093,
   0.0949120,
   0.1003152,
   0.1021502,
   0.1003152,
   0.0949120,
   0.0863093,
   0.0751344,
   0.0620109,
   0.0478808,
   0.0335666,
   0.0199119,
   0.0076515,
  -0.0026480,
  -0.0105863,
  -0.0159704,
  -0.0188277,
  -0.0193486,
  -0.0179032,
  -0.0149643,
  -0.0110463,
  -0.0067080,
  -0.0024132,
   0.0014128,
   0.0044613,
   0.0065592,
   0.0076296,
   0.0077179,
   0.0252935

    ]

{-
  In Octave:
  pkg load signal
  a = [1 1 0 0]
  f = [0 0.1 0.3 1]
  remez(30, f, a)
-}

coeffsAudioResampler :: [CDouble]
coeffsAudioResampler = [

  -3.0862e-04,
   1.9752e-03,
   4.1096e-03,
   5.8306e-03,
   4.9003e-03,
  -4.7097e-04,
  -1.0215e-02,
  -2.1264e-02,
  -2.7609e-02,
  -2.2054e-02,
   7.7070e-04,
   4.1331e-02,
   9.3585e-02,
   1.4595e-01,
   1.8477e-01,
   1.9910e-01,
   1.8477e-01,
   1.4595e-01,
   9.3585e-02,
   4.1331e-02,
   7.7070e-04,
  -2.2054e-02,
  -2.7609e-02,
  -2.1264e-02,
  -1.0215e-02,
  -4.7097e-04,
   4.9003e-03,
   5.8306e-03,
   4.1096e-03,
   1.9752e-03,
  -3.0862e-04
    ]

{-
  In Octave:
  pkg load signal
  a = [1 1 0 0]
  f = [0 0.3125 0.39 1]
  remez(50, f, a)
-}

coeffsAudioFilter :: [CDouble]
coeffsAudioFilter = [

   1.4300e-03,
   7.7910e-03,
   3.6881e-04,
  -3.9208e-03,
  -5.7843e-03,
  -5.4785e-04,
   7.0511e-03,
   7.9887e-03,
  -9.8260e-04,
  -1.1368e-02,
  -1.0366e-02,
   4.0556e-03,
   1.7507e-02,
   1.2702e-02,
  -9.6383e-03,
  -2.6509e-02,
  -1.4798e-02,
   1.9937e-02,
   4.1280e-02,
   1.6460e-02,
  -4.2208e-02,
  -7.3911e-02,
  -1.7528e-02,
   1.2712e-01,
   2.8367e-01,
   3.5123e-01,
   2.8367e-01,
   1.2712e-01,
  -1.7528e-02,
  -7.3911e-02,
  -4.2208e-02,
   1.6460e-02,
   4.1280e-02,
   1.9937e-02,
  -1.4798e-02,
  -2.6509e-02,
  -9.6383e-03,
   1.2702e-02,
   1.7507e-02,
   4.0556e-03,
  -1.0366e-02,
  -1.1368e-02,
  -9.8260e-04,
   7.9887e-03,
   7.0511e-03,
  -5.4785e-04,
  -5.7843e-03,
  -3.9208e-03,
   3.6881e-04,
   7.7910e-03,
   1.4300e-03
    ]
