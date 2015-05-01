{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Complex
import Control.Monad.Trans.Either
import Foreign.C.Types
import Data.Word

import qualified Data.Vector.Storable as VS hiding ((++))
import qualified Data.Vector.Generic  as VG hiding ((++))
import           Pipes
import qualified Pipes.Prelude        as P
import           Options.Applicative

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.FFT
import SDR.Plot
import SDR.Demod
import SDR.Pulse
import SDR.PipeUtils
import SDR.ArgUtils

import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

--The filter coefficients are stored in another module
import Coeffs

data Options = Options {
    frequency :: Word32
}

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "SDR library demo. Receive FM broadcast band radio." <> header "SDR demo")

samples      = 8192
audioSamples = 1024

{-
    sampling frequency of the input is 1280 khz
    this is decimated by a factor of 8 and then demodulated
    sampling frequency of demodulated signal is 160 khz
    need audio output at 48 khz
    resampling factor is 48/160 == 3/10
    FM pilot tone at 19khz (0.3958 * 48)
    start audio filter cutoff at 15khz (0.3125 * 48)
-}

doIt Options{..} = do

    --Initialize GLFW
    res            <- lift setupGLFW
    unless res (left "Unable to initilize GLFW")

    --Initialize the RTLSDR device
    str            <- sdrStream frequency 1280000 1 (fromIntegral samples * 2)

    --Initialize the graphs and FFTs
    rfFFT          <- lift $ fftw samples
    rfSpectrum     <- plotWaterfall 1024 480 samples 1000 jet_mod 
    audioFFT       <- lift $ fftwReal audioSamples 
    audioSpectrum  <- plotFillAxes 1024 480 ((audioSamples `quot` 2) + 1) jet (zeroAxes 1024 480 48 10)

    --Initialize the sound sink
    pulseSink      <- lift pulseAudioSink 

    --Initialize the filters
    deci <- lift $ fastDecimatorC 8 coeffsRFDecim 
    resp <- lift $ haskellResampler 3 10 coeffsAudioResampler
    filt <- lift $ fastSymmetricFilterR  coeffsAudioFilter

    let window0 = hanning samples :: VS.Vector Float
        window  = hanning samples

    --Build the pipeline
    lift $ runEffect $   str
                     >-> P.map convertCAVX
                     >-> foldl1 combine [

                            P.map (VG.zipWith (flip mult) window0 . VG.zipWith mult (fftFixup samples)) 
                        >-> P.map (VG.map (cplxMap (realToFrac :: Float -> CDouble)))
                        >-> rfFFT 
                        >-> P.map (VG.map ((* (32 / fromIntegral samples)) . realToFrac . magnitude)) 
                        >-> rfSpectrum,

                            firDecimator deci samples 
                        >-> P.map (fmDemodVec 0) 
                        >-> firResampler resp samples
                        >-> firFilter filt audioSamples
                        >-> foldl1 combine [

                                P.map (VG.zipWith (*) window) 
                            >-> P.map (VG.map (realToFrac :: Float -> CDouble))
                            >-> audioFFT 
                            >-> P.map (VG.map ((/ 10) . realToFrac . magnitude)) 
                            >-> audioSpectrum,

                                P.map (VG.map ((* 0.2) . realToFrac)) 
                            >-> pulseSink
                        ]
                    ]

main = execParser opt >>= eitherT putStrLn return . doIt

