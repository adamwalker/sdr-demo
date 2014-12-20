{-# LANGUAGE RecordWildCards #-}

import Data.Complex
import Control.Monad.Trans.Either
import Foreign.C.Types
import Data.Word

import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic as VG hiding ((++))
import Pipes
import qualified Pipes.Prelude as P
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL (GLfloat)
import Options.Applicative

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.FFT
import SDR.Plot
import SDR.Demod
import SDR.Pulse

import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

--The filter coefficients are stored in another module
import Coeffs

data Options = Options {
    frequency :: Word32
}

parseSize :: ReadM Integer
parseSize = eitherReader $ \arg -> case reads arg of
    [(r, suffix)] -> case suffix of 
        []  -> return r
        "K" -> return $ r * 1000 
        "M" -> return $ r * 1000000
        "G" -> return $ r * 1000000000
        x   -> Left  $ "Cannot parse suffix: `" ++ x ++ "'"
    _             -> Left $ "Cannot parse value: `" ++ arg ++ "'"

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

bufNum     = 1
bufLen     = 16384
samples    = fromIntegral (bufNum * bufLen) `quot` 2
decimation = 8
sqd        = samples `quot` decimation

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

    --Initialize the components that require initialization
    setupGLFW
    str            <- sdrStream frequency 1280000 bufNum bufLen
    rfFFT          <- lift $ fftw samples
    --rfSpectrum     <- return (devnull :: Consumer (VS.Vector GLfloat) IO ())
    --rfSpectrum     <- plotTextureAxes 1024 480 samples samples (centeredAxes 1024 480 104.5 1.28 0.25)
    rfSpectrum     <- plotWaterfall 1024 480 samples 1000 jet_mod 
    audioFFT       <- lift $ fftwReal sqd 
    --audioSpectrum  <- return (devnull :: Consumer (VS.Vector GLfloat) IO ())
    --audioSpectrum  <- plotTexture 1024 768 ((sqd `quot` 2) + 1) 1000
    audioSpectrum  <- plotFillAxes 1024 480 ((sqd `quot` 2) + 1) jet (zeroAxes 1024 480 48 10)
    pulseSink      <- lift $ pulseAudioSink 

    let window0 = hanning samples :: VS.Vector CDouble
        window  = hanning sqd

    --Build the pipeline
    let inputSpectrum ::  Producer (VS.Vector (Complex CDouble)) IO ()
        inputSpectrum =   str 
                      >-> P.map (makeComplexBufferVect samples) 

        spectrumFFTSink ::  Consumer (VS.Vector (Complex CDouble)) IO () 
        spectrumFFTSink =   P.map (VG.zipWith (flip mult) window0 . VG.zipWith mult (fftFixup samples)) 
                        >-> rfFFT 
                        >-> P.map (VG.map ((* (32 / fromIntegral samples)) . realToFrac . magnitude)) 
                        >-> rfSpectrum

        p1 :: Producer (VS.Vector (Complex CDouble)) IO () 
        p1 =  runEffect $   fork inputSpectrum 
                        >-> hoist lift spectrumFFTSink

        demodulated :: Producer (VS.Vector CDouble) IO ()
        demodulated =   p1 
                    >-> decimate decimation (VG.fromList coeffsRFDecim) samples sqd 
                    >-> P.map (fmDemodVec 0) 
                    >-> resample 3 10 (VG.fromList coeffsAudioResampler) sqd sqd 
                    >-> filterr (VG.fromList coeffsAudioFilter) sqd sqd

        audioSpectrumSink :: Consumer (VS.Vector CDouble) IO ()
        audioSpectrumSink =   P.map (VG.zipWith (*) window) 
                          >-> audioFFT 
                          >-> P.map (VG.map ((/ 100) . realToFrac . magnitude)) 
                          >-> audioSpectrum

        p2 :: Producer (VS.Vector CDouble) IO ()
        p2 =  runEffect $   fork demodulated 
                        >-> hoist lift audioSpectrumSink

        audioSink :: Consumer (VS.Vector CDouble) IO ()
        audioSink =   P.map (VG.map ((* 0.2) . realToFrac)) 
                  >-> {-rate sqd >->-} pulseSink

        pipeline :: IO ()
        pipeline = runEffect $ p2 >-> audioSink

    --Run the pipeline
    lift pipeline

main = execParser opt >>= eitherT putStrLn return . doIt
