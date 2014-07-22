import Data.Complex
import Control.Monad.Trans.Either
import Foreign.C.Types

import Data.Vector.Storable as VS
import Data.Vector.Generic as VG
import Pipes
import qualified Pipes.Prelude as P
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL (GLfloat)

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

main = eitherT putStrLn return $ do

    --Initialize the components that require initialization
    setupGLFW
    str            <- sdrStream 104500000 1280000 bufNum bufLen
    rfFFT          <- lift $ fftw sqd
    rfSpectrum     <- return (devnull :: Consumer (VS.Vector GLfloat) IO ())
    --rfSpectrum     <- plotTexture 1024 768 sqd sqd --jet (4 / fromIntegral sqd)
    audioFFT       <- lift $ fftwReal sqd 
    audioSpectrum  <- return (devnull :: Consumer (VS.Vector GLfloat) IO ())
    --audioSpectrum  <- plotWaterfall 1024 768 ((sqd `quot` 2) + 1) 800 jet_mod 
    pulseSink      <- lift $ pulseAudioSink 

    let window        = hanning sqd

    --Build the pipeline
    let inputSpectrum :: Producer (VS.Vector (Complex CDouble)) IO ()
        inputSpectrum = str >-> P.map (makeComplexBufferVect samples) >-> decimate decimation (VG.fromList coeffsRFDecim) samples sqd

        spectrumFFTSink :: Consumer (VS.Vector (Complex CDouble)) IO () 
        spectrumFFTSink = P.map (VG.zipWith (flip mult) window . VG.zipWith mult (fftFixup sqd)) >-> rfFFT >-> P.map (VG.map ((* (4 / fromIntegral sqd)) . realToFrac . magnitude)) >-> rfSpectrum

        p1 :: Producer (VS.Vector (Complex CDouble)) IO () 
        p1 = runEffect $ fork inputSpectrum >-> hoist lift spectrumFFTSink

        demodulated :: Producer (VS.Vector CDouble) IO ()
        demodulated = p1 >-> P.map (fmDemodVec 0) >-> resample 3 10 (VG.fromList coeffsAudioResampler) sqd sqd >-> filterr (VG.fromList coeffsAudioFilter) sqd sqd

        audioSpectrumSink :: Consumer (VS.Vector CDouble) IO ()
        audioSpectrumSink = P.map (VG.zipWith (*) window) >-> audioFFT >-> P.map (VG.map ((/ 100) . realToFrac . magnitude)) >-> audioSpectrum

        p2 :: Producer (VS.Vector CDouble) IO ()
        p2 = runEffect $ fork demodulated >-> hoist lift audioSpectrumSink

        audioSink :: Consumer (VS.Vector CDouble) IO ()
        audioSink = P.map (VG.map ((* 0.2) . realToFrac)) >-> rate sqd >-> pulseSink

        pipeline :: IO ()
        pipeline = runEffect $ p2 >-> audioSink

    --Run the pipeline
    lift pipeline

