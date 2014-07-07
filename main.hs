import Data.Complex
import Control.Monad.Trans.Either
import Pipes
import qualified Pipes.Prelude as P
import Foreign.C.Types
import Foreign.ForeignPtr
import Control.Monad

import Data.Vector.Storable as VS
import Data.Vector.Generic as VG

import Graphics.UI.GLFW as G

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.FFT
import SDR.Plot
import SDR.Demod
import SDR.Pulse
import SDR.Buffer

import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

import Coeffs

bufNum = 1
bufLen = 16384
samples = fromIntegral (bufNum * bufLen) `quot` 2
decimation = 8
sqd = samples `quot` decimation

main = eitherT putStrLn return $ do

    --sampling frequency of input is 1280 khz
    --sampling frequency of fm demodulated signal is 160 khz
    --resampling factor is 48/160
    --resampling factor is 3/10
    --audio frequency cutoff is 19khz (0.3958)
    --start cutoff at 15khz (0.3125)

    --initialize glfw 
    setupGLFW

    --Initialize the components that require initialization
    str            <- sdrStream 102100000 1280000 bufNum bufLen

    rfDecimator    <- lift $ decimate decimation (VG.fromList coeffsRFDecim) samples sqd

    --rfFFT          <- lift $ fftw sqd
    rfSpectrum     <- plotFillAxes 1024 768 sqd jet --(4 / fromIntegral sqd)

    audioResampler <- lift $ resample 3 10 (VG.fromList coeffsAudioResampler) sqd sqd

    audioFilter    <- lift $ filterr (VG.fromList coeffsAudioFilter) sqd sqd

    --audioFFT       <- lift $ fftwReal sqd 
    --audioSpectrum  <- plotWaterfall 1024 768 ((sqd `quot` 2) + 1) 800 jet_mod 

    pulseSink      <- lift $ pulseAudioSink 

    --let window = hanning sqd
    let z ** (x :+ y) = (x * z) :+ (y * z)
    let devnull2 :: Consumer (VS.Vector CDouble) IO ()
        devnull2 = devnull
    let devnull3 :: Consumer (VS.Vector (Complex CDouble)) IO ()
        devnull3 = devnull

    --Build the pipeline
    let inputSpectrum :: Producer (VS.Vector (Complex CDouble)) IO ()
        --inputSpectrum = str >-> P.mapM (makeComplexBuffer samples) >-> P.map (flip VS.unsafeFromForeignPtr0 sqd) >-> rfDecimator 
        inputSpectrum = str >-> P.map (flip VS.unsafeFromForeignPtr0 (fromIntegral $ bufNum * bufLen)) >-> P.map (makeComplexBufferVect samples) >-> rfDecimator 

        spectrumFFTSink :: Consumer (VS.Vector (Complex CDouble)) IO () 
        spectrumFFTSink = devnull --P.map (VG.zipWith (**) window . VG.zipWith (**) (fftFixup sqd)) >-> rfFFT >-> P.map (VG.map ((* (4 / fromIntegral sqd)) . realToFrac . magnitude)) >-> devnull2 --rfSpectrum

        p1 :: Producer (VS.Vector (Complex CDouble)) IO () 
        p1 = runEffect $ fork inputSpectrum >-> hoist lift spectrumFFTSink

        demodulated :: Producer (VS.Vector CDouble) IO ()
        --demodulated = p1 >-> P.map (fst . VS.unsafeToForeignPtr0) >-> fmDemod sqd >-> P.map (flip VS.unsafeFromForeignPtr0 sqd) >-> audioResampler >-> audioFilter
        demodulated = p1 >-> P.map (fmDemodVec 0) >-> audioResampler >-> audioFilter

        audioSpectrumSink :: Consumer (VS.Vector CDouble) IO ()
        audioSpectrumSink = devnull --P.map (VG.zipWith (*) window) >-> audioFFT >-> P.map (VG.map ((/ 100) . realToFrac . magnitude)) >-> devnull2 --audioSpectrum

        p2 :: Producer (VS.Vector CDouble) IO ()
        p2 = runEffect $ fork demodulated >-> hoist lift audioSpectrumSink

        audioSink :: Consumer (VS.Vector CDouble) IO ()
        audioSink = P.map (VG.map ((* 0.2) . realToFrac)) >-> rate sqd >-> pulseSink

        pipeline :: IO ()
        pipeline = runEffect $ p2 >-> audioSink

    --Run the pipeline
    lift pipeline

