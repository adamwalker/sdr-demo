import Data.Complex
import Control.Monad.Trans.Either
import Pipes
import qualified Pipes.Prelude as P
import Foreign.C.Types
import Foreign.ForeignPtr
import Control.Monad

import Graphics.UI.GLFW as G

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.FFT
import SDR.Plot
import SDR.Demod
import SDR.Pulse
import SDR.Buffer

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
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

    --Initialize the components that require initialization
    str            <- sdrStream 91100000 1280000 bufNum bufLen

    rfDecimator    <- lift $ decimateC decimation coeffsRFDecim samples sqd

    rfFFT          <- lift $ fftw sqd
    rfSpectrum     <- plotSimple sqd (1 / fromIntegral sqd)

    audioResampler <- lift $ resampleR 3 10 coeffsAudioResampler sqd sqd

    audioFilter    <- lift $ filterR coeffsAudioFilter sqd sqd

    audioFFT       <- lift $ fftwReal sqd 
    audioSpectrum  <- plotSimple ((sqd `quot` 2) + 1) (1/100)

    pulseSink      <- lift $ pulseAudioSink sqd

    --Build the pipeline
    let inputSpectrum :: Producer (ForeignPtr (Complex CDouble)) IO ()
        inputSpectrum = str >-> P.mapM (makeComplexBuffer samples) >-> rfDecimator 

        spectrumFFTSink :: Consumer (ForeignPtr (Complex CDouble)) IO () 
        spectrumFFTSink = rfFFT >-> devnull

        p1 :: Producer (ForeignPtr (Complex CDouble)) IO () 
        p1 = runEffect $ fork inputSpectrum >-> hoist lift spectrumFFTSink

        demodulated :: Producer (ForeignPtr CDouble) IO ()
        demodulated = p1 >-> fmDemod sqd >-> audioResampler >-> audioFilter

        audioSpectrumSink :: Consumer (ForeignPtr CDouble) IO ()
        audioSpectrumSink = audioFFT >-> audioSpectrum

        p2 :: Producer (ForeignPtr CDouble) IO ()
        p2 = runEffect $ fork demodulated >-> hoist lift audioSpectrumSink

        audioSink :: Consumer (ForeignPtr CDouble) IO ()
        audioSink = P.mapM (multiplyConstFF sqd 0.2) >-> P.mapM (doubleToFloat sqd) >-> rate sqd >-> pulseSink

        pipeline :: IO ()
        pipeline = runEffect $ p2 >-> audioSink

    --Run the pipeline
    lift pipeline

