{-# LANGUAGE NoImplicitPrelude, CPP #-}
module IHaskell.Display.Charts (sizeRenderable) where

import ClassyPrelude

import System.Directory
import Data.Default.Class
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import IHaskell.Display

instance IHaskellDisplay (Renderable a) where
  display renderable = do
    
    let (defaultWid, defaultHeight) = (450,300)

    pngDisp <- chartData defaultWid defaultHeight renderable PNG

    -- We can add `svg svgDisplay` to the output of `display`,
    -- but SVGs are not resizable in the IPython notebook.
    svgDisp <- chartData 450 300 renderable SVG

    return $ Display [pngDisp, svgDisp]

chartData :: Width -> Height -> Renderable a -> FileFormat -> IO DisplayData
chartData w h renderable format = do
  switchToTmpDir

  -- Write the PNG image.
  let tmpFile = ".ihaskell-chart.png"
      opts = def{_fo_format = format, _fo_size = (w,h)}
  renderableToFile opts tmpFile renderable

  -- Convert to base64.
  imgData <- readFile $ fpFromString tmpFile
  return $ case format of
    PNG -> png w h $ base64 imgData
    SVG -> svg $ Char.unpack imgData

data SizedRenderable a = SizedRenderable Width Height (Renderable a)

sizeRenderable :: (Renderable a) -> Width -> Height -> SizedRenderable a
sizeRenderable r w h = SizedRenderable w h r

instance IHaskellDisplay (SizedRenderable a) where
  display (SizedRenderable h w renderable) = do
    pngDisp <- chartData w h renderable PNG
    svgDisp <- chartData w h renderable SVG
    return $ Display [pngDisp, svgDisp]

