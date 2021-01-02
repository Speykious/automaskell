module DotShow where

import System.Process (callCommand)

-- | Typeclass similar to Show used to show the graph representation of automata using DOT.
class DotShow a where
  -- | Outputs a string written in the DOT language to show the graph representation of `a`.
  dotShow :: a -> String
  -- | Special case where we want to declare some objects like states inside a DOT file.
  dotDeclare :: a -> Maybe String
  dotDeclare = const Nothing
  
  -- | Generates and opens a PDF showing the graph representation of a (with default filename).
  dotPDF :: a -> IO ()
  dotPDF = dotShowPDF "temp"

  -- | Generates and opens a PDF showing the graph representation of a.
  dotShowPDF :: String -- ^ The name of the file to generate.
             -> a -> IO ()
  dotShowPDF name a = do
      writeFile dotName (dotShow a)
      callCommand $ "dot -Tpdf " ++ dotName ++ " -o " ++ pdfName
      callCommand $ "xdg-open " ++ pdfName
    where dotName = name ++ ".dot"
          pdfName = name ++ ".pdf"
  
  {-# MINIMAL dotShow #-}