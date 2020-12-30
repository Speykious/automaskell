module DotShow where

import System.Process (callCommand)

class DotShow a where
  dotShow :: a -> String
  dotDeclare :: a -> Maybe String
  dotDeclare = const Nothing
  
  dotShowPDF :: String -> a -> IO ()
  dotShowPDF name a = do
      writeFile dotName (dotShow a)
      callCommand $ "dot -Tpdf " ++ dotName ++ " -o " ++ pdfName
      callCommand $ "xdg-open " ++ pdfName
    where dotName = name ++ ".dot"
          pdfName = name ++ ".pdf"