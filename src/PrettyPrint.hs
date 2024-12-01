module PrettyPrint (prettyPrint) where

import Text.Pretty.Simple (CheckColorTty (NoCheckColorTty), OutputOptions (..), defaultOutputOptionsLightBg, pPrintOpt)

outputOpts :: OutputOptions
outputOpts =
  defaultOutputOptionsLightBg
    { outputOptionsIndentAmount = 2,
      outputOptionsPageWidth = 100,
      outputOptionsCompact = True,
      outputOptionsCompactParens = True
    }

prettyPrint :: (Show a) => a -> IO ()
prettyPrint = pPrintOpt NoCheckColorTty outputOpts
