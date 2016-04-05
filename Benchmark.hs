import Criterion.Main
import qualified Data.Text.IO as TIO
import Control.DeepSeq
import qualified Text.HTML.TagSoup as Soup
import qualified Text.HTML.Parser as Me

main :: IO ()
main = do
    t <- TIO.readFile "test.html"
    s <- readFile "test.html"
    defaultMain
        [ bgroup "Forced"
            [ bench "fast parse Text" $ nf (Soup.parseTagsOptions Soup.parseOptionsFast) t
            , bench "parse Text" $ nf (Soup.parseTagsOptions Soup.parseOptions) t
            , bench "mine Text" $ nf Me.tagStream t
            ]
        , bgroup "length"
            [ bench "fast parse Text" $ whnf (length . Soup.parseTagsOptions Soup.parseOptionsFast) t
            , bench "parse Text" $ whnf (length . Soup.parseTagsOptions Soup.parseOptions) t
            , bench "mine Text" $ whnf (length . Me.tagStream) t
            ]
        ]

instance NFData t => NFData (Soup.Tag t) where
    rnf (Soup.TagOpen t attrs) = rnf t `seq` rnf attrs `seq` ()
    rnf (Soup.TagClose t)      = rnf t `seq` ()
    rnf (Soup.TagText t)       = rnf t `seq` ()
    rnf (Soup.TagComment t)    = rnf t `seq` ()
    rnf (Soup.TagWarning t)    = rnf t `seq` ()
    rnf (Soup.TagPosition _ _) = ()
