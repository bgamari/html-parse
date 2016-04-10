import Criterion.Main
import qualified Data.Text.IO as TIO
import Control.DeepSeq
import qualified Text.HTML.TagSoup as Soup
import qualified Text.HTML.Parser as Me

main :: IO ()
main = do
    t <- TIO.readFile "test.html"
    defaultMain
        [ bgroup "Forced"
            [ bench "tagsoup fast Text" $ nf (Soup.parseTagsOptions Soup.parseOptionsFast) t
            , bench "tagsoup normal Text" $ nf (Soup.parseTagsOptions Soup.parseOptions) t
            , bench "html-parser" $ nf Me.tagStream t
            ]
        , bgroup "length"
            [ bench "tagsoup fast Text" $ whnf (length . Soup.parseTagsOptions Soup.parseOptionsFast) t
            , bench "tagsoup normal Text" $ whnf (length . Soup.parseTagsOptions Soup.parseOptions) t
            , bench "html-parser" $ whnf (length . Me.tagStream) t
            ]
        ]

instance NFData t => NFData (Soup.Tag t) where
    rnf (Soup.TagOpen t attrs) = rnf t `seq` rnf attrs `seq` ()
    rnf (Soup.TagClose t)      = rnf t `seq` ()
    rnf (Soup.TagText t)       = rnf t `seq` ()
    rnf (Soup.TagComment t)    = rnf t `seq` ()
    rnf (Soup.TagWarning t)    = rnf t `seq` ()
    rnf (Soup.TagPosition _ _) = ()
