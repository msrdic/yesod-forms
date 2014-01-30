import Import
import Handler.Home
import Handler.Fib
import Handler.Markdown

mkYesodDispatch "HKaido" resourcesHKaido

main :: IO ()
main = warpEnv HKaido