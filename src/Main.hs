import Import
import Handler.Home

mkYesodDispatch "HKaido" resourcesHKaido

main :: IO ()
main = warpEnv HKaido