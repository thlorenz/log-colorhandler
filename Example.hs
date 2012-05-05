module Example where

import System.Log.Logger
import System.Log.Handler.Color

main :: IO ()
main = do
    updateGlobalLogger "Console" (setLevel DEBUG)
    
    -- activate color logging
    updateGlobalLogger rootLoggerName (addHandler colorHandler)

    debugM     "Console"  "Logging to debug    "   
    infoM      "Console"  "Logging to info     "
    noticeM    "Console"  "Logging to notice   "
    warningM   "Console"  "Logging to warning  "
    errorM     "Console"  "Logging to error    "
    criticalM  "Console"  "Logging to critical "
    alertM     "Console"  "Logging to alert    "
    emergencyM "Console"  "Logging to emergency"

