{- |
   Module     : System.Log.Handler.Color
   Copyright  : Copyright (C) 2012 Thorsten Lorenz
   License    : BSD3

   Maintainer : Thorsten Lorenz <thlorenz@gmx.de> 
   Stability  : provisional
   Portability: portable

Color log handler

Written by Thorsten Lorenz thlorenz@gmx.de

A little hack to make hslogger print to console in colors.


Works by activating ANSI color in its emit method which changes the color with which the global logger logs to the console.
-}

module System.Log.Handler.Color(colorHandler) where 

import Prelude hiding (catch)
import Control.Exception (SomeException, catch)
import Data.Char (ord)

import System.Console.ANSI 

import System.Log
import System.Log.Handler
import System.Log.Formatter
import System.IO
import Control.Concurrent.MVar

data ColorHandler   = ColorHandler { priority    :: Priority,
                                     formatter   :: LogFormatter ColorHandler }

instance LogHandler ColorHandler where
    setLevel ch p      = ch { priority = p }
    getLevel ch        = priority ch
    setFormatter ch f  = ch { formatter = f }
    getFormatter ch    = formatter ch
    emit ch (p, msg) name = do
        case p of
            DEBUG     -> setSGR [ SetColor Foreground Dull White ]
            INFO      -> setSGR [ SetColor Foreground Vivid Green ]
            NOTICE    -> setSGR [ SetColor Foreground Dull Blue ]
            WARNING   -> setSGR [ SetColor Foreground Vivid Blue ]
            ERROR     -> setSGR [ SetColor Foreground Dull Red ]
            CRITICAL  -> setSGR [ SetColor Foreground Vivid Red ]
            ALERT     -> setSGR [ SetColor Foreground Dull Yellow ]
            EMERGENCY -> setSGR [ SetColor Foreground Vivid Yellow ]
            
        -- Print empty line to make color setting take effect
        putStrLn ""

        -- global logger will log to console at this point and afterwards we reset color settings
        setSGR [ Reset ]

    close ch           = setSGR [ Reset ]

colorHandler :: ColorHandler
colorHandler = ColorHandler { priority = DEBUG, formatter = nullFormatter }

