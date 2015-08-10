module Main where

import IRC
import Node.IRC

main = runBot { nick: Nick "nick goes here"
              , host: Host "something like irc.freenode.net or something"
              , name: "some name"
              , chan: Channel "some channel"
              , password: "some password"
              , keywords: ["some", "keywords"] }
