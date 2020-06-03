{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "generics-rep"
  , "lists"
  , "maybe"
  , "psci-support"
  , "read"
  , "strings"
  , "tuples"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
