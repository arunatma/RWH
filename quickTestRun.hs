import Prettify
import PrettifyTest
import Test.QuickCheck.Batch

-- TestOptions is available in Test.QuickCheck.Batch, but not getting loaded

-- If loaded,
-- runghc quickTestRun.hs 
-- this would run the tests under 'simple' and 'complex' tags

options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }

main = do
    runTests "simple" options
        [ run prop_empty_id
        , run prop_char
        , run prop_text
        , run prop_line
        , run prop_double
        ]

    runTests "complex" options
        [ run prop_hcat
        , run prop_puncutate'
        ]