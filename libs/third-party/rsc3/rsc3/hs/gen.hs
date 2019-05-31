import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Bindings.Lisp as DB {- hsc3-db -}

main :: IO ()
main = do
  let b = map (\nm -> DB.lisp_mk_ugen nm) DB.complete_names
  writeFile "/home/rohan/sw/rsc3/src/db/ugen.scm" (unlines b)
