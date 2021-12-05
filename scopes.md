- 'envBindings' is map string to enventry
- keep inmodule
- new lamndaenv type. string to lambdaenvvalue
? module bindings are filtered in inScope
? when looking up args, make sure lambdaenv takes precedent
* wrap 'whole env' (all 'envBindings' values filtered by module [inefficient, but who cares?]) when passing lambda into lambdaenv (ie, as argument)
  * put into availableBindings and use that for debugging
- both env and lambdaScope belong to some typeclass which can be pushed into by the extend functions
? letins are lambdas. pass bindings as args in lambdaenv
- update envLambdaEnv in env
- where do we remove lambdaenv from scope? 'after call' sounds simple
- set envEntryModule to Nothing for lambdaEnv, as they're callable without being in the right module
  * possibly rename it to 'moduleRestriction'
- when merging lambdaenv and env, how do we make sure lambdaenv scope overrides? we can't overwrite
  names, we want pattern matching available. just add lambdaenvbindings later in the list?
- list of lambdaEnvBindings to allow for instance calling lambda in function
  - on application, add one layer
  - on return, remove one layer

- if calling another function, DON'T push lambda env to env
- if calling lambda function, DO push lambda env to env


* write good tests and solve one at a time
  - a function can get an arg and return it
  - test that funtion that executes a lambda within it doesn't have access to a lambas scope
  XXX: * lambda module needs to be spawn site module
  * a lambda in a function has access to both its own lambda env and function lambda env



what happens when matching function definition can be found in destination module as well?
* only function `c` from lambda-spawn site should be available

module A {
  c [] = 2
  c (x::xs) = x
  a f = f 3
}

module B {
  c [] = 1
  c (x::xs) = x
  A.a (x: x + (c []))
}


qs:

Q: multiple definitions possible in lambdaenv?
  * yes, as pattern matched functions from module are pushed here on extension into lambdaenv
