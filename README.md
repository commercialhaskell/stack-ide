# Standalone JSON client for `ide-backend`

This executable binds against the `ide-backend` library and makes it API available over JSON. It is intended to faciliate integration of `ide-backend` in editors such Atom, Emacs, Sublime, Vim, etc.

# Installation instructions

Make sure to compile `ide-backend-client` against the same version of the `Cabal` library that your `cabal-install` uses. For instance, you could add a file `cabal.config`:

```
constraints: Cabal==1.22.0.0
```

# Example interaction:

``` javascript
// ide-backend-client sends welcome message and provides its version
{ "response": "welcome"
, "version": { "minor": 1, "major": 0, "patch": 0 }
}

// editor requests to load a source file
> { "request": "updateSession"
> , "update": [
>     { "update": "updateSourceFileFromFile"
>     , "filePath": "t/A.hs"
>     }
>   ]
> }

// client responds with progress message
< { "progress": {
<     "origMsg": "[1 of 1] Compiling A ( /path/to/t/A.hs, interpreted )",
<     "parsedMsg": "Compiling A",
<     "numSteps": 1,
<     "step": 1
<   }
< , "response": "sessionUpdate"
< }

// After the last progress message where will be one more response
< { "response": "sessionUpdate" }

// Editor requests the list of errors
> { "request": "getSourceErrors" }

// client responds with the empty list
< { "response": "getSourceErrors"
< , "errors": []
< }

// editor requests information about an identifier at a specific location
> { "request": "getSpanInfo"
> , "module": "A"
> , "span": {
>       "filePath": "t/A.hs"
>     , "fromLine": 3
>     , "fromColumn": 2
>     , "toLine": 3
>     , "toColumn": 2
>     }
> }

// client responds with the information
< {  
<   "response": "getSpanInfo",
<   "info": [  
<     {  
<       "span": {  
<         "toColumn": 4,
<         "fromLine": 3,
<         "filePath": "t/A.hs",
<         "toLine": 3,
<         "fromColumn": 1
<       },
<       "isQuasiQuote": false,
<       "idInfo": {  
<         "scope": {  
<           "scope": "local"
<         },
<         "prop": {  
<           "nameSpace": "varName",
<           "name": "foo",
<           "type": "Int",
<           "defSpan": {  
<             "toColumn": 4,
<             "fromLine": 4,
<             "filePath": "t/A.hs",
<             "toLine": 4,
<             "fromColumn": 1
<           },
<           "definedIn": {  
<             "name": "A",
<             "package": {  
<               "packageKey": "main",
<               "name": "main"
<             }
<           }
<         }
<       }
<     }
<   ]
< }

// editor asks for the type of an expression at a particular location
> { "request": "getExpTypes"
> , "module": "A"
> , "span": {
>       "filePath": "t/A.hs"
>     , "fromLine": 4
>     , "fromColumn": 7
>     , "toLine": 4
>     , "toColumn": 7
>     }
> }

// client responds
< {  
<   "response": "getExpTypes",
<   "info":[  
<     {  
<       "span": {  
<         "toColumn": 8,
<         "fromLine": 4,
<         "filePath": "t/A.hs",
<         "toLine": 4,
<         "fromColumn": 7
<       },
<       "type": "Int"
<     }
<   ]
< }

// editor requests shutdown
> { "request": "shutdownSession" }

// client shuts down
< { "response":"shutdownSession" }
