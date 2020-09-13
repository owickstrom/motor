# motor-reflection

Reflect on typeclasses following Motor FSM conventions, using Template
Haskell.

**This package is experimental!**

## Usage

First, you need to import the reflection module and the `Event` type:

``` haskell
import           Motor.FSM.Reflection
import           Motor.FSM.Reflection.Event (Event)
```

Given that your FSM typeclass follows Motor's conventions, you can
reflect to get a list of events:

``` haskell
data Open
data Closed

class MonadFSM m => Door (m :: Row * -> Row * -> * -> *) where
  type State m :: * -> *

  initial :: Name n -> Actions m '[n !+ State m Open] r ()
  close :: Name n -> Actions m '[n := State m Open !--> State m Closed] r ()
  open :: Name n -> Actions m '[n := State m Closed !--> State m Open] r ()
  end :: Name n -> Actions m '[n !- State m Closed] r ()

reflectEvents ''Door "doorEvents"
```

The last line, using `reflectEvents`, will define `doorEvents ::
[Motor.FSM.Reflection.Event.Event]` at the top-level.

### Conventions

* The typeclass must have an associated type (open type family) called
  `State`, parameterized by the instance, with a result of kind `* ->
  *`.
* Each FSM event in the typeclass must have a return type of the
  `Actions` type alias, listing the additions, deletions, and
  transitions of named resources.

These conventions may be relaxed in the future. This package is new
and experimental.

## License

Mozilla Public License Version 2.0, see `LICENSE` file in each
package.
