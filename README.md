# Motor: Type-safe effectful state machines in Haskell

*Motor* is an experimental Haskell library for building finite-state
machines with type-safe transitions and effects. It draws inspiration
from the Idris
[ST](http://docs.idris-lang.org/en/latest/st/state.html) library.

## Usage

See the [Motor module documentation on
Hackage](http://hackage.haskell.org/package/motor) for detailed usage.

## Packages

* [motor](motor/) — the core of Motor.
* [motor-reflection](motor-reflection/) — reflect on typeclasses
  following Motor FSM conventions.
* [motor-diagrams](motor-diagrams/) — use value-representations from
  `motor-reflection` to generate diagrams.

## Examples

* [Door](motor/examples/Door.hs)

## License

Mozilla Public License Version 2.0, see `LICENSE` file in each
package.
