# hecs-reactive

React- or Elm-like entity management based on
[`hecs`](https://crates.io/crates/hecs) and
[`hecs-hierarchy`](https://crates.io/crates/hecs-hierarchy).

This is intended to serve as a building block for a reactive UI system.

Every time the UI layout should be updated, the user creates a tree of property
entities, then passes it to `hecs_reactive::update`. The `update` function
creates a matching tree of node entities, with a component linking each node to
its properties. When possible, node entities are kept from the previous
iteration; this allows saving state.
