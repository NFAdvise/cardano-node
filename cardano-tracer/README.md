# Cardano Tracer

`cardano-tracer` is a service for logging and monitoring over Cardano nodes. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

For more details please [read the documentation](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md).

## RTView

RTView is a real-time monitoring tool for Cardano nodes (RTView is an abbreviation for "Real Time View"), it is a part of `cardano-tracer` service. RTView provides an interactive web page where you can see different kinds of information about connected nodes (something like Grafana).

For more details please [read its documentation](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-rtview.md).

## Developers

Benchmarking team is responsible for this service. The primary developer is [@denisshevchenko](https://github.com/denisshevchenko).
