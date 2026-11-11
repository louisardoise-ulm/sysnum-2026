# Netlist Simulator

OCaml netlist simulator for the SysNum 2026 project.

## Requirements

- OCaml
- Dune

## Compilation

From the project root:

```bash
cd tp1
dune build
```

## Execution

Run the simulator with a netlist file:

```bash
dune exec ./netlist_simulator.exe -- test/ram.net
```

To specify the number of simulation steps:

```bash
dune exec ./netlist_simulator.exe -- -n 5 test/ram.net
```

The simulator requests input values during execution. Inputs must be provided as binary strings (`0` and `1`).

## Tests

Example netlists are available in `tp1/test/`:

- `clockdiv.net`
- `fulladder.net`
- `fulladder_sch.net`
- `ram.net`
- `rom.net`

## Cleaning

To remove generated build files:

```bash
dune clean
```