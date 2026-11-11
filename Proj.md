# Part 1

## Q5

The architecture is somewhat linear using topological sort : 

``` Pseudo-code
get inputs, memory;
fetch registers;
for expr in topo_expr_list:
    eval expr(inputs, memory);
write_to_ram();
return outputs;
```

## Q6

One needs to update registers at each clock-cycle (upclock for instance) and not touch them during a clock cycle.
Essentially they just need to hold the previous value of the variable and not be disturbed til they re called.

ROM is read only but the address can change during a cycle so it has to be treated like any other component and computed as soon as the memory has been computed (topologically at least) wheras RAM can be written and read during a cycle. Yet the order is only ever specified by the program ran on the processor. Therefore it must not be considered as a variable (we dont wait for it to be computed, we fetch it when the r_addr is ready) and is written at the end of the cycle without any other trouble.