# Todo 

* Don't rerun all commands for every newly generate command
  + only reset the system when shrinking

* Problem of strategy (pick something as basis for progress): coverage, logs,
  value of shared memory, helps bootstap the process. Generalise to support more?

* Problem of tactics: picking a good input distributed for the testing problem
  at hand. Make previous input influence the next input? Dependent events, e.g.
  if one packet gets lost, there's a higher chance that the next packet will be
  lost as well.

* More realistic example, e.g.: leader election, transaction rollback,
  failover?
* Annoying to sprinkle sometimes assertions everywhere?
  - Can it be combined with logging or tracing?

* Use size parameter to implement AFL heuristic for choosing integers? Or just
  use `frequency`?

* Type-generic mutation?
* sometimes_each?
* https://en.wikipedia.org/wiki/L%C3%A9vy_flight (optimises search)


