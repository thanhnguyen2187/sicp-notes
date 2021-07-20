# Modularity, Objects, and State

> One powerful design strategy, which is particularly appropiate to the
> construction of programs for modeling physical systems, is to base the
> structure of our programs on the structure of the system being modeled.
>
> - For each object in the system, we construct a corresponding computational
>   object.
> - For each system action, we define a symbolic operation in our computational
>   model.
>
> Our hope in using this strategy is that extending the model to accomodate the
> new objects or new actions will require no strategic changes to the program,
> only the addition of the new symbolic analogs of those objects or actions.
>
> If we have been successful in our system organization, then to add a new
> feature or debug an old one we will have to work on only a localized part of
> the system.
>
> [...] two prominent organizational strategies arising from two rather
> different "world views" of the structure of systems.
> - The first organizational strategy concentrates on /objects/, viewing a large
>   system as a collection of distinct objects whose behaviors may change over
>   time.
> - An alternative organization strategy concentrates on the /streams/ of
>   information that flow in the system, much as an electrical engineer views a
>   signal-processing system.
