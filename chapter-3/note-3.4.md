# 3.4 Concurrency: Time Is of the Essence

## 3.4.2. Mechanisms for Controlling Concurrency

### Concurrency, time, and communication

We've seen how programming concurrent systems requires controlling the ordering
of events when different processes access shared state, and we've seend how to
achieve this control through judicious use of serializers. But the problems of
concurrency lie deeper than this, because, from a fundamental point of view,
it's not always clear what is meant by "shared state."

...

The basic phenomenon here is that synchronizing different processes,
establishing shared state, or imposing an order on events requires communication
among the processes. In essence, any notion of time in concurrency control must
be intimately tied to communication. It is intriguing that a similar connection
between time and communication also arises in the Theory of Relativity, where
the speed of light (the fastest signal that can be used to synchronize events)
is a fundamental constant relating time and space. The complexities we encounter
in dealing with time and state in our computational models may in fact mirror a
fundamental complexity of the physical universe.
