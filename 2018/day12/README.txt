CHALLENGES
==========

Had an off-by-one error that broke things for the initial example.

An irritating problem with growing the edges - choosing a fixed width and
failing if it reached would have been simpler in retrospect. I remembered to
expand, but I initially only had it grow after running a step of the simulation,
not before, so the first iteration was missed and skewed everything else, but
the example input wasn't affected. 
