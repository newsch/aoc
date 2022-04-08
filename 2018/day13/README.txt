entirely written without running

worked with the debugger, somewhat frustrating
cannot attach to separate program, so pass input file as arg not stdin

separating out into modules ocamllsp was not happy with, not sure how to resolve

BUGS
====

A logic error where is_crash was inverted and take_while was used instead of drop_while

Had vertical axes mixed up for new_pos in update_cart, then overzealously
reversed horizontal axes as well

Didn't check for collisions between carts while moving them. This stumped me
for a bit. To fix it I went with creating a "ripple" function to map with a
finger-like view of what had been changed and what hadn't instead of switching
to using a mutable Array everywhere. The instructions only talk about colliding
with a cart that has already been moved, but my answer involved colliding
with a cart that had not been moved yet. In the context of mutating an array
in-place it makes sense, but it's not something map or fold_left is suited for.
