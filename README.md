## 2016 ICFP Contest - Team Powershop's Code

### Summary

We're three programmer colleagues from varied backgrounds who decided
to give ICFPC 2016 a try as a team, with the generous support of our
employer.

The [challenge this year](http://2016.icfpcontest.org/) was to infer
from the shape of a folded Origami what the crease pattern of the
unfolded square of paper would look like, and how points on the
unfolded paper would map back to the folded version.

This was the first time any of us had entered the contest as part of a
team, and our focus was to make the most of our ability to parallelise
our efforts.

Ben immediately built a kick-ass viewer for the problems using
Javascript, the Canvas API and a sprinkling of Ruby. This soon allowed
us to visualise and browse the problems. He also wrote a tool to fetch
problem definitions from the contest server for local processing.

In the meantime, Michael & Steve wrote Haskell code for parsing
problem definitions, and built a rough framework for our solver. This
resulted in a "solver"" program and an "unfold" program, so that we
could produce a bunch of unfolded versions of a starting state. By
allowing these to be dumped in the same format as the problems, we
were able to use Ben's viewer to see what our code was doing.

Michael wrote a bunch of nice code for unfolding polygons about edges,
which allowed us to solve the case of a square folded in half and then
in half again.

This all went very nicely, we had lots of fun in the process, and we
also largely managed to keep our time commitment manageable, without
working crazy hours.

We ultimately failed to make further progress for a couple of
reasons. Firstly, we got hung up on the perceived necessity of (and
writing the code for) exhaustively polygonising the problem starting
points. And secondly, we did this despite not really having a plan for
how to determine which shapes to unfold. In other words, we didn't
really have a strategy for solving the problems.

Along the way, we also neglected to write code which would produce and
submit correctly-formatted solution files, assuming that there was no
point unless we had a good solver strategy. This was probably
reasonable given the way we were approaching the rest of the work.

Our efforts tailed off and we retired after maybe 50 hours elapsed.

In hindsight, having built the viewer, problem parser and solver
framework, we probably should have set about producing complete and
naïve solutions to the first few very simple problems, and used the
lessons learned to gradually build a more refined solver. We knew all
along that we didn't have a solver strategy yet failed to fall back to
incremental experimentation.

In any case, we learned a lot - including that polygon maths are hard,
and that Javascript isn't a great choice when some of the numbers
you're working with look
like 1267650600228229401496703205376. Hopefully the team will be back
with reinforcements for next year's contest!

-Steve

### Authors

This software was written by
[Ben Anderson](https://github.com/bagedevimo),
[Michael Fowler](https://github.com/mkrfowler) and
[Steve Purcell](https://github.com/purcell)
with the
support of our awesome employer
[Powershop](http://www.powershop.co.nz/), who have other cool stuff
[here on Github](https://github.com/powershop).

### License and copyright

Copyright Powershop NZ Ltd.
