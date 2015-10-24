# logen-bta
A binding-time analysis (BTA) for the logen offline partial evaluator.

Written by Michael Leuschel and German Vidal


# Building
To build you need SICStus Prolog Version 4.2.x

Simply run make which will produce a binary bta inside the bin directory.

Simply run "make test" to do a simple test, which will use the bta to
produce an annotated version for an example program, run LOGEN on it
and the run the specialised program.
(You need  to build Logen https://github.com/leuschel/logen and have the logen binary
on the PATH. You should probably also set the LOGENDIR environment variable to
point to the root directory containing the logen sources and binary.)

# Research
This system is based on research presented in

http://www.sciencedirect.com/science/article/pii/S0890540114000066
Fast offline partial evaluation of logic programs ☆
Michael Leuschel, Germán Vidal
Information and Computation 235, pages 70-97. April 2014.

http://stups.hhu.de/w/Special:Publication/LeuschelTamaritVidal_WFLP09
Fast and Accurate Strong Termination Analysis with an Application to Partial Evaluation
In Proceedings WFLP'2009, LNCS 5979, Springer-Verlag, 2010.

http://stups.hhu.de/w/Special:Publication/LeVi08_234
Fast Offline Partial Evaluation for Large Logic Programs
In Proceedings LOPSTR'08, volume 5438 of Lecture Notes in Computer Science, 2008.