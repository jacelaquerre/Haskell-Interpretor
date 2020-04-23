# Running Files

    make hw01
    make hw02
    ...(etc)

# Running ghcid on Files

    make hw01-dev
    make hw02-dev
    ...(etc)

# Observing Parser Output

    make pl1 E="1 + 2"
    cat <file> | make pl1

    make pl2 E="1 + 2 == 3"
    cat <file> | make pl2

# Debugging

If any of these makefile hooks aren't working, try:

    stack build

and make sure it completes successfully first.
