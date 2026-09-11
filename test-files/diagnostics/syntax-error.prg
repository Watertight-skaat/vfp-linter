* A dangling terminator is a real syntax error, and it is absorbed rather than thrown: the parser used to give up on the whole file, so the user lost every other diagnostic in it until the line was fixed -- and while typing, that line is usually the one being written.
ENDIF

* Everything below it is still checked, which is the point of absorbing it.
LOCAL lcUnused
lnUndeclared = 1
