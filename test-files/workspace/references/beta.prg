LOCAL lcOut, lcName
lcName = "Ping"
lcOut = Ping("beta")

* Named at run time, so it is not a reference to anything the index can see.
DO &lcName
