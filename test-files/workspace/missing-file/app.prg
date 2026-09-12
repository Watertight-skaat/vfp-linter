* Every way a .prg names a file, half of them naming one that is not there.
LOCAL lcForm
lcForm = "picker"

#INCLUDE "there.h"
#INCLUDE "gone.h"

SET PROCEDURE TO lib ADDITIVE
SET PROCEDURE TO vanished ADDITIVE
SET CLASSLIB TO controls
SET CLASSLIB TO absent

DO sub\task.prg
DO sub\gone.prg

DO FORM there
DO FORM elsewhere

* A drive this machine cannot see is not evidence of anything, so an absolute path that resolves to nothing is left alone.
#INCLUDE "S:\Libs\Watertight.WebClients.h"
SET PROCEDURE TO S:\Libs\shared.prg

* Named at run time.
DO FORM (m.lcForm)

* A bare name is a routine rather than a file, and what to say about one nothing defines belongs to a rule that knows the built-ins.
DO SomeRoutine
