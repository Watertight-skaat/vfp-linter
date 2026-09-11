* Properties and columns whose name happens to be a FoxPro keyword. A keyword after the dot is just a name, so these read as members; refusing them cut each reference short and left the rest of the line over. The .NET message objects and the EDI cursors both have them.
* What still has to be told apart is the dot operators -- `.AND.`, `.T.` and `.NULL.` are never members, and the closing dot is the only thing that says so.
LPARAMETERS m.oMessage, m.oRecord

m.cRecipient	= m.oMessage.To
m.cSender		= m.oMessage.From
m.cClassName	= m.oRecord.Class
m.cSelection	= m.oRecord.Select

m.oMessage.To = "billing@example.com"
