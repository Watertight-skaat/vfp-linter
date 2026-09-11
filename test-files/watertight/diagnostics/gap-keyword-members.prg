* Properties and columns whose name happens to be a FoxPro keyword. The grammar refuses a keyword
* after the dot, so the member reference is cut short and the rest of the line is left over. The
* .NET message objects and the EDI cursors both have these.
LPARAMETERS m.oMessage, m.oRecord

m.cRecipient	= m.oMessage.To
m.cSender		= m.oMessage.From
m.cClassName	= m.oRecord.Class
m.cSelection	= m.oRecord.Select

m.oMessage.To = "billing@example.com"
