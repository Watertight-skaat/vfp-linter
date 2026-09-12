LOCAL lcLabel
DO PostCharge WITH "A100", 25
DO FORM ledgerview
lcLabel = Describe("A100", 10)
? m.lcLabel
