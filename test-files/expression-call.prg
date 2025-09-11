* multi-line expression call
=SEEK(customers.branchId+customers.acctnum+STR(m.OrderObj.RecordObj.addrnum),"addresses","customers") ;
    .or. seek(customers.branchId+customers.acctnum+STR(customers.primeaddr),"addresses","customers") ;
    .or. seek(customers.branchId+customers.acctnum,"addresses","customers") 