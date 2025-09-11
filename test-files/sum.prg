SUM ALL cost_value ;
    FOR Cost_Sold.Cutoff=AcctHistDates.Cutoff ;
        .and. Cost_Sold.catnum = COGSlist.catnum ;
        .and. Cost_Sold.camefrom = COGSlist.camefrom ;
        .and. Cost_Sold.LedDept = COGSlist.LedDept ;
        .and. IsNull(Cost_Sold.l_acct_mos) ;
    TO m.CostTotal