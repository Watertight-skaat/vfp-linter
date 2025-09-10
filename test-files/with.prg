WITH moemployee
   .First_Name = 'John'
   .Last_Name = 'Smith'
   .Address = '16 Maple Lane'
ENDWITH

WITH m.toolbar
    .LockScreen = .t.

    * Add Separator
    .AddObject("objname" + LTRIM(STR(m.nToolButton)), "separator")
    .Objects(m.nToolButton).Style = 1

    * Add button
    m.nToolButton = m.nToolButton + 1
    .AddObject("objname" + LTRIM(STR(m.nToolButton)), "test")
    .AddProperty("propname" + LTRIM(STR(m.nToolButton)), "_screen.TestFunction.Click()")

    .Objects(m.nToolButton).Caption		= "Update"
    .Objects(m.nToolButton).ToolTipText	= _screen.UpdateNotificationBanner.Caption

    .Objects(m.nToolButton).FontBold	= .t.
    .Objects(m.nToolButton).BackColor	= _screen.UpdateNotificationBanner.BackColor	&& RGB(255, 210, 117)
    .Objects(m.nToolButton).ForeColor	= 0
    .Objects(m.nToolButton).Width		= 64
    .Objects(m.nToolButton).Autosize	= .t.
    .Objects(m.nToolButton).Autosize	= .f.
    .Objects(m.nToolButton).Height		= 32

    .LockScreen = .f.
ENDWITH