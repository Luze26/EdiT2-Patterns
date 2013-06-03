module Constraints
(Con Data "Cstr" "Cstr",Value {args = 3, fixity = Def, priority = 9, letBound = True, traced = True})
(Con Data "Identificator" "Content",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(Con Data "Identificator" "Identificator",Value {args = 2, fixity = Def, priority = 9, letBound = True, traced = True})
(Con Data "Identificator" "Label",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(Field "Cstr" "command",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(Field "Cstr" "items",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(Field "Cstr" "wher",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(TypeClass "Cmd",TyCls (Syn 0 THelper))
(TypeClass "Cstr",TyCls (Ty ["Cstr"] ["items","command","wher"]))
(TypeClass "Identificator",TyCls (Ty ["Label","Content","Identificator"] []))
(TypeClass "Identificators",TyCls (Syn 0 THelper))