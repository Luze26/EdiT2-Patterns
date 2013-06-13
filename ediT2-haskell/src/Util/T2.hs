module Util.T2 where


import Util.Tree
import Util.Cell



-- Pattern's objects.
type Objects = ([(String, String)],[(String, String)],[(String, String)],[(String, String)],[(String, String)])


data T2 =
	T2 {
		t2Notions :: [String],
		t2Objects :: Objects,
		t2Tree :: NTree Cell,
		t2Errors :: [String],
		t2Type :: Int,
		t2StructName :: String,
		t2TeacherNotes :: [String]
	} deriving ( Show )



getObject :: Int -> Objects -> [(String, String)]
getObject i (x,y,z,u,v)
	| i == 1 = x
	| i == 2 = y
	| i == 3 = z
	| i == 4 = u
	| i == 5 = v
	| otherwise = []



changeObject :: Int -> [(String, String)] -> Objects -> Objects
changeObject i object (x,y,z,u,v)
	| i == 1 = (object,y,z,u,v)
	| i == 2 = (x,object,z,u,v)
	| i == 3 = (x,y,object,u,v)
	| i == 4 = (x,y,z,object,v)
	| i == 5 = (x,y,z,u,object)
	| otherwise = (x,y,z,u,v)



emptyT2 :: T2
emptyT2 = T2 { t2Notions = [], t2Objects = ([],[],[],[],[]), t2Tree = EmptyTree, t2Errors = [], t2Type = -1, t2StructName = "", t2TeacherNotes = [] }



setT2Notions :: [String] -> T2 -> T2
setT2Notions notions T2 { t2Notions = _, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }
	= T2 { t2Notions = notions, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Objects :: Objects -> T2 -> T2
setT2Objects o T2 { t2Notions = no, t2Objects = _, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Tree :: NTree Cell -> T2 -> T2
setT2Tree tree T2 { t2Notions = no, t2Objects = o, t2Tree = _, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = tree, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Errors :: [String] -> T2 -> T2
setT2Errors errs T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = _, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = errs, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Type :: Int -> T2 -> T2
setT2Type typ T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = _, t2StructName = n, t2TeacherNotes = tn }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Name :: String -> T2 -> T2
setT2Name n T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = _, t2TeacherNotes = tn }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = tn }



setT2Notes :: [String] -> T2 -> T2
setT2Notes notes T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = _ }
	= T2 { t2Notions = no, t2Objects = o, t2Tree = t, t2Errors = err, t2Type = typ, t2StructName = n, t2TeacherNotes = notes }