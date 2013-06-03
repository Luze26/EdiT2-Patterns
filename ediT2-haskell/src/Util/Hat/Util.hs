module Hat.Util
  (Activity(),Group(),Participant(),Role(),Resource(),PatternObjects()
    ,ActivityObjects(),GroupObjects(),ParticipantObjects(),ResourceObjects()
    ,RoleObjects(),TeacherNotes(),Name(),Description(),Login(),FirstName()
    ,Surname(),Email(),Country(),City(),MoodleTableName(),MoodleResourceID()
    ,Possible(..),greadInt,hreadInt,gparticipantsLogins,hparticipantsLogins
    ,gresourcesNames,hresourcesNames,gactivitiesObjects,hactivitiesObjects
    ,ggroupsObjects,hgroupsObjects,gparticipantsObjects,hparticipantsObjects
    ,gresourcesObjects,hresourcesObjects,grolesObjects,hrolesObjects,gcreateList
    ,hcreateList,gcreateList',hcreateList',gwriteT2,hwriteT2,gwriteT2Err
    ,hwriteT2Err,gshowObjects,hshowObjects,gerrors,herrors,gerrors',herrors'
    ,greadHeader,hreadHeader,gstripHeader,hstripHeader,gstripFooter,hstripFooter
    ,greadTree,hreadTree,greadObjects,hreadObjects,greadConstraints
    ,hreadConstraints,gsplitList,hsplitList,gsplitList2,hsplitList2,gsplitList2'
    ,hsplitList2',gpossibleToList,hpossibleToList,grepartition,hrepartition
    ,grepartition',hrepartition',grepartitionUniform,hrepartitionUniform
    ,grepartition2,hrepartition2,grepartition2',hrepartition2',aPossible
    ,aNotPossible,areadInt,aparticipantsLogins,aresourcesNames
    ,aactivitiesObjects,agroupsObjects,aparticipantsObjects,aresourcesObjects
    ,arolesObjects,acreateList,acreateList',awriteT2,awriteT2Err,ashowObjects
    ,aerrors,aerrors',areadHeader,astripHeader,astripFooter,areadTree
    ,areadObjects,areadConstraints,asplitList,asplitList2,asplitList2'
    ,apossibleToList,arepartition,arepartition',arepartitionUniform
    ,arepartition2,arepartition2') where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Tree 
import Hat.Constraints 

type Activity = String

type Group = String

type Participant = String

type Role = String

type Resource = String

type PatternObjects =
  T.Tuple5 ActivityObjects GroupObjects ParticipantObjects ResourceObjects
    RoleObjects

type ActivityObjects = T.List (T.Tuple2 Name Description)

type GroupObjects = T.List (T.Tuple2 Name Description)

type ParticipantObjects =
  T.List (T.Tuple6 Login FirstName Surname Email City Country)

type ResourceObjects =
  T.List (T.Tuple4 Name Description MoodleTableName MoodleResourceID)

type RoleObjects = T.List (T.Tuple2 Name Description)

type TeacherNotes = T.List String

type Name = String

type Description = String

type Login = String

type FirstName = String

type Surname = String

type Email = String

type Country = String

type City = String

type MoodleTableName = String

type MoodleResourceID = String

data Possible a = Possible (T.R a) | NotPossible (T.R String)

instance T.WrapVal ((Possible a))
  where
  
  wrapVal pwrapVal (kwrapVal@(Possible (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aPossible z1wrapVal)
  wrapVal pwrapVal (kwrapVal@(NotPossible (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aNotPossible z1wrapVal)
  

instance Eq a => Eq ((Possible a))
  where
  
  (!==) (%==) p =
    T.ufun2 (+&@=+#=&@=+$==) (%==) p (*==)
    where
    
    (*==) (T.R (Possible fy1) _) (T.R (Possible fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) (T.R (NotPossible fy1) _) (T.R (NotPossible fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Show a => Show ((Possible a))
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a48v65v48v68showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Possible fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "Possible "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec fy1 (T.R (NotPossible fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "NotPossible "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec _ _ p = T.fatal p
    
  

greadInt :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String Int)

hreadInt :: (T.R String) -> T.RefExp -> T.R Int

greadInt preadInt p = T.fun1 areadInt preadInt p hreadInt

hreadInt fn p = T.uapp1 p58v13v58v18 p58v13v58v16 p aread hread fn

gparticipantsLogins ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ParticipantObjects (T.List Participant))

hparticipantsLogins ::
  (T.R ParticipantObjects) -> T.RefExp -> T.R (T.List Participant)

gparticipantsLogins pparticipantsLogins p =
  T.fun1 aparticipantsLogins pparticipantsLogins p hparticipantsLogins

hparticipantsLogins fps p =
  T.uapp2 p65v25v65v60 p65v25v65v27 p amap hmap
    (T.fun1 T.mkLambda p65v30v65v56 p
      (\ v65v30v65v56v1 p ->
        case (v65v30v65v56v1) of
          (T.R (T.Tuple6 flogin _ _ _ _ _) _) ->
            T.projection p65v52v65v56 p flogin
          _ -> T.fatal p)) fps

gresourcesNames ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ResourceObjects (T.List Resource))

hresourcesNames :: (T.R ResourceObjects) -> T.RefExp -> T.R (T.List Resource)

gresourcesNames presourcesNames p =
  T.fun1 aresourcesNames presourcesNames p hresourcesNames

hresourcesNames frs p =
  T.uapp2 p70v21v70v50 p70v21v70v23 p amap hmap
    (T.fun1 T.mkLambda p70v26v70v46 p
      (\ v70v26v70v46v1 p ->
        case (v70v26v70v46v1) of
          (T.R (T.Tuple4 fname _ _ _) _) -> T.projection p70v43v70v46 p fname
          _ -> T.fatal p)) frs

gactivitiesObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects ActivityObjects)

hactivitiesObjects :: (T.R PatternObjects) -> T.RefExp -> T.R ActivityObjects

gactivitiesObjects pactivitiesObjects p =
  T.fun1 aactivitiesObjects pactivitiesObjects p hactivitiesObjects

hactivitiesObjects (T.R (T.Tuple5 fo _ _ _ _) _) p =
  T.projection p76v33v76v33 p fo
hactivitiesObjects _ p = T.fatal p

ggroupsObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects GroupObjects)

hgroupsObjects :: (T.R PatternObjects) -> T.RefExp -> T.R GroupObjects

ggroupsObjects pgroupsObjects p =
  T.fun1 agroupsObjects pgroupsObjects p hgroupsObjects

hgroupsObjects (T.R (T.Tuple5 _ fo _ _ _) _) p = T.projection p81v29v81v29 p fo
hgroupsObjects _ p = T.fatal p

gparticipantsObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects ParticipantObjects)

hparticipantsObjects ::
  (T.R PatternObjects) -> T.RefExp -> T.R ParticipantObjects

gparticipantsObjects pparticipantsObjects p =
  T.fun1 aparticipantsObjects pparticipantsObjects p hparticipantsObjects

hparticipantsObjects (T.R (T.Tuple5 _ _ fo _ _) _) p =
  T.projection p87v35v87v35 p fo
hparticipantsObjects _ p = T.fatal p

gresourcesObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects ResourceObjects)

hresourcesObjects :: (T.R PatternObjects) -> T.RefExp -> T.R ResourceObjects

gresourcesObjects presourcesObjects p =
  T.fun1 aresourcesObjects presourcesObjects p hresourcesObjects

hresourcesObjects (T.R (T.Tuple5 _ _ _ fo _) _) p =
  T.projection p93v32v93v32 p fo
hresourcesObjects _ p = T.fatal p

grolesObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects RoleObjects)

hrolesObjects :: (T.R PatternObjects) -> T.RefExp -> T.R RoleObjects

grolesObjects prolesObjects p =
  T.fun1 arolesObjects prolesObjects p hrolesObjects

hrolesObjects (T.R (T.Tuple5 _ _ _ _ fo) _) p = T.projection p99v28v99v28 p fo
hrolesObjects _ p = T.fatal p

gcreateList :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int String)

hcreateList :: (T.R Int) -> T.RefExp -> T.R String

gcreateList pcreateList p = T.fun1 acreateList pcreateList p hcreateList

hcreateList fn p =
  T.con2 p109v16v109v41 p T.Cons T.aCons (T.conChar p109v16v109v18 p '[')
    (T.uapp2 p109v22v109v41 p109v36v109v37 p (+++) (*++)
      (T.app1 p109v22v109v34 p109v22v109v32 p acreateList' hcreateList' fn)
      (T.fromLitString p109v39v109v41 p "]"))

gcreateList' :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int String)

hcreateList' :: (T.R Int) -> T.RefExp -> T.R String

gcreateList' pcreateList' p = T.fun1 acreateList' pcreateList' p hcreateList'

hcreateList' fv117v13v117v13n p =
  T.cguard p117v13v117v13 p
    (T.ap2 p117v13v117v13 p (p117v13v117v13 Hat.Prelude.!== p) fv117v13v117v13n
      (T.ap1 p117v13v117v13 p (Hat.PreludeBasic.gfromInteger p117v13v117v13 p)
        (T.conInteger p117v13v117v13 p 1))) (\ p -> h p)
    (\ p -> y1createList' fv117v13v117v13n p)
  where
  
  h p = T.fromLitString p117v17v117v24 p "\"e1\""
  h p = y1createList' fv117v13v117v13n p
  
hcreateList' fv117v13v117v13n p = y1createList' fv117v13v117v13n p

y1createList' fn p =
  T.uapp2 p118v18v118v62 p118v44v118v45 p (+++) (*++)
    (T.con2 p118v18v118v41 p T.Cons T.aCons (T.conChar p118v18v118v21 p '"')
      (T.con2 p118v23v118v41 p T.Cons T.aCons (T.conChar p118v23v118v25 p 'e')
        (T.uapp2 p118v28v118v41 p118v35v118v36 p (+++) (*++)
          (T.ap1 p118v28v118v33 p (gshow p118v28v118v31 p) fn)
          (T.fromLitString p118v37v118v41 p "\","))))
    (T.app1 p118v47v118v62 p118v47v118v57 p acreateList' hcreateList'
      (T.ap2 p118v60v118v62 p (p118v61v118v61 !- p) fn
        (T.ap1 p118v62v118v62 p (Hat.PreludeBasic.gfromInteger p118v62v118v62 p)
          (T.conInteger p118v62v118v62 p 1))))

gwriteT2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun String
          (T.Fun (T.List String)
            (T.Fun (NTree Cell) (T.Fun PatternObjects (IO T.Tuple0)))))

hwriteT2 ::
  (T.R String) ->
    (T.R (T.List String)) ->
      (T.R (NTree Cell)) ->
        (T.R PatternObjects) -> T.RefExp -> T.R (IO T.Tuple0)

gwriteT2 pwriteT2 p = T.fun4 awriteT2 pwriteT2 p hwriteT2

hwriteT2 ffile fnotions ftree fobjects p =
  T.uapp2 p131v37v132v121 p131v52v131v52 p (+$) (*$)
    (T.ap1 p131v37v131v50 p (gwriteFile p131v37v131v45 p) ffile)
    (T.uapp2 p131v54v132v121 p131v67v131v68 p (+++) (*++)
      (T.ap1 p131v54v131v65 p (gshow p131v54v131v57 p) fnotions)
      (T.uapp2 p131v70v132v121 p131v84v131v85 p (+++) (*++)
        (T.fromLitString p131v70v131v82 p "\n\nscript=")
        (T.uapp2 p132v10v132v121 p132v25v132v26 p (+++) (*++)
          (T.app1 p132v10v132v22 p132v10v132v17 p ashowTree hshowTree ftree)
          (T.uapp2 p132v28v132v121 p132v35v132v36 p (+++) (*++)
            (T.fromLitString p132v28v132v33 p "\n\n")
            (T.uapp2 p132v39v132v121 p132v60v132v61 p (+++) (*++)
              (T.app1 p132v39v132v57 p132v39v132v49 p ashowObjects hshowObjects
                fobjects)
              (T.uapp2 p132v63v132v121 p132v85v132v86 p (+++) (*++)
                (T.fromLitString p132v63v132v83 p "\n\nteacherNotes = ")
                (T.uapp2 p132v89v132v121 p132v94v132v94 p (+$) (*$)
                  (gshow p132v89v132v92 p)
                  (T.uapp2 p132v96v132v121 p132v96v132v104 p areplicate
                    hreplicate
                    (T.app1 p132v107v132v117 p132v107v132v112 p anbLeaf hnbLeaf
                      ftree) (T.fromLitString p132v120v132v121 p "")))))))))

gwriteT2Err ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun String (T.Fun (T.List (Possible a)) (IO T.Tuple0)))

hwriteT2Err ::
  (T.R String) -> (T.R (T.List (Possible a))) -> T.RefExp -> T.R (IO T.Tuple0)

gwriteT2Err pwriteT2Err p = T.fun2 awriteT2Err pwriteT2Err p hwriteT2Err

hwriteT2Err ffile ferrs p =
  T.uapp2 p140v24v140v51 p140v39v140v39 p (+$) (*$)
    (T.ap1 p140v24v140v37 p (gwriteFile p140v24v140v32 p) ffile)
    (T.app1 p140v41v140v51 p140v41v140v46 p aerrors herrors ferrs)

gshowObjects :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun PatternObjects String)

hshowObjects :: (T.R PatternObjects) -> T.RefExp -> T.R String

gshowObjects pshowObjects p = T.fun1 ashowObjects pshowObjects p hshowObjects

hshowObjects (T.R (T.Tuple5 fa fg fp fr fro) _) p =
  T.uapp2 p148v28v149v129 p148v53v148v54 p (+++) (*++)
    (T.fromLitString p148v28v148v51 p "activityObjectsList = ")
    (T.uapp2 p148v57v149v129 p148v65v148v66 p (+++) (*++)
      (T.ap1 p148v57v148v62 p (gshow p148v57v148v60 p) fa)
      (T.uapp2 p148v68v149v129 p148v92v148v93 p (+++) (*++)
        (T.fromLitString p148v68v148v90 p "\ngroupObjectsList = ")
        (T.uapp2 p148v96v149v129 p148v104v148v105 p (+++) (*++)
          (T.ap1 p148v96v148v101 p (gshow p148v96v148v99 p) fg)
          (T.uapp2 p149v9v149v129 p149v39v149v40 p (+++) (*++)
            (T.fromLitString p149v9v149v37 p "\nparticipantObjectsList = ")
            (T.uapp2 p149v43v149v129 p149v51v149v52 p (+++) (*++)
              (T.ap1 p149v43v149v48 p (gshow p149v43v149v46 p) fp)
              (T.uapp2 p149v54v149v129 p149v81v149v82 p (+++) (*++)
                (T.fromLitString p149v54v149v79 p "\nresourceObjectsList = ")
                (T.uapp2 p149v85v149v129 p149v93v149v94 p (+++) (*++)
                  (T.ap1 p149v85v149v90 p (gshow p149v85v149v88 p) fr)
                  (T.uapp2 p149v96v149v129 p149v119v149v120 p (+++) (*++)
                    (T.fromLitString p149v96v149v117 p "\nroleObjectsList = ")
                    (T.ap1 p149v123v149v129 p (gshow p149v123v149v126 p)
                      fro)))))))))
hshowObjects _ p = T.fatal p

gerrors :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (Possible a)) String)

herrors :: (T.R (T.List (Possible a))) -> T.RefExp -> T.R String

gerrors perrors p = T.fun1 aerrors perrors p herrors

herrors ferr p =
  T.uapp2 p157v14v157v53 p157v24v157v25 p (+++) (*++)
    (T.fromLitString p157v14v157v22 p "Error=[")
    (T.uapp2 p157v28v157v53 p157v46v157v47 p (+++) (*++)
      (T.app2 p157v28v157v43 p157v28v157v34 p aerrors' herrors' ferr
        (T.con0 p157v40v157v43 p True aTrue))
      (T.fromLitString p157v49v157v53 p "]\n"))

gerrors' ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.List (Possible a)) (T.Fun Bool String))

herrors' :: (T.R (T.List (Possible a))) -> (T.R Bool) -> T.RefExp -> T.R String

gerrors' perrors' p = T.fun2 aerrors' perrors' p herrors'

herrors' (T.R T.List _) _ p = T.fromLitString p163v16v163v17 p ""
herrors' (T.R (T.Cons (T.R (NotPossible fmsg) _) fes) _) ffirst p =
  T.uapp2 p164v39v164v105 p164v72v164v73 p (+++) (*++)
    (T.cif p164v39v164v69 p ffirst
      (\ p -> T.fromLitString p164v55v164v58 p "\"")
      (\ p -> T.fromLitString p164v65v164v69 p ",\""))
    (T.uapp2 p164v75v164v105 p164v79v164v80 p (+++) (*++) fmsg
      (T.uapp2 p164v82v164v105 p164v87v164v88 p (+++) (*++)
        (T.fromLitString p164v82v164v85 p "\"")
        (T.app2 p164v90v164v105 p164v90v164v96 p aerrors' herrors' fes
          (T.con0 p164v101v164v105 p False aFalse))))
herrors' _ _ p = T.fatal p

greadHeader :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String String)

hreadHeader :: (T.R String) -> T.RefExp -> T.R String

greadHeader preadHeader p = T.fun1 areadHeader preadHeader p hreadHeader

hreadHeader fcontent p =
  T.uapp2 p171v22v171v41 p171v27v171v27 p (+$) (*$) (ghead p171v22v171v25 p)
    (T.uapp1 p171v29v171v41 p171v29v171v33 p alines hlines fcontent)

gstripHeader :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String String)

hstripHeader :: (T.R String) -> T.RefExp -> T.R String

gstripHeader pstripHeader p = T.fun1 astripHeader pstripHeader p hstripHeader

hstripHeader ftext p =
  T.uapp2 p179v20v179v77 p179v27v179v27 p (+$) (*$)
    (T.ap1 p179v20v179v25 p (gdrop p179v20v179v23 p)
      (T.ap1 p179v25v179v25 p (Hat.PreludeBasic.gfromInteger p179v25v179v25 p)
        (T.conInteger p179v25v179v25 p 7)))
    (T.uapp2 p179v29v179v77 p179v57v179v57 p (+$) (*$)
      (T.uapp1 p179v29v179v54 p179v29v179v37 p aconcatMap hconcatMap
        (T.fun1 T.mkLambda p179v40v179v54 p
          (\ fl p ->
            T.uapp2 p179v46v179v54 p179v48v179v49 p (+++) (*++) fl
              (T.fromLitString p179v51v179v54 p "\n"))))
      (T.uapp2 p179v59v179v77 p179v66v179v66 p (+$) (*$)
        (T.ap1 p179v59v179v64 p (gdrop p179v59v179v62 p)
          (T.ap1 p179v64v179v64 p
            (Hat.PreludeBasic.gfromInteger p179v64v179v64 p)
            (T.conInteger p179v64v179v64 p 2)))
        (T.uapp1 p179v68v179v77 p179v68v179v72 p alines hlines ftext)))

gstripFooter ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List String) (T.List String))

hstripFooter :: (T.R (T.List String)) -> T.RefExp -> T.R (T.List String)

gstripFooter pstripFooter p = T.fun1 astripFooter pstripFooter p hstripFooter

hstripFooter (T.R T.List _) p = T.con0 p186v18v186v19 p T.List T.aList
hstripFooter (T.R (T.Cons fl fls) _) p =
  T.cguard p188v11v188v17 p
    (T.ap2 p188v11v188v17 p (p188v13v188v14 !== p) fl
      (T.fromLitString p188v16v188v17 p ""))
    (\ p -> T.con0 p188v21v188v22 p T.List T.aList)
    (\ p ->
      T.cguard p189v11v189v19 p (gotherwise p189v11v189v19 p)
        (\ p ->
          T.con2 p189v23v189v41 p T.Cons T.aCons fl
            (T.app1 p189v28v189v41 p189v28v189v38 p astripFooter hstripFooter
              fls)) (\ p -> T.fatal p))
hstripFooter _ p = T.fatal p

greadTree :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO (NTree Cell)))

hreadTree :: (T.R String) -> T.RefExp -> T.R (IO (NTree Cell))

greadTree preadTree p = T.fun1 areadTree preadTree p hreadTree

hreadTree ffile p =
  T.ap2 p194v20v194v32 p (p194v20v194v32 Hat.Prelude.!>>= p)
    (T.uapp1 p194v20v194v32 p194v20v194v27 p areadFile hreadFile ffile)
    (T.fun1 T.mkDoLambda p194v20v194v32 p
      (\ fcontent p ->
        T.ap1 p195v9v195v73 p (greturn p195v9v195v14 p)
          (T.uapp2 p195v17v195v73 p195v22v195v22 p (+$) (*$)
            (gread p195v17v195v20 p)
            (T.uapp2 p195v24v195v73 p195v31v195v31 p (+$) (*$)
              (gconcat p195v24v195v29 p)
              (T.uapp2 p195v33v195v73 p195v45v195v45 p (+$) (*$)
                (gstripFooter p195v33v195v43 p)
                (T.uapp2 p195v47v195v73 p195v53v195v53 p (+$) (*$)
                  (glines p195v47v195v51 p)
                  (T.app1 p195v55v195v73 p195v55v195v65 p astripHeader
                    hstripHeader fcontent)))))))

greadObjects ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (T.Tuple2 String PatternObjects))

hreadObjects :: (T.R String) -> T.RefExp -> T.R (T.Tuple2 String PatternObjects)

greadObjects preadObjects p = T.fun1 areadObjects preadObjects p hreadObjects

hreadObjects ffirstLine p =
  T.uapp1 p202v25v202v38 p202v25v202v28 p aread hread ffirstLine
    :: T.R (T.Tuple2 String PatternObjects)

greadConstraints ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO (T.List Cstr)))

hreadConstraints :: (T.R String) -> T.RefExp -> T.R (IO (T.List Cstr))

greadConstraints preadConstraints p =
  T.fun1 areadConstraints preadConstraints p hreadConstraints

hreadConstraints ffile p =
  T.ap2 p210v20v210v32 p (p210v20v210v32 Hat.Prelude.!>>= p)
    (T.uapp1 p210v20v210v32 p210v20v210v27 p areadFile hreadFile ffile)
    (T.fun1 T.mkDoLambda p210v20v210v32 p
      (\ fcontent p ->
        T.ap1 p211v9v211v57 p (greturn p211v9v211v14 p)
          (T.uapp2 p211v17v211v57 p211v17v211v19 p amap hmap
            (T.fun1 T.mkLambda p211v22v211v41 p
              (\ fl p ->
                T.uapp1 p211v28v211v33 p211v28v211v31 p aread hread fl
                  :: T.R Cstr))
            (T.uapp1 p211v45v211v57 p211v45v211v49 p alines hlines fcontent))))

gsplitList ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.List a) (T.Fun (T.List Int) (T.List (T.List a))))

hsplitList ::
  (T.R (T.List a)) -> (T.R (T.List Int)) -> T.RefExp -> T.R (T.List (T.List a))

gsplitList psplitList p = T.fun2 asplitList psplitList p hsplitList

hsplitList (T.R T.List _) _ p = T.con0 p221v18v221v19 p T.List T.aList
hsplitList flist (T.R (T.Cons fn fnumbers) _) p =
  T.con2 p222v30v222v60 p T.Cons T.aCons (gfirst p222v30v222v34 p)
    (T.app2 p222v39v222v60 p222v39v222v47 p asplitList hsplitList
      (grest p222v49v222v52 p) fnumbers)
  where
  
  gfirst pfirst p = T.constUse pfirst p sfirst
  
  grest pfirst p = T.constUse pfirst p srest
  
  j224v17v224v29first =
    case T.uapp2 p224v33v224v46 p224v33v224v39 p asplitAt hsplitAt fn flist of
      T.R (T.Tuple2 ffirst frest) kfirst -> (kfirst,ffirst,frest)
      _ -> T.fatal p
  
  sfirst =
    T.constDef p a224v18v224v22first
      (\ _ ->
        case j224v17v224v29first of
          (kfirst,ffirst,frest) -> T.projection p224v18v224v22 kfirst ffirst)
  
  srest =
    T.constDef p a224v25v224v28rest
      (\ _ ->
        case j224v17v224v29first of
          (kfirst,ffirst,frest) -> T.projection p224v25v224v28 kfirst frest)
  
hsplitList _ _ p = T.fatal p

gsplitList2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (T.List a) (T.Fun Int (T.Fun Int (T.List (T.List a)))))

hsplitList2 ::
  (T.R (T.List a)) ->
    (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (T.List (T.List a))

gsplitList2 psplitList2 p = T.fun3 asplitList2 psplitList2 p hsplitList2

hsplitList2 (T.R T.List _) _ _ p = T.con0 p234v21v234v22 p T.List T.aList
hsplitList2 flist fnb fsize p =
  T.app3 p235v27v235v58 p235v27v235v37 p asplitList2' hsplitList2'
    (T.uapp1 p235v40v235v49 p235v40v235v44 p acycle hcycle flist) fnb fsize

gsplitList2' ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (T.List a) (T.Fun Int (T.Fun Int (T.List (T.List a)))))

hsplitList2' ::
  (T.R (T.List a)) ->
    (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (T.List (T.List a))

gsplitList2' psplitList2' p = T.fun3 asplitList2' psplitList2' p hsplitList2'

hsplitList2' (z1splitList2'@_) fv241v15v241v15n v241v17v241v17n p =
  T.cguard p241v13v241v17 p
    (T.ap2 p241v15v241v15 p (p241v15v241v15 Hat.Prelude.!== p) fv241v15v241v15n
      (T.ap1 p241v15v241v15 p (Hat.PreludeBasic.gfromInteger p241v15v241v15 p)
        (T.conInteger p241v15v241v15 p 0))) (\ p -> h v241v17v241v17n p)
    (\ p -> y1splitList2' z1splitList2' fv241v15v241v15n v241v17v241v17n p)
  where
  
  h _ p = T.con0 p241v21v241v22 p T.List T.aList
  h _ p = y1splitList2' z1splitList2' fv241v15v241v15n v241v17v241v17n p
  
hsplitList2' z1splitList2' fv241v15v241v15n v241v17v241v17n p =
  y1splitList2' z1splitList2' fv241v15v241v15n v241v17v241v17n p

y1splitList2' flist fnb fsize p =
  T.con2 p242v28v242v84 p T.Cons T.aCons
    (T.uapp2 p242v28v242v41 p242v28v242v31 p atake htake fsize flist)
    (T.app3 p242v45v242v84 p242v45v242v55 p asplitList2' hsplitList2'
      (T.uapp2 p242v58v242v71 p242v58v242v61 p adrop hdrop fsize flist)
      (T.ap2 p242v75v242v78 p (p242v77v242v77 !- p) fnb
        (T.ap1 p242v78v242v78 p (Hat.PreludeBasic.gfromInteger p242v78v242v78 p)
          (T.conInteger p242v78v242v78 p 1))) fsize)

gpossibleToList ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Possible (T.List a)) (T.List a))

hpossibleToList :: (T.R (Possible (T.List a))) -> T.RefExp -> T.R (T.List a)

gpossibleToList ppossibleToList p =
  T.fun1 apossibleToList ppossibleToList p hpossibleToList

hpossibleToList fp p =
  T.ccase p247v20v247v71 p
    (let
      v247v20v247v71v1 (T.R (NotPossible _) _) p =
        T.con0 p247v47v247v48 p T.List T.aList
      v247v20v247v71v1 (T.R (Possible flist) _) p =
        T.projection p247v68v247v71 p flist
      v247v20v247v71v1 _ p = T.fatal p in (v247v20v247v71v1)) fp

grepartition ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int (T.Fun Int (T.Fun Int (T.Fun Int (Possible (T.List Int))))))

hrepartition ::
  (T.R Int) ->
    (T.R Int) ->
      (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (Possible (T.List Int))

grepartition prepartition p = T.fun4 arepartition prepartition p hrepartition

hrepartition fnbP fn fa fb p =
  T.cguard p259v11v259v12 p (gok p259v11v259v12 p)
    (\ p ->
      T.con1 p259v16v259v29 p Possible aPossible (glist1 p259v25v259v29 p))
    (\ p ->
      T.cguard p260v11v260v13 p (gok2 p260v11v260v13 p)
        (\ p ->
          T.con1 p260v17v260v30 p Possible aPossible (glist2 p260v26v260v30 p))
        (\ p ->
          T.cguard p261v11v261v19 p (gotherwise p261v11v261v19 p)
            (\ p ->
              T.con1 p261v23v261v63 p NotPossible aNotPossible
                (T.fromLitString p261v35v261v63 p
                  "Can't do a good repartition")) (\ p -> T.fatal p)))
  where
  
  gok pok p = T.constUse pok p sok
  
  glist1 pok p = T.constUse pok p slist1
  
  j263v17v263v27ok =
    case
      T.app5 p263v31v263v60 p263v31v263v48 p arepartitionUniform
        hrepartitionUniform fnbP fn fa fb
        (T.ap1 p263v60v263v60 p (Hat.PreludeBasic.gfromInteger p263v60v263v60 p)
          (T.conInteger p263v60v263v60 p 0)) of
      T.R (T.Tuple2 fok flist1) kok -> (kok,fok,flist1)
      _ -> T.fatal p
  
  sok =
    T.constDef p a263v18v263v19ok
      (\ _ ->
        case j263v17v263v27ok of
          (kok,fok,flist1) -> T.projection p263v18v263v19 kok fok)
  
  slist1 =
    T.constDef p a263v22v263v26list1
      (\ _ ->
        case j263v17v263v27ok of
          (kok,fok,flist1) -> T.projection p263v22v263v26 kok flist1)
  
  gok2 pok2 p = T.constUse pok2 p sok2
  
  glist2 pok2 p = T.constUse pok2 p slist2
  
  j264v17v264v28ok2 =
    case
      T.app5 p264v32v264v55 p264v32v264v43 p arepartition' hrepartition' fnbP fn
        fa fb
        (T.ap1 p264v55v264v55 p (Hat.PreludeBasic.gfromInteger p264v55v264v55 p)
          (T.conInteger p264v55v264v55 p 0)) of
      T.R (T.Tuple2 fok2 flist2) kok2 -> (kok2,fok2,flist2)
      _ -> T.fatal p
  
  sok2 =
    T.constDef p a264v18v264v20ok2
      (\ _ ->
        case j264v17v264v28ok2 of
          (kok2,fok2,flist2) -> T.projection p264v18v264v20 kok2 fok2)
  
  slist2 =
    T.constDef p a264v23v264v27list2
      (\ _ ->
        case j264v17v264v28ok2 of
          (kok2,fok2,flist2) -> T.projection p264v23v264v27 kok2 flist2)
  

grepartition' ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun Int
            (T.Fun Int (T.Fun Int (T.Fun Int (T.Tuple2 Bool (T.List Int)))))))

hrepartition' ::
  (T.R Int) ->
    (T.R Int) ->
      (T.R Int) ->
        (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (T.Tuple2 Bool (T.List Int))

grepartition' prepartition' p =
  T.fun5 arepartition' prepartition' p hrepartition'

hrepartition' fnbP fn fa fb fm p =
  T.cguard p276v11v276v17 p
    (T.ap2 p276v11v276v17 p (p276v15v276v15 !> p) (gsub p276v11v276v13 p)
      (T.ap1 p276v17v276v17 p (Hat.PreludeBasic.gfromInteger p276v17v276v17 p)
        (T.conInteger p276v17v276v17 p 0)))
    (\ p ->
      let
        gok pok p = T.constUse pok p sok
        glist pok p = T.constUse pok p slist
        j276v25v276v34ok =
          case
            T.app5 p276v38v276v61 p276v38v276v49 p arepartition' hrepartition'
              (gsub p276v51v276v53 p) fn fa fb fm of
            T.R (T.Tuple2 fok flist) kok -> (kok,fok,flist)
            _ -> T.fatal p
        sok =
          T.constDef p a276v26v276v27ok
            (\ _ ->
              case j276v25v276v34ok of
                (kok,fok,flist) -> T.projection p276v26v276v27 kok fok)
        slist =
          T.constDef p a276v30v276v33list
            (\ _ ->
              case j276v25v276v34ok of
                (kok,fok,flist) -> T.projection p276v30v276v33 kok flist) in
        (T.cif p276v66v276v103 p (gok p276v69v276v70 p)
          (\ p ->
            T.con2 p276v77v276v89 p T.Tuple2 T.aTuple2 (gok p276v78v276v79 p)
              (T.con2 p276v82v276v88 p T.Cons T.aCons (gnb p276v82v276v83 p)
                (glist p276v85v276v88 p)))
          (\ p -> gdecrease p276v96v276v103 p)))
    (\ p ->
      T.cguard p277v11v277v18 p
        (T.ap2 p277v11v277v18 p (p277v15v277v16 !== p) (gsub p277v11v277v13 p)
          (T.ap1 p277v18v277v18 p
            (Hat.PreludeBasic.gfromInteger p277v18v277v18 p)
            (T.conInteger p277v18v277v18 p 0)))
        (\ p ->
          T.con2 p277v22v277v33 p T.Tuple2 T.aTuple2
            (T.con0 p277v23v277v26 p True aTrue)
            (T.fromExpList p277v29v277v32 p [gnb p277v30v277v31 p]))
        (\ p ->
          T.cguard p278v11v278v19 p (gotherwise p278v11v278v19 p)
            (\ p -> gdecrease p278v23v278v30 p) (\ p -> T.fatal p)))
  where
  
  gnb pnb p = T.constUse pnb p snb
  
  snb =
    T.constDef p a280v17v280v24nb
      (\ p -> T.ap2 p280v22v280v24 p (p280v23v280v23 !- p) fn fm)
  
  gsub psub p = T.constUse psub p ssub
  
  ssub =
    T.constDef p a281v17v281v28sub
      (\ p ->
        T.ap2 p281v23v281v28 p (p281v26v281v26 !- p) fnbP
          (gnb p281v27v281v28 p))
  
  gdecrease pdecrease p = T.constUse pdecrease p sdecrease
  
  sdecrease =
    T.constDef p a282v17v282v94decrease
      (\ p ->
        T.cif p282v28v282v94 p
          (T.uapp2 p282v31v282v41 p282v35v282v36 p (+&&) (*&&)
            (T.ap2 p282v31v282v33 p (p282v32v282v32 !< p) fm fb)
            (T.ap2 p282v38v282v41 p (p282v40v282v40 !> p) (gnb p282v38v282v39 p)
              (T.ap1 p282v41v282v41 p
                (Hat.PreludeBasic.gfromInteger p282v41v282v41 p)
                (T.conInteger p282v41v282v41 p 2))))
          (\ p ->
            T.app5 p282v49v282v75 p282v49v282v60 p arepartition' hrepartition'
              fnbP fn fa fb
              (T.ap2 p282v73v282v75 p (p282v74v282v74 !+ p) fm
                (T.ap1 p282v75v282v75 p
                  (Hat.PreludeBasic.gfromInteger p282v75v282v75 p)
                  (T.conInteger p282v75v282v75 p 1))))
          (\ p ->
            T.con2 p282v84v282v94 p T.Tuple2 T.aTuple2
              (T.con0 p282v85v282v89 p False aFalse)
              (T.con0 p282v92v282v93 p T.List T.aList)))
  

grepartitionUniform ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun Int
            (T.Fun Int (T.Fun Int (T.Fun Int (T.Tuple2 Bool (T.List Int)))))))

hrepartitionUniform ::
  (T.R Int) ->
    (T.R Int) ->
      (T.R Int) ->
        (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (T.Tuple2 Bool (T.List Int))

grepartitionUniform prepartitionUniform p =
  T.fun5 arepartitionUniform prepartitionUniform p hrepartitionUniform

hrepartitionUniform fnbP fn fa fb fm p =
  T.cguard p294v11v294v33 p
    (T.uapp2 p294v11v294v33 p294v16v294v17 p (+&&) (*&&)
      (T.ap2 p294v11v294v14 p (p294v12v294v13 !<= p) fm fa)
      (T.ap2 p294v19v294v33 p (p294v30v294v31 !== p)
        (T.ap2 p294v19v294v28 p (gmod p294v19v294v21 p) fnbP
          (gnb p294v27v294v28 p))
        (T.ap1 p294v33v294v33 p (Hat.PreludeBasic.gfromInteger p294v33v294v33 p)
          (T.conInteger p294v33v294v33 p 0))))
    (\ p ->
      T.con2 p294v37v294v69 p T.Tuple2 T.aTuple2
        (T.con0 p294v38v294v41 p True aTrue)
        (T.uapp2 p294v44v294v68 p294v44v294v52 p areplicate hreplicate
          (T.ap2 p294v55v294v64 p (gdiv p294v55v294v57 p) fnbP
            (gnb p294v63v294v64 p)) (gnb p294v67v294v68 p)))
    (\ p ->
      T.cguard p295v11v295v42 p
        (T.uapp2 p295v11v295v42 p295v16v295v17 p (+&&) (*&&)
          (T.ap2 p295v11v295v14 p (p295v12v295v13 !<= p) fm fb)
          (T.uapp2 p295v19v295v42 p295v24v295v25 p (+&&) (*&&)
            (T.ap2 p295v19v295v22 p (p295v21v295v21 !> p) (gnb p295v19v295v20 p)
              (T.ap1 p295v22v295v22 p
                (Hat.PreludeBasic.gfromInteger p295v22v295v22 p)
                (T.conInteger p295v22v295v22 p 1)))
            (T.ap2 p295v27v295v42 p (p295v39v295v40 !== p)
              (T.ap2 p295v27v295v37 p (gmod p295v27v295v29 p) fnbP
                (gnb1 p295v35v295v37 p))
              (T.ap1 p295v42v295v42 p
                (Hat.PreludeBasic.gfromInteger p295v42v295v42 p)
                (T.conInteger p295v42v295v42 p 0)))))
        (\ p ->
          T.con2 p295v46v295v80 p T.Tuple2 T.aTuple2
            (T.con0 p295v47v295v50 p True aTrue)
            (T.uapp2 p295v53v295v79 p295v53v295v61 p areplicate hreplicate
              (T.ap2 p295v64v295v74 p (gdiv p295v64v295v66 p) fnbP
                (gnb1 p295v72v295v74 p)) (gnb1 p295v77v295v79 p)))
        (\ p ->
          T.cguard p296v11v296v20 p
            (T.uapp2 p296v11v296v20 p296v15v296v16 p (+||) (*||)
              (T.ap2 p296v11v296v13 p (p296v12v296v12 !< p) fm fa)
              (T.ap2 p296v18v296v20 p (p296v19v296v19 !< p) fm fb))
            (\ p ->
              T.app5 p296v24v296v56 p296v24v296v41 p arepartitionUniform
                hrepartitionUniform fnbP fn fa fb
                (T.ap2 p296v54v296v56 p (p296v55v296v55 !+ p) fm
                  (T.ap1 p296v56v296v56 p
                    (Hat.PreludeBasic.gfromInteger p296v56v296v56 p)
                    (T.conInteger p296v56v296v56 p 1))))
            (\ p ->
              T.cguard p297v11v297v19 p (gotherwise p297v11v297v19 p)
                (\ p ->
                  T.con2 p297v23v297v33 p T.Tuple2 T.aTuple2
                    (T.con0 p297v24v297v28 p False aFalse)
                    (T.con0 p297v31v297v32 p T.List T.aList))
                (\ p -> T.fatal p))))
  where
  
  gnb pnb p = T.constUse pnb p snb
  
  snb =
    T.constDef p a299v17v299v24nb
      (\ p -> T.ap2 p299v22v299v24 p (p299v23v299v23 !+ p) fn fm)
  
  gnb1 pnb1 p = T.constUse pnb1 p snb1
  
  snb1 =
    T.constDef p a300v17v300v25nb1
      (\ p -> T.ap2 p300v23v300v25 p (p300v24v300v24 !- p) fn fm)
  

grepartition2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun (T.List (T.Tuple3 Int Int Int)) (Possible (T.List Int))))

hrepartition2 ::
  (T.R Int) ->
    (T.R (T.List (T.Tuple3 Int Int Int))) ->
      T.RefExp -> T.R (Possible (T.List Int))

grepartition2 prepartition2 p =
  T.fun2 arepartition2 prepartition2 p hrepartition2

hrepartition2 fnbP fgroups p =
  T.cguard p311v11v311v24 p
    (T.ap2 p311v11v311v24 p (p311v19v311v20 !== p) (gsumSize p311v11v311v17 p)
      fnbP)
    (\ p ->
      T.con1 p311v28v311v46 p Possible aPossible (gsizeGroups p311v37v311v46 p))
    (\ p ->
      T.cguard p312v12v312v87 p
        (T.uapp2 p312v12v312v87 p312v48v312v49 p (+||) (*||)
          (T.uapp2 p312v12v312v45 p312v24v312v25 p (+&&) (*&&)
            (T.ap2 p312v12v312v22 p (p312v20v312v20 !> p)
              (gdiffSum p312v12v312v18 p)
              (T.ap1 p312v22v312v22 p
                (Hat.PreludeBasic.gfromInteger p312v22v312v22 p)
                (T.conInteger p312v22v312v22 p 0)))
            (T.ap2 p312v27v312v45 p (p312v35v312v36 !<= p)
              (gdiffSum p312v27v312v33 p) (gsumAbove p312v38v312v45 p)))
          (T.uapp2 p312v52v312v87 p312v64v312v65 p (+&&) (*&&)
            (T.ap2 p312v52v312v62 p (p312v60v312v60 !< p)
              (gdiffSum p312v52v312v58 p)
              (T.ap1 p312v62v312v62 p
                (Hat.PreludeBasic.gfromInteger p312v62v312v62 p)
                (T.conInteger p312v62v312v62 p 0)))
            (T.ap2 p312v67v312v87 p (p312v75v312v76 !>= p)
              (gdiffSum p312v67v312v73 p)
              (T.ap1 p312v79v312v87 p (gnegate p312v79v312v79 p)
                (gsumBelow p312v80v312v87 p)))))
        (\ p ->
          T.uapp2 p312v93v312v135 p312v102v312v102 p (+$) (*$)
            (T.pa0 Possible T.cn1 p312v93v312v100 p aPossible)
            (T.app3 p312v104v312v135 p312v104v312v116 p arepartition2'
              hrepartition2' fnbP (gdiffSum p312v122v312v128 p) fgroups))
        (\ p ->
          T.cguard p313v11v313v19 p (gotherwise p313v11v313v19 p)
            (\ p ->
              T.con1 p313v23v313v60 p NotPossible aNotPossible
                (T.fromLitString p313v35v313v60 p "repartition not possible"))
            (\ p -> T.fatal p)))
  where
  
  gsizeGroups psizeGroups p = T.constUse psizeGroups p ssizeGroups
  
  ssizeGroups =
    T.constDef p a315v17v315v57sizeGroups
      (\ p ->
        T.uapp2 p315v30v315v57 p315v30v315v32 p amap hmap
          (T.fun1 T.mkLambda p315v35v315v49 p
            (\ v315v35v315v49v1 p ->
              case (v315v35v315v49v1) of
                (T.R (T.Tuple3 fnb _ _) _) -> T.projection p315v48v315v49 p fnb
                _ -> T.fatal p)) fgroups)
  
  gsumSize psumSize p = T.constUse psumSize p ssumSize
  
  gsumAbove psumSize p = T.constUse psumSize p ssumAbove
  
  gsumBelow psumSize p = T.constUse psumSize p ssumBelow
  
  j316v17v316v45sumSize =
    case
      T.uapp3 p316v49v316v110 p316v49v316v53 p afoldl hfoldl
        (T.fun2 T.mkLambda p316v56v316v94 p
          (\ v316v56v316v94v1 v316v56v316v94v2 p ->
            case (v316v56v316v94v1,v316v56v316v94v2) of
              (T.R (T.Tuple3 fs fa fb) _,T.R (T.Tuple3 fs' fa' fb') _) ->
                T.con3 p316v79v316v94 p T.Tuple3 T.aTuple3
                  (T.ap2 p316v80v316v83 p (p316v81v316v81 !+ p) fs fs')
                  (T.ap2 p316v85v316v88 p (p316v86v316v86 !+ p) fa fa')
                  (T.ap2 p316v90v316v93 p (p316v91v316v91 !+ p) fb fb')
              _ -> T.fatal p))
        (T.con3 p316v97v316v103 p T.Tuple3 T.aTuple3
          (T.ap1 p316v98v316v98 p
            (Hat.PreludeBasic.gfromInteger p316v98v316v98 p)
            (T.conInteger p316v98v316v98 p 0))
          (T.ap1 p316v100v316v100 p
            (Hat.PreludeBasic.gfromInteger p316v100v316v100 p)
            (T.conInteger p316v100v316v100 p 0))
          (T.ap1 p316v102v316v102 p
            (Hat.PreludeBasic.gfromInteger p316v102v316v102 p)
            (T.conInteger p316v102v316v102 p 0))) fgroups of
      T.R (T.Tuple3 fsumSize fsumAbove fsumBelow) ksumSize ->
        (ksumSize,fsumSize,fsumAbove,fsumBelow)
      _ -> T.fatal p
  
  ssumSize =
    T.constDef p a316v18v316v24sumSize
      (\ _ ->
        case j316v17v316v45sumSize of
          (ksumSize,fsumSize,fsumAbove,fsumBelow) ->
            T.projection p316v18v316v24 ksumSize fsumSize)
  
  ssumAbove =
    T.constDef p a316v27v316v34sumAbove
      (\ _ ->
        case j316v17v316v45sumSize of
          (ksumSize,fsumSize,fsumAbove,fsumBelow) ->
            T.projection p316v27v316v34 ksumSize fsumAbove)
  
  ssumBelow =
    T.constDef p a316v37v316v44sumBelow
      (\ _ ->
        case j316v17v316v45sumSize of
          (ksumSize,fsumSize,fsumAbove,fsumBelow) ->
            T.projection p316v37v316v44 ksumSize fsumBelow)
  
  gdiffSum pdiffSum p = T.constUse pdiffSum p sdiffSum
  
  sdiffSum =
    T.constDef p a317v17v317v39diffSum
      (\ p ->
        T.ap2 p317v27v317v39 p (p317v31v317v31 !- p) fnbP
          (gsumSize p317v33v317v39 p))
  

grepartition2' ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun Int (T.Fun (T.List (T.Tuple3 Int Int Int)) (T.List Int))))

hrepartition2' ::
  (T.R Int) ->
    (T.R Int) ->
      (T.R (T.List (T.Tuple3 Int Int Int))) -> T.RefExp -> T.R (T.List Int)

grepartition2' prepartition2' p =
  T.fun3 arepartition2' prepartition2' p hrepartition2'

hrepartition2' _ _ (T.R T.List _) p = T.con0 p324v24v324v25 p T.List T.aList
hrepartition2' fnbP fdiff
  (T.R (T.Cons (T.R (T.Tuple3 fnbG faG fbG) _) fgroups) _) p =
  T.con2 p325v47v325v97 p T.Cons T.aCons (gsize p325v47v325v50 p)
    (T.app3 p325v54v325v97 p325v54v325v66 p arepartition2' hrepartition2'
      (T.ap2 p325v69v325v76 p (p325v72v325v72 !- p) fnbP
        (gsize p325v73v325v76 p))
      (T.ap2 p325v80v325v89 p (p325v84v325v84 !- p) fdiff
        (gextra p325v85v325v89 p)) fgroups)
  where
  
  gsize psize p = T.constUse psize p ssize
  
  ssize =
    T.constDef p a327v17v327v34size
      (\ p ->
        T.ap2 p327v24v327v34 p (p327v28v327v28 !+ p) fnbG
          (gextra p327v30v327v34 p))
  
  gextra pextra p = T.constUse pextra p sextra
  
  sextra =
    T.constDef p a328v17v331v39extra
      (\ p ->
        T.cguard p329v27v329v44 p
          (T.uapp2 p329v27v329v44 p329v36v329v37 p (+&&) (*&&)
            (T.ap2 p329v27v329v34 p (p329v32v329v32 !> p) fdiff
              (T.ap1 p329v34v329v34 p
                (Hat.PreludeBasic.gfromInteger p329v34v329v34 p)
                (T.conInteger p329v34v329v34 p 0)))
            (T.ap2 p329v39v329v44 p (p329v42v329v42 !> p) faG
              (T.ap1 p329v44v329v44 p
                (Hat.PreludeBasic.gfromInteger p329v44v329v44 p)
                (T.conInteger p329v44v329v44 p 0))))
          (\ p -> T.ap2 p329v48v329v58 p (gmin p329v48v329v50 p) fdiff faG)
          (\ p ->
            T.cguard p330v27v330v44 p
              (T.uapp2 p330v27v330v44 p330v36v330v37 p (+&&) (*&&)
                (T.ap2 p330v27v330v34 p (p330v32v330v32 !< p) fdiff
                  (T.ap1 p330v34v330v34 p
                    (Hat.PreludeBasic.gfromInteger p330v34v330v34 p)
                    (T.conInteger p330v34v330v34 p 0)))
                (T.ap2 p330v39v330v44 p (p330v42v330v42 !> p) fbG
                  (T.ap1 p330v44v330v44 p
                    (Hat.PreludeBasic.gfromInteger p330v44v330v44 p)
                    (T.conInteger p330v44v330v44 p 0))))
              (\ p ->
                T.ap2 p330v48v330v60 p (gmax p330v48v330v50 p) fdiff
                  (T.ap1 p330v58v330v60 p (gnegate p330v58v330v58 p) fbG))
              (\ p ->
                T.cguard p331v27v331v35 p (gotherwise p331v27v331v35 p)
                  (\ p ->
                    T.ap1 p331v39v331v39 p
                      (Hat.PreludeBasic.gfromInteger p331v39v331v39 p)
                      (T.conInteger p331v39v331v39 p 0)) (\ p -> T.fatal p))))
  
hrepartition2' _ _ _ p = T.fatal p

tUtil = T.mkModule "Util" "../../src/Util/Util.hs" Prelude.True

aPossible = T.mkConstructor tUtil 480019 480026 3 1 "Possible"

aNotPossible = T.mkConstructor tUtil 480032 480042 3 1 "NotPossible"

areadInt = T.mkVariable tUtil 580001 580018 3 1 "readInt" Prelude.False

aparticipantsLogins =
  T.mkVariable tUtil 650001 650060 3 1 "participantsLogins" Prelude.False

aresourcesNames =
  T.mkVariable tUtil 700001 700050 3 1 "resourcesNames" Prelude.False

aactivitiesObjects =
  T.mkVariable tUtil 760001 760033 3 1 "activitiesObjects" Prelude.False

agroupsObjects =
  T.mkVariable tUtil 810001 810029 3 1 "groupsObjects" Prelude.False

aparticipantsObjects =
  T.mkVariable tUtil 870001 870035 3 1 "participantsObjects" Prelude.False

aresourcesObjects =
  T.mkVariable tUtil 930001 930032 3 1 "resourcesObjects" Prelude.False

arolesObjects =
  T.mkVariable tUtil 990001 990028 3 1 "rolesObjects" Prelude.False

acreateList = T.mkVariable tUtil 1090001 1090041 3 1 "createList" Prelude.False

acreateList' =
  T.mkVariable tUtil 1170001 1180062 3 1 "createList'" Prelude.False

awriteT2 = T.mkVariable tUtil 1310001 1320121 3 4 "writeT2" Prelude.False

awriteT2Err = T.mkVariable tUtil 1400001 1400051 3 2 "writeT2Err" Prelude.False

ashowObjects =
  T.mkVariable tUtil 1480001 1490129 3 1 "showObjects" Prelude.False

aerrors = T.mkVariable tUtil 1570001 1570053 3 1 "errors" Prelude.False

aerrors' = T.mkVariable tUtil 1630001 1640105 3 2 "errors'" Prelude.False

areadHeader = T.mkVariable tUtil 1710001 1710041 3 1 "readHeader" Prelude.False

astripHeader =
  T.mkVariable tUtil 1790001 1790077 3 1 "stripHeader" Prelude.False

astripFooter =
  T.mkVariable tUtil 1860001 1890041 3 1 "stripFooter" Prelude.False

areadTree = T.mkVariable tUtil 1930001 1950073 3 1 "readTree" Prelude.False

areadObjects =
  T.mkVariable tUtil 2020001 2020065 3 1 "readObjects" Prelude.False

areadConstraints =
  T.mkVariable tUtil 2090001 2110057 3 1 "readConstraints" Prelude.False

asplitList = T.mkVariable tUtil 2210001 2240046 3 2 "splitList" Prelude.False

asplitList2 = T.mkVariable tUtil 2340001 2350058 3 3 "splitList2" Prelude.False

asplitList2' =
  T.mkVariable tUtil 2410001 2420084 3 3 "splitList2'" Prelude.False

apossibleToList =
  T.mkVariable tUtil 2470001 2470071 3 1 "possibleToList" Prelude.False

arepartition =
  T.mkVariable tUtil 2580001 2640055 3 4 "repartition" Prelude.False

arepartition' =
  T.mkVariable tUtil 2750001 2820094 3 5 "repartition'" Prelude.False

arepartitionUniform =
  T.mkVariable tUtil 2930001 3000025 3 5 "repartitionUniform" Prelude.False

arepartition2 =
  T.mkVariable tUtil 3100001 3170039 3 2 "repartition2" Prelude.False

arepartition2' =
  T.mkVariable tUtil 3240001 3310039 3 3 "repartition2'" Prelude.False

(+&@=+#=&@=+$==) = T.mkVariable tUtil 480061 480062 3 2 "==" Prelude.False

a48v65v48v68showsPrec =
  T.mkVariable tUtil 480065 480068 3 2 "showsPrec" Prelude.False

a224v18v224v22first =
  T.mkVariable tUtil 2240018 2240022 3 0 "first" Prelude.True

a224v25v224v28rest = T.mkVariable tUtil 2240025 2240028 3 0 "rest" Prelude.True

a263v18v263v19ok = T.mkVariable tUtil 2630018 2630019 3 0 "ok" Prelude.True

a263v22v263v26list1 =
  T.mkVariable tUtil 2630022 2630026 3 0 "list1" Prelude.True

a264v18v264v20ok2 = T.mkVariable tUtil 2640018 2640020 3 0 "ok2" Prelude.True

a264v23v264v27list2 =
  T.mkVariable tUtil 2640023 2640027 3 0 "list2" Prelude.True

a280v17v280v24nb = T.mkVariable tUtil 2800017 2800024 3 0 "nb" Prelude.True

a281v17v281v28sub = T.mkVariable tUtil 2810017 2810028 3 0 "sub" Prelude.True

a282v17v282v94decrease =
  T.mkVariable tUtil 2820017 2820094 3 0 "decrease" Prelude.True

a276v26v276v27ok = T.mkVariable tUtil 2760026 2760027 3 0 "ok" Prelude.True

a276v30v276v33list = T.mkVariable tUtil 2760030 2760033 3 0 "list" Prelude.True

a299v17v299v24nb = T.mkVariable tUtil 2990017 2990024 3 0 "nb" Prelude.True

a300v17v300v25nb1 = T.mkVariable tUtil 3000017 3000025 3 0 "nb1" Prelude.True

a315v17v315v57sizeGroups =
  T.mkVariable tUtil 3150017 3150057 3 0 "sizeGroups" Prelude.True

a316v18v316v24sumSize =
  T.mkVariable tUtil 3160018 3160024 3 0 "sumSize" Prelude.True

a316v27v316v34sumAbove =
  T.mkVariable tUtil 3160027 3160034 3 0 "sumAbove" Prelude.True

a316v37v316v44sumBelow =
  T.mkVariable tUtil 3160037 3160044 3 0 "sumBelow" Prelude.True

a317v17v317v39diffSum =
  T.mkVariable tUtil 3170017 3170039 3 0 "diffSum" Prelude.True

a327v17v327v34size = T.mkVariable tUtil 3270017 3270034 3 0 "size" Prelude.True

a328v17v331v39extra =
  T.mkVariable tUtil 3280017 3310039 3 0 "extra" Prelude.True

p48v19v48v26 = T.mkSrcPos tUtil 480019 480026

p48v32v48v42 = T.mkSrcPos tUtil 480032 480042

p48v61v48v62 = T.mkSrcPos tUtil 480061 480062

p48v65v48v68 = T.mkSrcPos tUtil 480065 480068

p58v1v58v18 = T.mkSrcPos tUtil 580001 580018

p58v13v58v18 = T.mkSrcPos tUtil 580013 580018

p58v13v58v16 = T.mkSrcPos tUtil 580013 580016

p65v1v65v60 = T.mkSrcPos tUtil 650001 650060

p65v25v65v60 = T.mkSrcPos tUtil 650025 650060

p65v25v65v27 = T.mkSrcPos tUtil 650025 650027

p65v30v65v56 = T.mkSrcPos tUtil 650030 650056

p65v52v65v56 = T.mkSrcPos tUtil 650052 650056

p70v1v70v50 = T.mkSrcPos tUtil 700001 700050

p70v21v70v50 = T.mkSrcPos tUtil 700021 700050

p70v21v70v23 = T.mkSrcPos tUtil 700021 700023

p70v26v70v46 = T.mkSrcPos tUtil 700026 700046

p70v43v70v46 = T.mkSrcPos tUtil 700043 700046

p76v1v76v33 = T.mkSrcPos tUtil 760001 760033

p76v33v76v33 = T.mkSrcPos tUtil 760033 760033

p81v1v81v29 = T.mkSrcPos tUtil 810001 810029

p81v29v81v29 = T.mkSrcPos tUtil 810029 810029

p87v1v87v35 = T.mkSrcPos tUtil 870001 870035

p87v35v87v35 = T.mkSrcPos tUtil 870035 870035

p93v1v93v32 = T.mkSrcPos tUtil 930001 930032

p93v32v93v32 = T.mkSrcPos tUtil 930032 930032

p99v1v99v28 = T.mkSrcPos tUtil 990001 990028

p99v28v99v28 = T.mkSrcPos tUtil 990028 990028

p109v1v109v41 = T.mkSrcPos tUtil 1090001 1090041

p109v16v109v41 = T.mkSrcPos tUtil 1090016 1090041

p109v16v109v18 = T.mkSrcPos tUtil 1090016 1090018

p109v22v109v41 = T.mkSrcPos tUtil 1090022 1090041

p109v36v109v37 = T.mkSrcPos tUtil 1090036 1090037

p109v22v109v34 = T.mkSrcPos tUtil 1090022 1090034

p109v22v109v32 = T.mkSrcPos tUtil 1090022 1090032

p109v39v109v41 = T.mkSrcPos tUtil 1090039 1090041

p117v1v118v62 = T.mkSrcPos tUtil 1170001 1180062

p117v13v117v13 = T.mkSrcPos tUtil 1170013 1170013

p117v17v117v24 = T.mkSrcPos tUtil 1170017 1170024

p118v18v118v62 = T.mkSrcPos tUtil 1180018 1180062

p118v44v118v45 = T.mkSrcPos tUtil 1180044 1180045

p118v18v118v41 = T.mkSrcPos tUtil 1180018 1180041

p118v18v118v21 = T.mkSrcPos tUtil 1180018 1180021

p118v23v118v41 = T.mkSrcPos tUtil 1180023 1180041

p118v23v118v25 = T.mkSrcPos tUtil 1180023 1180025

p118v28v118v41 = T.mkSrcPos tUtil 1180028 1180041

p118v35v118v36 = T.mkSrcPos tUtil 1180035 1180036

p118v28v118v33 = T.mkSrcPos tUtil 1180028 1180033

p118v28v118v31 = T.mkSrcPos tUtil 1180028 1180031

p118v37v118v41 = T.mkSrcPos tUtil 1180037 1180041

p118v47v118v62 = T.mkSrcPos tUtil 1180047 1180062

p118v47v118v57 = T.mkSrcPos tUtil 1180047 1180057

p118v60v118v62 = T.mkSrcPos tUtil 1180060 1180062

p118v61v118v61 = T.mkSrcPos tUtil 1180061 1180061

p118v62v118v62 = T.mkSrcPos tUtil 1180062 1180062

p131v1v132v121 = T.mkSrcPos tUtil 1310001 1320121

p131v37v132v121 = T.mkSrcPos tUtil 1310037 1320121

p131v52v131v52 = T.mkSrcPos tUtil 1310052 1310052

p131v37v131v50 = T.mkSrcPos tUtil 1310037 1310050

p131v37v131v45 = T.mkSrcPos tUtil 1310037 1310045

p131v54v132v121 = T.mkSrcPos tUtil 1310054 1320121

p131v67v131v68 = T.mkSrcPos tUtil 1310067 1310068

p131v54v131v65 = T.mkSrcPos tUtil 1310054 1310065

p131v54v131v57 = T.mkSrcPos tUtil 1310054 1310057

p131v70v132v121 = T.mkSrcPos tUtil 1310070 1320121

p131v84v131v85 = T.mkSrcPos tUtil 1310084 1310085

p131v70v131v82 = T.mkSrcPos tUtil 1310070 1310082

p132v10v132v121 = T.mkSrcPos tUtil 1320010 1320121

p132v25v132v26 = T.mkSrcPos tUtil 1320025 1320026

p132v10v132v22 = T.mkSrcPos tUtil 1320010 1320022

p132v10v132v17 = T.mkSrcPos tUtil 1320010 1320017

p132v28v132v121 = T.mkSrcPos tUtil 1320028 1320121

p132v35v132v36 = T.mkSrcPos tUtil 1320035 1320036

p132v28v132v33 = T.mkSrcPos tUtil 1320028 1320033

p132v39v132v121 = T.mkSrcPos tUtil 1320039 1320121

p132v60v132v61 = T.mkSrcPos tUtil 1320060 1320061

p132v39v132v57 = T.mkSrcPos tUtil 1320039 1320057

p132v39v132v49 = T.mkSrcPos tUtil 1320039 1320049

p132v63v132v121 = T.mkSrcPos tUtil 1320063 1320121

p132v85v132v86 = T.mkSrcPos tUtil 1320085 1320086

p132v63v132v83 = T.mkSrcPos tUtil 1320063 1320083

p132v89v132v121 = T.mkSrcPos tUtil 1320089 1320121

p132v94v132v94 = T.mkSrcPos tUtil 1320094 1320094

p132v89v132v92 = T.mkSrcPos tUtil 1320089 1320092

p132v96v132v121 = T.mkSrcPos tUtil 1320096 1320121

p132v96v132v104 = T.mkSrcPos tUtil 1320096 1320104

p132v107v132v117 = T.mkSrcPos tUtil 1320107 1320117

p132v107v132v112 = T.mkSrcPos tUtil 1320107 1320112

p132v120v132v121 = T.mkSrcPos tUtil 1320120 1320121

p140v1v140v51 = T.mkSrcPos tUtil 1400001 1400051

p140v24v140v51 = T.mkSrcPos tUtil 1400024 1400051

p140v39v140v39 = T.mkSrcPos tUtil 1400039 1400039

p140v24v140v37 = T.mkSrcPos tUtil 1400024 1400037

p140v24v140v32 = T.mkSrcPos tUtil 1400024 1400032

p140v41v140v51 = T.mkSrcPos tUtil 1400041 1400051

p140v41v140v46 = T.mkSrcPos tUtil 1400041 1400046

p148v1v149v129 = T.mkSrcPos tUtil 1480001 1490129

p148v28v149v129 = T.mkSrcPos tUtil 1480028 1490129

p148v53v148v54 = T.mkSrcPos tUtil 1480053 1480054

p148v28v148v51 = T.mkSrcPos tUtil 1480028 1480051

p148v57v149v129 = T.mkSrcPos tUtil 1480057 1490129

p148v65v148v66 = T.mkSrcPos tUtil 1480065 1480066

p148v57v148v62 = T.mkSrcPos tUtil 1480057 1480062

p148v57v148v60 = T.mkSrcPos tUtil 1480057 1480060

p148v68v149v129 = T.mkSrcPos tUtil 1480068 1490129

p148v92v148v93 = T.mkSrcPos tUtil 1480092 1480093

p148v68v148v90 = T.mkSrcPos tUtil 1480068 1480090

p148v96v149v129 = T.mkSrcPos tUtil 1480096 1490129

p148v104v148v105 = T.mkSrcPos tUtil 1480104 1480105

p148v96v148v101 = T.mkSrcPos tUtil 1480096 1480101

p148v96v148v99 = T.mkSrcPos tUtil 1480096 1480099

p149v9v149v129 = T.mkSrcPos tUtil 1490009 1490129

p149v39v149v40 = T.mkSrcPos tUtil 1490039 1490040

p149v9v149v37 = T.mkSrcPos tUtil 1490009 1490037

p149v43v149v129 = T.mkSrcPos tUtil 1490043 1490129

p149v51v149v52 = T.mkSrcPos tUtil 1490051 1490052

p149v43v149v48 = T.mkSrcPos tUtil 1490043 1490048

p149v43v149v46 = T.mkSrcPos tUtil 1490043 1490046

p149v54v149v129 = T.mkSrcPos tUtil 1490054 1490129

p149v81v149v82 = T.mkSrcPos tUtil 1490081 1490082

p149v54v149v79 = T.mkSrcPos tUtil 1490054 1490079

p149v85v149v129 = T.mkSrcPos tUtil 1490085 1490129

p149v93v149v94 = T.mkSrcPos tUtil 1490093 1490094

p149v85v149v90 = T.mkSrcPos tUtil 1490085 1490090

p149v85v149v88 = T.mkSrcPos tUtil 1490085 1490088

p149v96v149v129 = T.mkSrcPos tUtil 1490096 1490129

p149v119v149v120 = T.mkSrcPos tUtil 1490119 1490120

p149v96v149v117 = T.mkSrcPos tUtil 1490096 1490117

p149v123v149v129 = T.mkSrcPos tUtil 1490123 1490129

p149v123v149v126 = T.mkSrcPos tUtil 1490123 1490126

p157v1v157v53 = T.mkSrcPos tUtil 1570001 1570053

p157v14v157v53 = T.mkSrcPos tUtil 1570014 1570053

p157v24v157v25 = T.mkSrcPos tUtil 1570024 1570025

p157v14v157v22 = T.mkSrcPos tUtil 1570014 1570022

p157v28v157v53 = T.mkSrcPos tUtil 1570028 1570053

p157v46v157v47 = T.mkSrcPos tUtil 1570046 1570047

p157v28v157v43 = T.mkSrcPos tUtil 1570028 1570043

p157v28v157v34 = T.mkSrcPos tUtil 1570028 1570034

p157v40v157v43 = T.mkSrcPos tUtil 1570040 1570043

p157v49v157v53 = T.mkSrcPos tUtil 1570049 1570053

p163v1v164v105 = T.mkSrcPos tUtil 1630001 1640105

p163v16v163v17 = T.mkSrcPos tUtil 1630016 1630017

p164v39v164v105 = T.mkSrcPos tUtil 1640039 1640105

p164v72v164v73 = T.mkSrcPos tUtil 1640072 1640073

p164v39v164v69 = T.mkSrcPos tUtil 1640039 1640069

p164v55v164v58 = T.mkSrcPos tUtil 1640055 1640058

p164v65v164v69 = T.mkSrcPos tUtil 1640065 1640069

p164v75v164v105 = T.mkSrcPos tUtil 1640075 1640105

p164v79v164v80 = T.mkSrcPos tUtil 1640079 1640080

p164v82v164v105 = T.mkSrcPos tUtil 1640082 1640105

p164v87v164v88 = T.mkSrcPos tUtil 1640087 1640088

p164v82v164v85 = T.mkSrcPos tUtil 1640082 1640085

p164v90v164v105 = T.mkSrcPos tUtil 1640090 1640105

p164v90v164v96 = T.mkSrcPos tUtil 1640090 1640096

p164v101v164v105 = T.mkSrcPos tUtil 1640101 1640105

p171v1v171v41 = T.mkSrcPos tUtil 1710001 1710041

p171v22v171v41 = T.mkSrcPos tUtil 1710022 1710041

p171v27v171v27 = T.mkSrcPos tUtil 1710027 1710027

p171v22v171v25 = T.mkSrcPos tUtil 1710022 1710025

p171v29v171v41 = T.mkSrcPos tUtil 1710029 1710041

p171v29v171v33 = T.mkSrcPos tUtil 1710029 1710033

p179v1v179v77 = T.mkSrcPos tUtil 1790001 1790077

p179v20v179v77 = T.mkSrcPos tUtil 1790020 1790077

p179v27v179v27 = T.mkSrcPos tUtil 1790027 1790027

p179v20v179v25 = T.mkSrcPos tUtil 1790020 1790025

p179v20v179v23 = T.mkSrcPos tUtil 1790020 1790023

p179v25v179v25 = T.mkSrcPos tUtil 1790025 1790025

p179v29v179v77 = T.mkSrcPos tUtil 1790029 1790077

p179v57v179v57 = T.mkSrcPos tUtil 1790057 1790057

p179v29v179v54 = T.mkSrcPos tUtil 1790029 1790054

p179v29v179v37 = T.mkSrcPos tUtil 1790029 1790037

p179v40v179v54 = T.mkSrcPos tUtil 1790040 1790054

p179v46v179v54 = T.mkSrcPos tUtil 1790046 1790054

p179v48v179v49 = T.mkSrcPos tUtil 1790048 1790049

p179v51v179v54 = T.mkSrcPos tUtil 1790051 1790054

p179v59v179v77 = T.mkSrcPos tUtil 1790059 1790077

p179v66v179v66 = T.mkSrcPos tUtil 1790066 1790066

p179v59v179v64 = T.mkSrcPos tUtil 1790059 1790064

p179v59v179v62 = T.mkSrcPos tUtil 1790059 1790062

p179v64v179v64 = T.mkSrcPos tUtil 1790064 1790064

p179v68v179v77 = T.mkSrcPos tUtil 1790068 1790077

p179v68v179v72 = T.mkSrcPos tUtil 1790068 1790072

p186v1v189v41 = T.mkSrcPos tUtil 1860001 1890041

p186v18v186v19 = T.mkSrcPos tUtil 1860018 1860019

p188v11v188v17 = T.mkSrcPos tUtil 1880011 1880017

p188v13v188v14 = T.mkSrcPos tUtil 1880013 1880014

p188v16v188v17 = T.mkSrcPos tUtil 1880016 1880017

p188v21v188v22 = T.mkSrcPos tUtil 1880021 1880022

p189v11v189v19 = T.mkSrcPos tUtil 1890011 1890019

p189v23v189v41 = T.mkSrcPos tUtil 1890023 1890041

p189v28v189v41 = T.mkSrcPos tUtil 1890028 1890041

p189v28v189v38 = T.mkSrcPos tUtil 1890028 1890038

p193v1v195v73 = T.mkSrcPos tUtil 1930001 1950073

p194v20v194v32 = T.mkSrcPos tUtil 1940020 1940032

p194v20v194v27 = T.mkSrcPos tUtil 1940020 1940027

p195v9v195v73 = T.mkSrcPos tUtil 1950009 1950073

p195v9v195v14 = T.mkSrcPos tUtil 1950009 1950014

p195v17v195v73 = T.mkSrcPos tUtil 1950017 1950073

p195v22v195v22 = T.mkSrcPos tUtil 1950022 1950022

p195v17v195v20 = T.mkSrcPos tUtil 1950017 1950020

p195v24v195v73 = T.mkSrcPos tUtil 1950024 1950073

p195v31v195v31 = T.mkSrcPos tUtil 1950031 1950031

p195v24v195v29 = T.mkSrcPos tUtil 1950024 1950029

p195v33v195v73 = T.mkSrcPos tUtil 1950033 1950073

p195v45v195v45 = T.mkSrcPos tUtil 1950045 1950045

p195v33v195v43 = T.mkSrcPos tUtil 1950033 1950043

p195v47v195v73 = T.mkSrcPos tUtil 1950047 1950073

p195v53v195v53 = T.mkSrcPos tUtil 1950053 1950053

p195v47v195v51 = T.mkSrcPos tUtil 1950047 1950051

p195v55v195v73 = T.mkSrcPos tUtil 1950055 1950073

p195v55v195v65 = T.mkSrcPos tUtil 1950055 1950065

p202v1v202v65 = T.mkSrcPos tUtil 2020001 2020065

p202v25v202v38 = T.mkSrcPos tUtil 2020025 2020038

p202v25v202v28 = T.mkSrcPos tUtil 2020025 2020028

p209v1v211v57 = T.mkSrcPos tUtil 2090001 2110057

p210v20v210v32 = T.mkSrcPos tUtil 2100020 2100032

p210v20v210v27 = T.mkSrcPos tUtil 2100020 2100027

p211v9v211v57 = T.mkSrcPos tUtil 2110009 2110057

p211v9v211v14 = T.mkSrcPos tUtil 2110009 2110014

p211v17v211v57 = T.mkSrcPos tUtil 2110017 2110057

p211v17v211v19 = T.mkSrcPos tUtil 2110017 2110019

p211v22v211v41 = T.mkSrcPos tUtil 2110022 2110041

p211v28v211v33 = T.mkSrcPos tUtil 2110028 2110033

p211v28v211v31 = T.mkSrcPos tUtil 2110028 2110031

p211v45v211v57 = T.mkSrcPos tUtil 2110045 2110057

p211v45v211v49 = T.mkSrcPos tUtil 2110045 2110049

p221v1v224v46 = T.mkSrcPos tUtil 2210001 2240046

p221v18v221v19 = T.mkSrcPos tUtil 2210018 2210019

p224v18v224v22 = T.mkSrcPos tUtil 2240018 2240022

p224v25v224v28 = T.mkSrcPos tUtil 2240025 2240028

p224v33v224v46 = T.mkSrcPos tUtil 2240033 2240046

p224v33v224v39 = T.mkSrcPos tUtil 2240033 2240039

p222v30v222v60 = T.mkSrcPos tUtil 2220030 2220060

p222v30v222v34 = T.mkSrcPos tUtil 2220030 2220034

p222v39v222v60 = T.mkSrcPos tUtil 2220039 2220060

p222v39v222v47 = T.mkSrcPos tUtil 2220039 2220047

p222v49v222v52 = T.mkSrcPos tUtil 2220049 2220052

p234v1v235v58 = T.mkSrcPos tUtil 2340001 2350058

p234v21v234v22 = T.mkSrcPos tUtil 2340021 2340022

p235v27v235v58 = T.mkSrcPos tUtil 2350027 2350058

p235v27v235v37 = T.mkSrcPos tUtil 2350027 2350037

p235v40v235v49 = T.mkSrcPos tUtil 2350040 2350049

p235v40v235v44 = T.mkSrcPos tUtil 2350040 2350044

p241v1v242v84 = T.mkSrcPos tUtil 2410001 2420084

p241v13v241v17 = T.mkSrcPos tUtil 2410013 2410017

p241v15v241v15 = T.mkSrcPos tUtil 2410015 2410015

p241v21v241v22 = T.mkSrcPos tUtil 2410021 2410022

p242v28v242v84 = T.mkSrcPos tUtil 2420028 2420084

p242v28v242v41 = T.mkSrcPos tUtil 2420028 2420041

p242v28v242v31 = T.mkSrcPos tUtil 2420028 2420031

p242v45v242v84 = T.mkSrcPos tUtil 2420045 2420084

p242v45v242v55 = T.mkSrcPos tUtil 2420045 2420055

p242v58v242v71 = T.mkSrcPos tUtil 2420058 2420071

p242v58v242v61 = T.mkSrcPos tUtil 2420058 2420061

p242v75v242v78 = T.mkSrcPos tUtil 2420075 2420078

p242v77v242v77 = T.mkSrcPos tUtil 2420077 2420077

p242v78v242v78 = T.mkSrcPos tUtil 2420078 2420078

p247v1v247v71 = T.mkSrcPos tUtil 2470001 2470071

p247v20v247v71 = T.mkSrcPos tUtil 2470020 2470071

p247v47v247v48 = T.mkSrcPos tUtil 2470047 2470048

p247v68v247v71 = T.mkSrcPos tUtil 2470068 2470071

p258v1v264v55 = T.mkSrcPos tUtil 2580001 2640055

p263v18v263v19 = T.mkSrcPos tUtil 2630018 2630019

p263v22v263v26 = T.mkSrcPos tUtil 2630022 2630026

p263v31v263v60 = T.mkSrcPos tUtil 2630031 2630060

p263v31v263v48 = T.mkSrcPos tUtil 2630031 2630048

p263v60v263v60 = T.mkSrcPos tUtil 2630060 2630060

p264v18v264v20 = T.mkSrcPos tUtil 2640018 2640020

p264v23v264v27 = T.mkSrcPos tUtil 2640023 2640027

p264v32v264v55 = T.mkSrcPos tUtil 2640032 2640055

p264v32v264v43 = T.mkSrcPos tUtil 2640032 2640043

p264v55v264v55 = T.mkSrcPos tUtil 2640055 2640055

p259v11v259v12 = T.mkSrcPos tUtil 2590011 2590012

p259v16v259v29 = T.mkSrcPos tUtil 2590016 2590029

p259v25v259v29 = T.mkSrcPos tUtil 2590025 2590029

p260v11v260v13 = T.mkSrcPos tUtil 2600011 2600013

p260v17v260v30 = T.mkSrcPos tUtil 2600017 2600030

p260v26v260v30 = T.mkSrcPos tUtil 2600026 2600030

p261v11v261v19 = T.mkSrcPos tUtil 2610011 2610019

p261v23v261v63 = T.mkSrcPos tUtil 2610023 2610063

p261v35v261v63 = T.mkSrcPos tUtil 2610035 2610063

p275v1v282v94 = T.mkSrcPos tUtil 2750001 2820094

p280v17v280v24 = T.mkSrcPos tUtil 2800017 2800024

p280v22v280v24 = T.mkSrcPos tUtil 2800022 2800024

p280v23v280v23 = T.mkSrcPos tUtil 2800023 2800023

p281v17v281v28 = T.mkSrcPos tUtil 2810017 2810028

p281v23v281v28 = T.mkSrcPos tUtil 2810023 2810028

p281v26v281v26 = T.mkSrcPos tUtil 2810026 2810026

p281v27v281v28 = T.mkSrcPos tUtil 2810027 2810028

p282v17v282v94 = T.mkSrcPos tUtil 2820017 2820094

p282v28v282v94 = T.mkSrcPos tUtil 2820028 2820094

p282v31v282v41 = T.mkSrcPos tUtil 2820031 2820041

p282v35v282v36 = T.mkSrcPos tUtil 2820035 2820036

p282v31v282v33 = T.mkSrcPos tUtil 2820031 2820033

p282v32v282v32 = T.mkSrcPos tUtil 2820032 2820032

p282v38v282v41 = T.mkSrcPos tUtil 2820038 2820041

p282v40v282v40 = T.mkSrcPos tUtil 2820040 2820040

p282v38v282v39 = T.mkSrcPos tUtil 2820038 2820039

p282v41v282v41 = T.mkSrcPos tUtil 2820041 2820041

p282v49v282v75 = T.mkSrcPos tUtil 2820049 2820075

p282v49v282v60 = T.mkSrcPos tUtil 2820049 2820060

p282v73v282v75 = T.mkSrcPos tUtil 2820073 2820075

p282v74v282v74 = T.mkSrcPos tUtil 2820074 2820074

p282v75v282v75 = T.mkSrcPos tUtil 2820075 2820075

p282v84v282v94 = T.mkSrcPos tUtil 2820084 2820094

p282v85v282v89 = T.mkSrcPos tUtil 2820085 2820089

p282v92v282v93 = T.mkSrcPos tUtil 2820092 2820093

p276v11v276v17 = T.mkSrcPos tUtil 2760011 2760017

p276v15v276v15 = T.mkSrcPos tUtil 2760015 2760015

p276v11v276v13 = T.mkSrcPos tUtil 2760011 2760013

p276v17v276v17 = T.mkSrcPos tUtil 2760017 2760017

p276v26v276v27 = T.mkSrcPos tUtil 2760026 2760027

p276v30v276v33 = T.mkSrcPos tUtil 2760030 2760033

p276v38v276v61 = T.mkSrcPos tUtil 2760038 2760061

p276v38v276v49 = T.mkSrcPos tUtil 2760038 2760049

p276v51v276v53 = T.mkSrcPos tUtil 2760051 2760053

p276v66v276v103 = T.mkSrcPos tUtil 2760066 2760103

p276v69v276v70 = T.mkSrcPos tUtil 2760069 2760070

p276v77v276v89 = T.mkSrcPos tUtil 2760077 2760089

p276v78v276v79 = T.mkSrcPos tUtil 2760078 2760079

p276v82v276v88 = T.mkSrcPos tUtil 2760082 2760088

p276v82v276v83 = T.mkSrcPos tUtil 2760082 2760083

p276v85v276v88 = T.mkSrcPos tUtil 2760085 2760088

p276v96v276v103 = T.mkSrcPos tUtil 2760096 2760103

p277v11v277v18 = T.mkSrcPos tUtil 2770011 2770018

p277v15v277v16 = T.mkSrcPos tUtil 2770015 2770016

p277v11v277v13 = T.mkSrcPos tUtil 2770011 2770013

p277v18v277v18 = T.mkSrcPos tUtil 2770018 2770018

p277v22v277v33 = T.mkSrcPos tUtil 2770022 2770033

p277v23v277v26 = T.mkSrcPos tUtil 2770023 2770026

p277v29v277v32 = T.mkSrcPos tUtil 2770029 2770032

p277v30v277v31 = T.mkSrcPos tUtil 2770030 2770031

p278v11v278v19 = T.mkSrcPos tUtil 2780011 2780019

p278v23v278v30 = T.mkSrcPos tUtil 2780023 2780030

p293v1v300v25 = T.mkSrcPos tUtil 2930001 3000025

p299v17v299v24 = T.mkSrcPos tUtil 2990017 2990024

p299v22v299v24 = T.mkSrcPos tUtil 2990022 2990024

p299v23v299v23 = T.mkSrcPos tUtil 2990023 2990023

p300v17v300v25 = T.mkSrcPos tUtil 3000017 3000025

p300v23v300v25 = T.mkSrcPos tUtil 3000023 3000025

p300v24v300v24 = T.mkSrcPos tUtil 3000024 3000024

p294v11v294v33 = T.mkSrcPos tUtil 2940011 2940033

p294v16v294v17 = T.mkSrcPos tUtil 2940016 2940017

p294v11v294v14 = T.mkSrcPos tUtil 2940011 2940014

p294v12v294v13 = T.mkSrcPos tUtil 2940012 2940013

p294v19v294v33 = T.mkSrcPos tUtil 2940019 2940033

p294v30v294v31 = T.mkSrcPos tUtil 2940030 2940031

p294v19v294v28 = T.mkSrcPos tUtil 2940019 2940028

p294v19v294v21 = T.mkSrcPos tUtil 2940019 2940021

p294v27v294v28 = T.mkSrcPos tUtil 2940027 2940028

p294v33v294v33 = T.mkSrcPos tUtil 2940033 2940033

p294v37v294v69 = T.mkSrcPos tUtil 2940037 2940069

p294v38v294v41 = T.mkSrcPos tUtil 2940038 2940041

p294v44v294v68 = T.mkSrcPos tUtil 2940044 2940068

p294v44v294v52 = T.mkSrcPos tUtil 2940044 2940052

p294v55v294v64 = T.mkSrcPos tUtil 2940055 2940064

p294v55v294v57 = T.mkSrcPos tUtil 2940055 2940057

p294v63v294v64 = T.mkSrcPos tUtil 2940063 2940064

p294v67v294v68 = T.mkSrcPos tUtil 2940067 2940068

p295v11v295v42 = T.mkSrcPos tUtil 2950011 2950042

p295v16v295v17 = T.mkSrcPos tUtil 2950016 2950017

p295v11v295v14 = T.mkSrcPos tUtil 2950011 2950014

p295v12v295v13 = T.mkSrcPos tUtil 2950012 2950013

p295v19v295v42 = T.mkSrcPos tUtil 2950019 2950042

p295v24v295v25 = T.mkSrcPos tUtil 2950024 2950025

p295v19v295v22 = T.mkSrcPos tUtil 2950019 2950022

p295v21v295v21 = T.mkSrcPos tUtil 2950021 2950021

p295v19v295v20 = T.mkSrcPos tUtil 2950019 2950020

p295v22v295v22 = T.mkSrcPos tUtil 2950022 2950022

p295v27v295v42 = T.mkSrcPos tUtil 2950027 2950042

p295v39v295v40 = T.mkSrcPos tUtil 2950039 2950040

p295v27v295v37 = T.mkSrcPos tUtil 2950027 2950037

p295v27v295v29 = T.mkSrcPos tUtil 2950027 2950029

p295v35v295v37 = T.mkSrcPos tUtil 2950035 2950037

p295v42v295v42 = T.mkSrcPos tUtil 2950042 2950042

p295v46v295v80 = T.mkSrcPos tUtil 2950046 2950080

p295v47v295v50 = T.mkSrcPos tUtil 2950047 2950050

p295v53v295v79 = T.mkSrcPos tUtil 2950053 2950079

p295v53v295v61 = T.mkSrcPos tUtil 2950053 2950061

p295v64v295v74 = T.mkSrcPos tUtil 2950064 2950074

p295v64v295v66 = T.mkSrcPos tUtil 2950064 2950066

p295v72v295v74 = T.mkSrcPos tUtil 2950072 2950074

p295v77v295v79 = T.mkSrcPos tUtil 2950077 2950079

p296v11v296v20 = T.mkSrcPos tUtil 2960011 2960020

p296v15v296v16 = T.mkSrcPos tUtil 2960015 2960016

p296v11v296v13 = T.mkSrcPos tUtil 2960011 2960013

p296v12v296v12 = T.mkSrcPos tUtil 2960012 2960012

p296v18v296v20 = T.mkSrcPos tUtil 2960018 2960020

p296v19v296v19 = T.mkSrcPos tUtil 2960019 2960019

p296v24v296v56 = T.mkSrcPos tUtil 2960024 2960056

p296v24v296v41 = T.mkSrcPos tUtil 2960024 2960041

p296v54v296v56 = T.mkSrcPos tUtil 2960054 2960056

p296v55v296v55 = T.mkSrcPos tUtil 2960055 2960055

p296v56v296v56 = T.mkSrcPos tUtil 2960056 2960056

p297v11v297v19 = T.mkSrcPos tUtil 2970011 2970019

p297v23v297v33 = T.mkSrcPos tUtil 2970023 2970033

p297v24v297v28 = T.mkSrcPos tUtil 2970024 2970028

p297v31v297v32 = T.mkSrcPos tUtil 2970031 2970032

p310v1v317v39 = T.mkSrcPos tUtil 3100001 3170039

p315v17v315v57 = T.mkSrcPos tUtil 3150017 3150057

p315v30v315v57 = T.mkSrcPos tUtil 3150030 3150057

p315v30v315v32 = T.mkSrcPos tUtil 3150030 3150032

p315v35v315v49 = T.mkSrcPos tUtil 3150035 3150049

p315v48v315v49 = T.mkSrcPos tUtil 3150048 3150049

p316v18v316v24 = T.mkSrcPos tUtil 3160018 3160024

p316v27v316v34 = T.mkSrcPos tUtil 3160027 3160034

p316v37v316v44 = T.mkSrcPos tUtil 3160037 3160044

p316v49v316v110 = T.mkSrcPos tUtil 3160049 3160110

p316v49v316v53 = T.mkSrcPos tUtil 3160049 3160053

p316v56v316v94 = T.mkSrcPos tUtil 3160056 3160094

p316v79v316v94 = T.mkSrcPos tUtil 3160079 3160094

p316v80v316v83 = T.mkSrcPos tUtil 3160080 3160083

p316v81v316v81 = T.mkSrcPos tUtil 3160081 3160081

p316v85v316v88 = T.mkSrcPos tUtil 3160085 3160088

p316v86v316v86 = T.mkSrcPos tUtil 3160086 3160086

p316v90v316v93 = T.mkSrcPos tUtil 3160090 3160093

p316v91v316v91 = T.mkSrcPos tUtil 3160091 3160091

p316v97v316v103 = T.mkSrcPos tUtil 3160097 3160103

p316v98v316v98 = T.mkSrcPos tUtil 3160098 3160098

p316v100v316v100 = T.mkSrcPos tUtil 3160100 3160100

p316v102v316v102 = T.mkSrcPos tUtil 3160102 3160102

p317v17v317v39 = T.mkSrcPos tUtil 3170017 3170039

p317v27v317v39 = T.mkSrcPos tUtil 3170027 3170039

p317v31v317v31 = T.mkSrcPos tUtil 3170031 3170031

p317v33v317v39 = T.mkSrcPos tUtil 3170033 3170039

p311v11v311v24 = T.mkSrcPos tUtil 3110011 3110024

p311v19v311v20 = T.mkSrcPos tUtil 3110019 3110020

p311v11v311v17 = T.mkSrcPos tUtil 3110011 3110017

p311v28v311v46 = T.mkSrcPos tUtil 3110028 3110046

p311v37v311v46 = T.mkSrcPos tUtil 3110037 3110046

p312v12v312v87 = T.mkSrcPos tUtil 3120012 3120087

p312v48v312v49 = T.mkSrcPos tUtil 3120048 3120049

p312v12v312v45 = T.mkSrcPos tUtil 3120012 3120045

p312v24v312v25 = T.mkSrcPos tUtil 3120024 3120025

p312v12v312v22 = T.mkSrcPos tUtil 3120012 3120022

p312v20v312v20 = T.mkSrcPos tUtil 3120020 3120020

p312v12v312v18 = T.mkSrcPos tUtil 3120012 3120018

p312v22v312v22 = T.mkSrcPos tUtil 3120022 3120022

p312v27v312v45 = T.mkSrcPos tUtil 3120027 3120045

p312v35v312v36 = T.mkSrcPos tUtil 3120035 3120036

p312v27v312v33 = T.mkSrcPos tUtil 3120027 3120033

p312v38v312v45 = T.mkSrcPos tUtil 3120038 3120045

p312v52v312v87 = T.mkSrcPos tUtil 3120052 3120087

p312v64v312v65 = T.mkSrcPos tUtil 3120064 3120065

p312v52v312v62 = T.mkSrcPos tUtil 3120052 3120062

p312v60v312v60 = T.mkSrcPos tUtil 3120060 3120060

p312v52v312v58 = T.mkSrcPos tUtil 3120052 3120058

p312v62v312v62 = T.mkSrcPos tUtil 3120062 3120062

p312v67v312v87 = T.mkSrcPos tUtil 3120067 3120087

p312v75v312v76 = T.mkSrcPos tUtil 3120075 3120076

p312v67v312v73 = T.mkSrcPos tUtil 3120067 3120073

p312v79v312v87 = T.mkSrcPos tUtil 3120079 3120087

p312v79v312v79 = T.mkSrcPos tUtil 3120079 3120079

p312v80v312v87 = T.mkSrcPos tUtil 3120080 3120087

p312v93v312v135 = T.mkSrcPos tUtil 3120093 3120135

p312v102v312v102 = T.mkSrcPos tUtil 3120102 3120102

p312v93v312v100 = T.mkSrcPos tUtil 3120093 3120100

p312v104v312v135 = T.mkSrcPos tUtil 3120104 3120135

p312v104v312v116 = T.mkSrcPos tUtil 3120104 3120116

p312v122v312v128 = T.mkSrcPos tUtil 3120122 3120128

p313v11v313v19 = T.mkSrcPos tUtil 3130011 3130019

p313v23v313v60 = T.mkSrcPos tUtil 3130023 3130060

p313v35v313v60 = T.mkSrcPos tUtil 3130035 3130060

p324v1v331v39 = T.mkSrcPos tUtil 3240001 3310039

p324v24v324v25 = T.mkSrcPos tUtil 3240024 3240025

p327v17v327v34 = T.mkSrcPos tUtil 3270017 3270034

p327v24v327v34 = T.mkSrcPos tUtil 3270024 3270034

p327v28v327v28 = T.mkSrcPos tUtil 3270028 3270028

p327v30v327v34 = T.mkSrcPos tUtil 3270030 3270034

p328v17v331v39 = T.mkSrcPos tUtil 3280017 3310039

p329v27v329v44 = T.mkSrcPos tUtil 3290027 3290044

p329v36v329v37 = T.mkSrcPos tUtil 3290036 3290037

p329v27v329v34 = T.mkSrcPos tUtil 3290027 3290034

p329v32v329v32 = T.mkSrcPos tUtil 3290032 3290032

p329v34v329v34 = T.mkSrcPos tUtil 3290034 3290034

p329v39v329v44 = T.mkSrcPos tUtil 3290039 3290044

p329v42v329v42 = T.mkSrcPos tUtil 3290042 3290042

p329v44v329v44 = T.mkSrcPos tUtil 3290044 3290044

p329v48v329v58 = T.mkSrcPos tUtil 3290048 3290058

p329v48v329v50 = T.mkSrcPos tUtil 3290048 3290050

p330v27v330v44 = T.mkSrcPos tUtil 3300027 3300044

p330v36v330v37 = T.mkSrcPos tUtil 3300036 3300037

p330v27v330v34 = T.mkSrcPos tUtil 3300027 3300034

p330v32v330v32 = T.mkSrcPos tUtil 3300032 3300032

p330v34v330v34 = T.mkSrcPos tUtil 3300034 3300034

p330v39v330v44 = T.mkSrcPos tUtil 3300039 3300044

p330v42v330v42 = T.mkSrcPos tUtil 3300042 3300042

p330v44v330v44 = T.mkSrcPos tUtil 3300044 3300044

p330v48v330v60 = T.mkSrcPos tUtil 3300048 3300060

p330v48v330v50 = T.mkSrcPos tUtil 3300048 3300050

p330v58v330v60 = T.mkSrcPos tUtil 3300058 3300060

p330v58v330v58 = T.mkSrcPos tUtil 3300058 3300058

p331v27v331v35 = T.mkSrcPos tUtil 3310027 3310035

p331v39v331v39 = T.mkSrcPos tUtil 3310039 3310039

p325v47v325v97 = T.mkSrcPos tUtil 3250047 3250097

p325v47v325v50 = T.mkSrcPos tUtil 3250047 3250050

p325v54v325v97 = T.mkSrcPos tUtil 3250054 3250097

p325v54v325v66 = T.mkSrcPos tUtil 3250054 3250066

p325v69v325v76 = T.mkSrcPos tUtil 3250069 3250076

p325v72v325v72 = T.mkSrcPos tUtil 3250072 3250072

p325v73v325v76 = T.mkSrcPos tUtil 3250073 3250076

p325v80v325v89 = T.mkSrcPos tUtil 3250080 3250089

p325v84v325v84 = T.mkSrcPos tUtil 3250084 3250084

p325v85v325v89 = T.mkSrcPos tUtil 3250085 3250089
