module Hat.Tree
  (CellLabel(),CellNumbering(),CellFatherNumbering(),CellComponents(),Cell()
    ,NTree(Node),aNode,gshowTree,ashowTree,hshowTree,gnode,anode,hnode,gsubtrees
    ,asubtrees,hsubtrees,gcellComponents,acellComponents,hcellComponents,gnbLeaf
    ,anbLeaf,hnbLeaf) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

type CellLabel = String

type CellNumbering = String

type CellFatherNumbering = String

type CellComponents = T.List String

type Cell = T.Tuple4 CellLabel CellNumbering CellFatherNumbering CellComponents

data NTree a = Node (T.R a) (T.R (T.List (NTree a)))

instance T.WrapVal ((NTree a))
  where
  
  wrapVal pwrapVal (kwrapVal@(Node (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aNode z1wrapVal z2wrapVal)
  

instance Eq a => Eq ((NTree a))
  where
  
  (!==) (%==) p =
    T.ufun2 (+%!=&%=%!=&&==) (%==) p (*==)
    where
    
    (*==) (T.R (Node fy1 fy2) _) (T.R (Node fy3 fy4) _) p =
      T.uwrapForward p
        (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy3)
            Hat.Prelude.*&&
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy2 fy4)) p)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord a => Ord ((NTree a))
  where
  
  gcompare pcompare p =
    T.ufun2 a30v46v30v48compare pcompare p hcompare
    where
    
    hcompare (T.R (Node fy3 fy4) _) (T.R (Node fy5 fy6) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v30v46v30v48v1 (T.R Hat.Prelude.EQ _) p =
            T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy4 fy6
          v30v46v30v48v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
          (v30v46v30v48v1))
        (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy3 fy5)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a30v46v30v48localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a30v46v30v48localFromEnum
      
      hlocalFromEnum (T.R (Node _ _) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Show a => Show ((NTree a))
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a30v50v30v53showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Node fy2 fy3) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "Node "))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)) fy2)
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                  (T.conChar T.mkNoSrcPos p ' ')))
              (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy3))) p)
    hshowsPrec _ _ p = T.fatal p
    
  

instance Read a => Read ((NTree a))
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a30v55v30v58readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uwrapForward p
        (Hat.Prelude.hreadParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.pa0 Node T.cn2 T.mkNoSrcPos p aNode))
                  (T.fromLitString T.mkNoSrcPos p "Node") p))
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10))))
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)))) p)
    
  

gnode :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (NTree Cell) Cell)

hnode :: (T.R (NTree Cell)) -> T.RefExp -> T.R Cell

gnode pnode p = T.fun1 anode pnode p hnode

hnode (T.R (Node fcell _) _) p = T.projection p38v22v38v25 p fcell
hnode _ p = T.fatal p

gsubtrees ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (NTree Cell) (T.List (NTree Cell)))

hsubtrees :: (T.R (NTree Cell)) -> T.RefExp -> T.R (T.List (NTree Cell))

gsubtrees psubtrees p = T.fun1 asubtrees psubtrees p hsubtrees

hsubtrees (T.R (Node _ fstrees) _) p = T.projection p46v28v46v33 p fstrees
hsubtrees _ p = T.fatal p

gcellComponents :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Cell CellComponents)

hcellComponents :: (T.R Cell) -> T.RefExp -> T.R CellComponents

gcellComponents pcellComponents p =
  T.fun1 acellComponents pcellComponents p hcellComponents

hcellComponents (T.R (T.Tuple4 _ _ _ fcomponents) _) p =
  T.projection p54v37v54v46 p fcomponents
hcellComponents _ p = T.fatal p

gnbLeaf :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (NTree Cell) Int)

hnbLeaf :: (T.R (NTree Cell)) -> T.RefExp -> T.R Int

gnbLeaf pnbLeaf p = T.fun1 anbLeaf pnbLeaf p hnbLeaf

hnbLeaf ftree p =
  T.cguard p63v11v63v23 p
    (T.ap2 p63v11v63v23 p (p63v19v63v20 !== p) (gsbtrees p63v11v63v17 p)
      (T.con0 p63v22v63v23 p T.List T.aList))
    (\ p ->
      T.ap1 p63v27v63v27 p (Hat.PreludeBasic.gfromInteger p63v27v63v27 p)
        (T.conInteger p63v27v63v27 p 1))
    (\ p ->
      T.cguard p64v11v64v19 p (gotherwise p64v11v64v19 p)
        (\ p ->
          T.uapp3 p64v23v64v64 p64v23v64v27 p afoldl hfoldl
            (T.fun2 T.mkLambda p64v30v64v53 p
              (\ facc fx p ->
                T.ap2 p64v40v64v53 p (p64v44v64v44 !+ p) facc
                  (T.app1 p64v46v64v53 p64v46v64v51 p anbLeaf hnbLeaf fx)))
            (T.ap1 p64v56v64v56 p (Hat.PreludeBasic.gfromInteger p64v56v64v56 p)
              (T.conInteger p64v56v64v56 p 0)) (gsbtrees p64v58v64v64 p))
        (\ p -> T.fatal p))
  where
  
  gsbtrees psbtrees p = T.constUse psbtrees p ssbtrees
  
  ssbtrees =
    T.constDef p a66v17v66v39sbtrees
      (\ p -> T.app1 p66v27v66v39 p66v27v66v34 p asubtrees hsubtrees ftree)
  

gshowTree :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (NTree Cell) String)

hshowTree :: (T.R (NTree Cell)) -> T.RefExp -> T.R String

gshowTree pshowTree p = T.fun1 ashowTree pshowTree p hshowTree

hshowTree ftree p =
  T.app3 p74v17v74v38 p74v17v74v25 p ashowTree' hshowTree'
    (T.con0 p74v27v74v30 p True aTrue) (T.fromLitString p74v32v74v33 p "") ftree

gshowTree' ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Bool (T.Fun String (T.Fun (NTree Cell) String)))

hshowTree' ::
  (T.R Bool) -> (T.R String) -> (T.R (NTree Cell)) -> T.RefExp -> T.R String

gshowTree' pshowTree' p = T.fun3 ashowTree' pshowTree' p hshowTree'

hshowTree' fnoComma ftabs (T.R (Node fcell fsbtrees) _) p =
  T.uapp2 p84v46v84v135 p84v51v84v52 p (+++) (*++) ftabs
    (T.uapp2 p84v54v84v135 p84v62v84v63 p (+++) (*++)
      (T.fromLitString p84v54v84v60 p "Node ")
      (T.uapp2 p84v66v84v135 p84v77v84v78 p (+++) (*++)
        (T.ap1 p84v66v84v74 p (gshow p84v66v84v69 p) fcell)
        (T.uapp2 p84v80v84v135 p84v89v84v90 p (+++) (*++)
          (gstarting p84v80v84v87 p)
          (T.uapp2 p84v93v84v135 p84v127v84v128 p (+++) (*++)
            (T.app2 p84v93v84v124 p84v93v84v104 p ashowSubTrees hshowSubTrees
              (T.con2 p84v107v84v115 p T.Cons T.aCons
                (T.conChar p84v107v84v110 p '\t') ftabs) fsbtrees)
            (gending p84v130v84v135 p)))))
  where
  
  gstarting pstarting p = T.constUse pstarting p sstarting
  
  sstarting =
    T.constDef p a86v17v88v44starting
      (\ p ->
        T.cguard p87v27v87v39 p
          (T.ap2 p87v27v87v39 p (p87v35v87v36 !== p) fsbtrees
            (T.con0 p87v38v87v39 p T.List T.aList))
          (\ p -> T.fromLitString p87v43v87v46 p " [")
          (\ p ->
            T.cguard p88v27v88v35 p (gotherwise p88v27v88v35 p)
              (\ p -> T.fromLitString p88v39v88v44 p " [\n")
              (\ p -> T.fatal p)))
  
  gending pending p = T.constUse pending p sending
  
  sending =
    T.constDef p a89v17v91v44ending
      (\ p ->
        T.cguard p90v27v90v33 p fnoComma
          (\ p -> T.fromLitString p90v37v90v39 p "]")
          (\ p ->
            T.cguard p91v27v91v35 p (gotherwise p91v27v91v35 p)
              (\ p -> T.fromLitString p91v39v91v44 p "],\n")
              (\ p -> T.fatal p)))
  
hshowTree' _ _ _ p = T.fatal p

gshowSubTrees ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun String (T.Fun (T.List (NTree Cell)) String))

hshowSubTrees ::
  (T.R String) -> (T.R (T.List (NTree Cell))) -> T.RefExp -> T.R String

gshowSubTrees pshowSubTrees p =
  T.fun2 ashowSubTrees pshowSubTrees p hshowSubTrees

hshowSubTrees _ (T.R T.List _) p = T.fromLitString p100v21v100v22 p ""
hshowSubTrees ftabs (T.R (T.Cons fs fsbtrees) _) p =
  T.uapp2 p101v33v101v86 p101v58v101v59 p (+++) (*++)
    (T.app3 p101v33v101v56 p101v33v101v41 p ashowTree' hshowTree'
      (gnoComma p101v43v101v49 p) ftabs fs)
    (T.app2 p101v62v101v86 p101v62v101v73 p ashowSubTrees hshowSubTrees ftabs
      fsbtrees)
  where
  
  gnoComma pnoComma p = T.constUse pnoComma p snoComma
  
  snoComma =
    T.constDef p a103v17v103v39noComma
      (\ p ->
        T.ap2 p103v27v103v39 p (p103v35v103v36 !== p) fsbtrees
          (T.con0 p103v38v103v39 p T.List T.aList))
  
hshowSubTrees _ _ p = T.fatal p

tTree = T.mkModule "Tree" "../../src/Util/Tree.hs" Prelude.True

aNode = T.mkConstructor tTree 300016 300019 3 2 "Node"

anode = T.mkVariable tTree 380001 380025 3 1 "node" Prelude.False

asubtrees = T.mkVariable tTree 460001 460033 3 1 "subtrees" Prelude.False

acellComponents =
  T.mkVariable tTree 540001 540046 3 1 "cellComponents" Prelude.False

anbLeaf = T.mkVariable tTree 620001 660039 3 1 "nbLeaf" Prelude.False

ashowTree = T.mkVariable tTree 740001 740038 3 1 "showTree" Prelude.False

ashowTree' = T.mkVariable tTree 840001 910044 3 3 "showTree'" Prelude.False

ashowSubTrees =
  T.mkVariable tTree 1000001 1030039 3 2 "showSubTrees" Prelude.False

(+%!=&%=%!=&&==) = T.mkVariable tTree 300043 300044 3 2 "==" Prelude.False

a30v46v30v48compare =
  T.mkVariable tTree 300046 300048 3 2 "compare" Prelude.False

a30v50v30v53showsPrec =
  T.mkVariable tTree 300050 300053 3 2 "showsPrec" Prelude.False

a30v55v30v58readsPrec =
  T.mkVariable tTree 300055 300058 3 1 "readsPrec" Prelude.False

a30v46v30v48localFromEnum =
  T.mkVariable tTree 300046 300048 3 1 "localFromEnum" Prelude.True

a66v17v66v39sbtrees =
  T.mkVariable tTree 660017 660039 3 0 "sbtrees" Prelude.True

a86v17v88v44starting =
  T.mkVariable tTree 860017 880044 3 0 "starting" Prelude.True

a89v17v91v44ending = T.mkVariable tTree 890017 910044 3 0 "ending" Prelude.True

a103v17v103v39noComma =
  T.mkVariable tTree 1030017 1030039 3 0 "noComma" Prelude.True

p30v16v30v19 = T.mkSrcPos tTree 300016 300019

p30v43v30v44 = T.mkSrcPos tTree 300043 300044

p30v46v30v48 = T.mkSrcPos tTree 300046 300048

p30v50v30v53 = T.mkSrcPos tTree 300050 300053

p30v55v30v58 = T.mkSrcPos tTree 300055 300058

p38v1v38v25 = T.mkSrcPos tTree 380001 380025

p38v22v38v25 = T.mkSrcPos tTree 380022 380025

p46v1v46v33 = T.mkSrcPos tTree 460001 460033

p46v28v46v33 = T.mkSrcPos tTree 460028 460033

p54v1v54v46 = T.mkSrcPos tTree 540001 540046

p54v37v54v46 = T.mkSrcPos tTree 540037 540046

p62v1v66v39 = T.mkSrcPos tTree 620001 660039

p66v17v66v39 = T.mkSrcPos tTree 660017 660039

p66v27v66v39 = T.mkSrcPos tTree 660027 660039

p66v27v66v34 = T.mkSrcPos tTree 660027 660034

p63v11v63v23 = T.mkSrcPos tTree 630011 630023

p63v19v63v20 = T.mkSrcPos tTree 630019 630020

p63v11v63v17 = T.mkSrcPos tTree 630011 630017

p63v22v63v23 = T.mkSrcPos tTree 630022 630023

p63v27v63v27 = T.mkSrcPos tTree 630027 630027

p64v11v64v19 = T.mkSrcPos tTree 640011 640019

p64v23v64v64 = T.mkSrcPos tTree 640023 640064

p64v23v64v27 = T.mkSrcPos tTree 640023 640027

p64v30v64v53 = T.mkSrcPos tTree 640030 640053

p64v40v64v53 = T.mkSrcPos tTree 640040 640053

p64v44v64v44 = T.mkSrcPos tTree 640044 640044

p64v46v64v53 = T.mkSrcPos tTree 640046 640053

p64v46v64v51 = T.mkSrcPos tTree 640046 640051

p64v56v64v56 = T.mkSrcPos tTree 640056 640056

p64v58v64v64 = T.mkSrcPos tTree 640058 640064

p74v1v74v38 = T.mkSrcPos tTree 740001 740038

p74v17v74v38 = T.mkSrcPos tTree 740017 740038

p74v17v74v25 = T.mkSrcPos tTree 740017 740025

p74v27v74v30 = T.mkSrcPos tTree 740027 740030

p74v32v74v33 = T.mkSrcPos tTree 740032 740033

p84v1v91v44 = T.mkSrcPos tTree 840001 910044

p86v17v88v44 = T.mkSrcPos tTree 860017 880044

p87v27v87v39 = T.mkSrcPos tTree 870027 870039

p87v35v87v36 = T.mkSrcPos tTree 870035 870036

p87v38v87v39 = T.mkSrcPos tTree 870038 870039

p87v43v87v46 = T.mkSrcPos tTree 870043 870046

p88v27v88v35 = T.mkSrcPos tTree 880027 880035

p88v39v88v44 = T.mkSrcPos tTree 880039 880044

p89v17v91v44 = T.mkSrcPos tTree 890017 910044

p90v27v90v33 = T.mkSrcPos tTree 900027 900033

p90v37v90v39 = T.mkSrcPos tTree 900037 900039

p91v27v91v35 = T.mkSrcPos tTree 910027 910035

p91v39v91v44 = T.mkSrcPos tTree 910039 910044

p84v46v84v135 = T.mkSrcPos tTree 840046 840135

p84v51v84v52 = T.mkSrcPos tTree 840051 840052

p84v54v84v135 = T.mkSrcPos tTree 840054 840135

p84v62v84v63 = T.mkSrcPos tTree 840062 840063

p84v54v84v60 = T.mkSrcPos tTree 840054 840060

p84v66v84v135 = T.mkSrcPos tTree 840066 840135

p84v77v84v78 = T.mkSrcPos tTree 840077 840078

p84v66v84v74 = T.mkSrcPos tTree 840066 840074

p84v66v84v69 = T.mkSrcPos tTree 840066 840069

p84v80v84v135 = T.mkSrcPos tTree 840080 840135

p84v89v84v90 = T.mkSrcPos tTree 840089 840090

p84v80v84v87 = T.mkSrcPos tTree 840080 840087

p84v93v84v135 = T.mkSrcPos tTree 840093 840135

p84v127v84v128 = T.mkSrcPos tTree 840127 840128

p84v93v84v124 = T.mkSrcPos tTree 840093 840124

p84v93v84v104 = T.mkSrcPos tTree 840093 840104

p84v107v84v115 = T.mkSrcPos tTree 840107 840115

p84v107v84v110 = T.mkSrcPos tTree 840107 840110

p84v130v84v135 = T.mkSrcPos tTree 840130 840135

p100v1v103v39 = T.mkSrcPos tTree 1000001 1030039

p100v21v100v22 = T.mkSrcPos tTree 1000021 1000022

p103v17v103v39 = T.mkSrcPos tTree 1030017 1030039

p103v27v103v39 = T.mkSrcPos tTree 1030027 1030039

p103v35v103v36 = T.mkSrcPos tTree 1030035 1030036

p103v38v103v39 = T.mkSrcPos tTree 1030038 1030039

p101v33v101v86 = T.mkSrcPos tTree 1010033 1010086

p101v58v101v59 = T.mkSrcPos tTree 1010058 1010059

p101v33v101v56 = T.mkSrcPos tTree 1010033 1010056

p101v33v101v41 = T.mkSrcPos tTree 1010033 1010041

p101v43v101v49 = T.mkSrcPos tTree 1010043 1010049

p101v62v101v86 = T.mkSrcPos tTree 1010062 1010086

p101v62v101v73 = T.mkSrcPos tTree 1010062 1010073
