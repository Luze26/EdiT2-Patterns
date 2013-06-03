module Hat.TreeGenerator (Name(),Level(),ggenerate,agenerate,hgenerate) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Tree 

type Name = String

type Level = T.Tuple2 Name (T.List (T.List (T.List String)))

ggenerate :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List Level) (NTree Cell))

hgenerate :: (T.R (T.List Level)) -> T.RefExp -> T.R (NTree Cell)

ggenerate pgenerate p = T.fun1 agenerate pgenerate p hgenerate

hgenerate (T.R (T.Cons fl flvls) _) p =
  T.uapp2 p19v21v19v72 p19v49v19v49 p (+$) (*$)
    (T.pa1 Node T.cn1 p19v21v19v47 p aNode
      (T.con4 p19v26v19v47 p T.Tuple4 T.aTuple4
        (T.fromLitString p19v27v19v32 p "Root")
        (T.fromLitString p19v34v19v36 p "1")
        (T.fromLitString p19v38v19v43 p "null")
        (T.con0 p19v45v19v46 p T.List T.aList)))
    (T.app4 p19v51v19v72 p19v51v19v59 p agenerate' hgenerate' fl flvls
      (T.fromLitString p19v68v19v70 p "1")
      (T.ap1 p19v72v19v72 p (Hat.PreludeBasic.gfromInteger p19v72v19v72 p)
        (T.conInteger p19v72v19v72 p 1)))
hgenerate _ p = T.fatal p

ggenerate' ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Level
          (T.Fun (T.List Level)
            (T.Fun String (T.Fun Int (T.List (NTree Cell))))))

hgenerate' ::
  (T.R Level) ->
    (T.R (T.List Level)) ->
      (T.R String) -> (T.R Int) -> T.RefExp -> T.R (T.List (NTree Cell))

ggenerate' pgenerate' p = T.fun4 agenerate' pgenerate' p hgenerate'

hgenerate' (T.R (T.Tuple2 _ (T.R T.List _)) _) _ _ _ p =
  T.con0 p24v27v24v28 p T.List T.aList
hgenerate'
  (T.R
    (T.Tuple2 fname (T.R (T.Cons (T.R (T.Cons fr (T.R T.List _)) _) frows) _))
    _) flvls fpid fi p =
  T.con2 p25v42v25v110 p T.Cons T.aCons
    (T.con2 p25v42v25v72 p Node aNode
      (T.con4 p25v47v25v64 p T.Tuple4 T.aTuple4 fname (gid p25v54v25v55 p) fpid
        fr) (gsubtree p25v66v25v72 p))
    (T.app4 p25v77v25v110 p25v77v25v85 p agenerate' hgenerate'
      (T.con2 p25v87v25v98 p T.Tuple2 T.aTuple2 fname frows)
      (gls p25v100v25v101 p) fpid
      (T.ap2 p25v108v25v110 p (p25v109v25v109 !+ p) fi
        (T.ap1 p25v110v25v110 p (Hat.PreludeBasic.gfromInteger p25v110v25v110 p)
          (T.conInteger p25v110v25v110 p 1))))
  where
  
  gid pid p = T.constUse pid p sid
  
  sid =
    T.constDef p a27v17v27v35id
      (\ p ->
        T.uapp2 p27v22v27v35 p27v26v27v27 p (+++) (*++) fpid
          (T.ap1 p27v30v27v35 p (gshow p27v30v27v33 p) fi))
  
  gls pls p = T.constUse pls p sls
  
  gsubtree pls p = T.constUse pls p ssubtree
  
  j28v17v28v29ls =
    case
      T.cguard p29v27v29v36 p
        (T.ap2 p29v27v29v36 p (p29v32v29v33 !== p) flvls
          (T.con0 p29v35v29v36 p T.List T.aList))
        (\ p ->
          T.con2 p29v40v29v47 p T.Tuple2 T.aTuple2
            (T.con0 p29v41v29v42 p T.List T.aList)
            (T.con0 p29v45v29v46 p T.List T.aList))
        (\ p ->
          T.cguard p30v27v30v35 p (gotherwise p30v27v30v35 p)
            (\ p ->
              T.app4 p30v39v30v77 p30v39v30v48 p agenerate'' hgenerate''
                (T.uapp1 p30v51v30v59 p30v51v30v54 p ahead hhead flvls)
                (T.uapp1 p30v63v30v71 p30v63v30v66 p atail htail flvls)
                (gid p30v74v30v75 p)
                (T.ap1 p30v77v30v77 p
                  (Hat.PreludeBasic.gfromInteger p30v77v30v77 p)
                  (T.conInteger p30v77v30v77 p 1))) (\ p -> T.fatal p)) of
      T.R (T.Tuple2 fls fsubtree) kls -> (kls,fls,fsubtree)
      _ -> T.fatal p
  
  sls =
    T.constDef p a28v18v28v19ls
      (\ _ ->
        case j28v17v28v29ls of
          (kls,fls,fsubtree) -> T.projection p28v18v28v19 kls fls)
  
  ssubtree =
    T.constDef p a28v22v28v28subtree
      (\ _ ->
        case j28v17v28v29ls of
          (kls,fls,fsubtree) -> T.projection p28v22v28v28 kls fsubtree)
  
hgenerate' _ _ _ _ p = T.fatal p

ggenerate'' ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Level
          (T.Fun (T.List Level)
            (T.Fun String
              (T.Fun Int (T.Tuple2 (T.List Level) (T.List (NTree Cell)))))))

hgenerate'' ::
  (T.R Level) ->
    (T.R (T.List Level)) ->
      (T.R String) ->
        (T.R Int) ->
          T.RefExp -> T.R (T.Tuple2 (T.List Level) (T.List (NTree Cell)))

ggenerate'' pgenerate'' p = T.fun4 agenerate'' pgenerate'' p hgenerate''

hgenerate'' (T.R (T.Tuple2 fname (T.R T.List _)) _) flvls _ _ p =
  T.con2 p35v34v35v53 p T.Tuple2 T.aTuple2
    (T.con2 p35v35v35v48 p T.Cons T.aCons
      (T.con2 p35v35v35v43 p T.Tuple2 T.aTuple2 fname
        (T.con0 p35v41v35v42 p T.List T.aList)) flvls)
    (T.con0 p35v51v35v52 p T.List T.aList)
hgenerate'' (T.R (T.Tuple2 fname (T.R (T.Cons fr frows) _)) _) flvls fpid fi p =
  T.cguard p37v11v37v17 p
    (T.ap2 p37v11v37v17 p (p37v13v37v14 !== p) fr
      (T.con0 p37v16v37v17 p T.List T.aList))
    (\ p ->
      T.con2 p37v21v37v48 p T.Tuple2 T.aTuple2
        (T.con2 p37v22v37v38 p T.Cons T.aCons
          (T.con2 p37v22v37v33 p T.Tuple2 T.aTuple2 fname frows)
          (glsub p37v35v37v38 p)) (gsubtree p37v41v37v47 p))
    (\ p ->
      T.cguard p38v11v38v19 p (gotherwise p38v11v38v19 p)
        (\ p ->
          T.con2 p38v23v38v75 p T.Tuple2 T.aTuple2 (gls p38v24v38v25 p)
            (T.con2 p38v29v38v74 p T.Cons T.aCons
              (T.con2 p38v29v38v64 p Node aNode
                (T.con4 p38v34v38v56 p T.Tuple4 T.aTuple4 fname
                  (gid p38v41v38v42 p) fpid
                  (T.uapp1 p38v50v38v55 p38v50v38v53 p ahead hhead fr))
                (gsubtree p38v58v38v64 p)) (gnexttree p38v67v38v74 p)))
        (\ p -> T.fatal p))
  where
  
  gid pid p = T.constUse pid p sid
  
  sid =
    T.constDef p a40v17v40v35id
      (\ p ->
        T.uapp2 p40v22v40v35 p40v26v40v27 p (+++) (*++) fpid
          (T.ap1 p40v30v40v35 p (gshow p40v30v40v33 p) fi))
  
  glsub plsub p = T.constUse plsub p slsub
  
  gsubtree plsub p = T.constUse plsub p ssubtree
  
  j41v17v41v31lsub =
    case
      T.cguard p42v27v42v36 p
        (T.ap2 p42v27v42v36 p (p42v32v42v33 !== p) flvls
          (T.con0 p42v35v42v36 p T.List T.aList))
        (\ p ->
          T.con2 p42v40v42v47 p T.Tuple2 T.aTuple2
            (T.con0 p42v41v42v42 p T.List T.aList)
            (T.con0 p42v45v42v46 p T.List T.aList))
        (\ p ->
          T.cguard p43v27v43v35 p (gotherwise p43v27v43v35 p)
            (\ p ->
              T.app4 p43v39v43v77 p43v39v43v48 p agenerate'' hgenerate''
                (T.uapp1 p43v51v43v59 p43v51v43v54 p ahead hhead flvls)
                (T.uapp1 p43v63v43v71 p43v63v43v66 p atail htail flvls)
                (gid p43v74v43v75 p)
                (T.ap1 p43v77v43v77 p
                  (Hat.PreludeBasic.gfromInteger p43v77v43v77 p)
                  (T.conInteger p43v77v43v77 p 1))) (\ p -> T.fatal p)) of
      T.R (T.Tuple2 flsub fsubtree) klsub -> (klsub,flsub,fsubtree)
      _ -> T.fatal p
  
  slsub =
    T.constDef p a41v18v41v21lsub
      (\ _ ->
        case j41v17v41v31lsub of
          (klsub,flsub,fsubtree) -> T.projection p41v18v41v21 klsub flsub)
  
  ssubtree =
    T.constDef p a41v24v41v30subtree
      (\ _ ->
        case j41v17v41v31lsub of
          (klsub,flsub,fsubtree) -> T.projection p41v24v41v30 klsub fsubtree)
  
  gls pls p = T.constUse pls p sls
  
  gnexttree pls p = T.constUse pls p snexttree
  
  j44v17v44v30ls =
    case
      T.app4 p44v34v44v79 p44v34v44v43 p agenerate'' hgenerate''
        (T.con2 p44v45v44v65 p T.Tuple2 T.aTuple2 fname
          (T.con2 p44v53v44v64 p T.Cons T.aCons
            (T.uapp1 p44v53v44v58 p44v53v44v56 p atail htail fr) frows))
        (glsub p44v67v44v70 p) fpid
        (T.ap2 p44v77v44v79 p (p44v78v44v78 !+ p) fi
          (T.ap1 p44v79v44v79 p (Hat.PreludeBasic.gfromInteger p44v79v44v79 p)
            (T.conInteger p44v79v44v79 p 1))) of
      T.R (T.Tuple2 fls fnexttree) kls -> (kls,fls,fnexttree)
      _ -> T.fatal p
  
  sls =
    T.constDef p a44v18v44v19ls
      (\ _ ->
        case j44v17v44v30ls of
          (kls,fls,fnexttree) -> T.projection p44v18v44v19 kls fls)
  
  snexttree =
    T.constDef p a44v22v44v29nexttree
      (\ _ ->
        case j44v17v44v30ls of
          (kls,fls,fnexttree) -> T.projection p44v22v44v29 kls fnexttree)
  
hgenerate'' _ _ _ _ p = T.fatal p

tTreeGenerator =
  T.mkModule "TreeGenerator" "../../src/Util/TreeGenerator.hs" Prelude.True

agenerate =
  T.mkVariable tTreeGenerator 190001 190072 3 1 "generate" Prelude.False

agenerate' =
  T.mkVariable tTreeGenerator 240001 300077 3 4 "generate'" Prelude.False

agenerate'' =
  T.mkVariable tTreeGenerator 350001 440079 3 4 "generate''" Prelude.False

a27v17v27v35id = T.mkVariable tTreeGenerator 270017 270035 3 0 "id" Prelude.True

a28v18v28v19ls = T.mkVariable tTreeGenerator 280018 280019 3 0 "ls" Prelude.True

a28v22v28v28subtree =
  T.mkVariable tTreeGenerator 280022 280028 3 0 "subtree" Prelude.True

a40v17v40v35id = T.mkVariable tTreeGenerator 400017 400035 3 0 "id" Prelude.True

a41v18v41v21lsub =
  T.mkVariable tTreeGenerator 410018 410021 3 0 "lsub" Prelude.True

a41v24v41v30subtree =
  T.mkVariable tTreeGenerator 410024 410030 3 0 "subtree" Prelude.True

a44v18v44v19ls = T.mkVariable tTreeGenerator 440018 440019 3 0 "ls" Prelude.True

a44v22v44v29nexttree =
  T.mkVariable tTreeGenerator 440022 440029 3 0 "nexttree" Prelude.True

p19v1v19v72 = T.mkSrcPos tTreeGenerator 190001 190072

p19v21v19v72 = T.mkSrcPos tTreeGenerator 190021 190072

p19v49v19v49 = T.mkSrcPos tTreeGenerator 190049 190049

p19v21v19v47 = T.mkSrcPos tTreeGenerator 190021 190047

p19v26v19v47 = T.mkSrcPos tTreeGenerator 190026 190047

p19v27v19v32 = T.mkSrcPos tTreeGenerator 190027 190032

p19v34v19v36 = T.mkSrcPos tTreeGenerator 190034 190036

p19v38v19v43 = T.mkSrcPos tTreeGenerator 190038 190043

p19v45v19v46 = T.mkSrcPos tTreeGenerator 190045 190046

p19v51v19v72 = T.mkSrcPos tTreeGenerator 190051 190072

p19v51v19v59 = T.mkSrcPos tTreeGenerator 190051 190059

p19v68v19v70 = T.mkSrcPos tTreeGenerator 190068 190070

p19v72v19v72 = T.mkSrcPos tTreeGenerator 190072 190072

p24v1v30v77 = T.mkSrcPos tTreeGenerator 240001 300077

p24v27v24v28 = T.mkSrcPos tTreeGenerator 240027 240028

p27v17v27v35 = T.mkSrcPos tTreeGenerator 270017 270035

p27v22v27v35 = T.mkSrcPos tTreeGenerator 270022 270035

p27v26v27v27 = T.mkSrcPos tTreeGenerator 270026 270027

p27v30v27v35 = T.mkSrcPos tTreeGenerator 270030 270035

p27v30v27v33 = T.mkSrcPos tTreeGenerator 270030 270033

p28v18v28v19 = T.mkSrcPos tTreeGenerator 280018 280019

p28v22v28v28 = T.mkSrcPos tTreeGenerator 280022 280028

p29v27v29v36 = T.mkSrcPos tTreeGenerator 290027 290036

p29v32v29v33 = T.mkSrcPos tTreeGenerator 290032 290033

p29v35v29v36 = T.mkSrcPos tTreeGenerator 290035 290036

p29v40v29v47 = T.mkSrcPos tTreeGenerator 290040 290047

p29v41v29v42 = T.mkSrcPos tTreeGenerator 290041 290042

p29v45v29v46 = T.mkSrcPos tTreeGenerator 290045 290046

p30v27v30v35 = T.mkSrcPos tTreeGenerator 300027 300035

p30v39v30v77 = T.mkSrcPos tTreeGenerator 300039 300077

p30v39v30v48 = T.mkSrcPos tTreeGenerator 300039 300048

p30v51v30v59 = T.mkSrcPos tTreeGenerator 300051 300059

p30v51v30v54 = T.mkSrcPos tTreeGenerator 300051 300054

p30v63v30v71 = T.mkSrcPos tTreeGenerator 300063 300071

p30v63v30v66 = T.mkSrcPos tTreeGenerator 300063 300066

p30v74v30v75 = T.mkSrcPos tTreeGenerator 300074 300075

p30v77v30v77 = T.mkSrcPos tTreeGenerator 300077 300077

p25v42v25v110 = T.mkSrcPos tTreeGenerator 250042 250110

p25v42v25v72 = T.mkSrcPos tTreeGenerator 250042 250072

p25v47v25v64 = T.mkSrcPos tTreeGenerator 250047 250064

p25v54v25v55 = T.mkSrcPos tTreeGenerator 250054 250055

p25v66v25v72 = T.mkSrcPos tTreeGenerator 250066 250072

p25v77v25v110 = T.mkSrcPos tTreeGenerator 250077 250110

p25v77v25v85 = T.mkSrcPos tTreeGenerator 250077 250085

p25v87v25v98 = T.mkSrcPos tTreeGenerator 250087 250098

p25v100v25v101 = T.mkSrcPos tTreeGenerator 250100 250101

p25v108v25v110 = T.mkSrcPos tTreeGenerator 250108 250110

p25v109v25v109 = T.mkSrcPos tTreeGenerator 250109 250109

p25v110v25v110 = T.mkSrcPos tTreeGenerator 250110 250110

p35v1v44v79 = T.mkSrcPos tTreeGenerator 350001 440079

p35v34v35v53 = T.mkSrcPos tTreeGenerator 350034 350053

p35v35v35v48 = T.mkSrcPos tTreeGenerator 350035 350048

p35v35v35v43 = T.mkSrcPos tTreeGenerator 350035 350043

p35v41v35v42 = T.mkSrcPos tTreeGenerator 350041 350042

p35v51v35v52 = T.mkSrcPos tTreeGenerator 350051 350052

p40v17v40v35 = T.mkSrcPos tTreeGenerator 400017 400035

p40v22v40v35 = T.mkSrcPos tTreeGenerator 400022 400035

p40v26v40v27 = T.mkSrcPos tTreeGenerator 400026 400027

p40v30v40v35 = T.mkSrcPos tTreeGenerator 400030 400035

p40v30v40v33 = T.mkSrcPos tTreeGenerator 400030 400033

p41v18v41v21 = T.mkSrcPos tTreeGenerator 410018 410021

p41v24v41v30 = T.mkSrcPos tTreeGenerator 410024 410030

p42v27v42v36 = T.mkSrcPos tTreeGenerator 420027 420036

p42v32v42v33 = T.mkSrcPos tTreeGenerator 420032 420033

p42v35v42v36 = T.mkSrcPos tTreeGenerator 420035 420036

p42v40v42v47 = T.mkSrcPos tTreeGenerator 420040 420047

p42v41v42v42 = T.mkSrcPos tTreeGenerator 420041 420042

p42v45v42v46 = T.mkSrcPos tTreeGenerator 420045 420046

p43v27v43v35 = T.mkSrcPos tTreeGenerator 430027 430035

p43v39v43v77 = T.mkSrcPos tTreeGenerator 430039 430077

p43v39v43v48 = T.mkSrcPos tTreeGenerator 430039 430048

p43v51v43v59 = T.mkSrcPos tTreeGenerator 430051 430059

p43v51v43v54 = T.mkSrcPos tTreeGenerator 430051 430054

p43v63v43v71 = T.mkSrcPos tTreeGenerator 430063 430071

p43v63v43v66 = T.mkSrcPos tTreeGenerator 430063 430066

p43v74v43v75 = T.mkSrcPos tTreeGenerator 430074 430075

p43v77v43v77 = T.mkSrcPos tTreeGenerator 430077 430077

p44v18v44v19 = T.mkSrcPos tTreeGenerator 440018 440019

p44v22v44v29 = T.mkSrcPos tTreeGenerator 440022 440029

p44v34v44v79 = T.mkSrcPos tTreeGenerator 440034 440079

p44v34v44v43 = T.mkSrcPos tTreeGenerator 440034 440043

p44v45v44v65 = T.mkSrcPos tTreeGenerator 440045 440065

p44v53v44v64 = T.mkSrcPos tTreeGenerator 440053 440064

p44v53v44v58 = T.mkSrcPos tTreeGenerator 440053 440058

p44v53v44v56 = T.mkSrcPos tTreeGenerator 440053 440056

p44v67v44v70 = T.mkSrcPos tTreeGenerator 440067 440070

p44v77v44v79 = T.mkSrcPos tTreeGenerator 440077 440079

p44v78v44v78 = T.mkSrcPos tTreeGenerator 440078 440078

p44v79v44v79 = T.mkSrcPos tTreeGenerator 440079 440079

p37v11v37v17 = T.mkSrcPos tTreeGenerator 370011 370017

p37v13v37v14 = T.mkSrcPos tTreeGenerator 370013 370014

p37v16v37v17 = T.mkSrcPos tTreeGenerator 370016 370017

p37v21v37v48 = T.mkSrcPos tTreeGenerator 370021 370048

p37v22v37v38 = T.mkSrcPos tTreeGenerator 370022 370038

p37v22v37v33 = T.mkSrcPos tTreeGenerator 370022 370033

p37v35v37v38 = T.mkSrcPos tTreeGenerator 370035 370038

p37v41v37v47 = T.mkSrcPos tTreeGenerator 370041 370047

p38v11v38v19 = T.mkSrcPos tTreeGenerator 380011 380019

p38v23v38v75 = T.mkSrcPos tTreeGenerator 380023 380075

p38v24v38v25 = T.mkSrcPos tTreeGenerator 380024 380025

p38v29v38v74 = T.mkSrcPos tTreeGenerator 380029 380074

p38v29v38v64 = T.mkSrcPos tTreeGenerator 380029 380064

p38v34v38v56 = T.mkSrcPos tTreeGenerator 380034 380056

p38v41v38v42 = T.mkSrcPos tTreeGenerator 380041 380042

p38v50v38v55 = T.mkSrcPos tTreeGenerator 380050 380055

p38v50v38v53 = T.mkSrcPos tTreeGenerator 380050 380053

p38v58v38v64 = T.mkSrcPos tTreeGenerator 380058 380064

p38v67v38v74 = T.mkSrcPos tTreeGenerator 380067 380074
