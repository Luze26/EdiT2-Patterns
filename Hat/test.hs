module Main where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

gmain pmain p = T.constUse pmain p smain

smain =
  T.constDef T.mkRoot amain
    (\ p ->
      T.uapp1 p1v8v1v35 p1v8v1v12 p aprint hprint
        (T.app5 p1v15v1v35 p1v15v1v25 p arepartition hrepartition
          (T.ap1 p1v27v1v27 p (Hat.PreludeBasic.gfromInteger p1v27v1v27 p)
            (T.conInteger p1v27v1v27 p 9))
          (T.ap1 p1v29v1v29 p (Hat.PreludeBasic.gfromInteger p1v29v1v29 p)
            (T.conInteger p1v29v1v29 p 4))
          (T.ap1 p1v31v1v31 p (Hat.PreludeBasic.gfromInteger p1v31v1v31 p)
            (T.conInteger p1v31v1v31 p 0))
          (T.ap1 p1v33v1v33 p (Hat.PreludeBasic.gfromInteger p1v33v1v33 p)
            (T.conInteger p1v33v1v33 p 1))
          (T.ap1 p1v35v1v35 p (Hat.PreludeBasic.gfromInteger p1v35v1v35 p)
            (T.conInteger p1v35v1v35 p 0))))

grepartition ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun Int
            (T.Fun Int (T.Fun Int (T.Fun Int (T.Tuple2 Bool (T.List Int)))))))

hrepartition ::
  (T.R Int) ->
    (T.R Int) ->
      (T.R Int) ->
        (T.R Int) -> (T.R Int) -> T.RefExp -> T.R (T.Tuple2 Bool (T.List Int))

grepartition prepartition p = T.fun5 arepartition prepartition p hrepartition

hrepartition fnbP fn fa fb fm p =
  T.cguard p5v11v5v17 p
    (T.ap2 p5v11v5v17 p (p5v15v5v15 !> p) (gsub p5v11v5v13 p)
      (T.ap1 p5v17v5v17 p (Hat.PreludeBasic.gfromInteger p5v17v5v17 p)
        (T.conInteger p5v17v5v17 p 0)))
    (\ p ->
      let
        gok pok p = T.constUse pok p sok
        glist pok p = T.constUse pok p slist
        j5v25v5v34ok =
          case
            T.app5 p5v38v5v60 p5v38v5v48 p arepartition hrepartition
              (gsub p5v50v5v52 p) fn fa fb fm of
            T.R (T.Tuple2 fok flist) kok -> (kok,fok,flist)
            _ -> T.fatal p
        sok =
          T.constDef p a5v26v5v27ok
            (\ _ ->
              case j5v25v5v34ok of
                (kok,fok,flist) -> T.projection p5v26v5v27 kok fok)
        slist =
          T.constDef p a5v30v5v33list
            (\ _ ->
              case j5v25v5v34ok of
                (kok,fok,flist) -> T.projection p5v30v5v33 kok flist) in
        (T.cif p5v65v5v169 p (gok p5v68v5v69 p)
          (\ p ->
            T.con2 p5v76v5v88 p T.Tuple2 T.aTuple2 (gok p5v77v5v78 p)
              (T.con2 p5v81v5v87 p T.Cons T.aCons (gnb p5v81v5v82 p)
                (glist p5v84v5v87 p)))
          (\ p ->
            let
              gok2 pok2 p = T.constUse pok2 p sok2
              glist2 pok2 p = T.constUse pok2 p slist2
              j5v99v5v110ok2 =
                case gdecrease p5v114v5v121 p of
                  T.R (T.Tuple2 fok2 flist2) kok2 -> (kok2,fok2,flist2)
                  _ -> T.fatal p
              sok2 =
                T.constDef p a5v100v5v102ok2
                  (\ _ ->
                    case j5v99v5v110ok2 of
                      (kok2,fok2,flist2) -> T.projection p5v100v5v102 kok2 fok2)
              slist2 =
                T.constDef p a5v105v5v109list2
                  (\ _ ->
                    case j5v99v5v110ok2 of
                      (kok2,fok2,flist2) ->
                        T.projection p5v105v5v109 kok2 flist2) in
              (T.cif p5v126v5v169 p (gok2 p5v129v5v131 p)
                (\ p ->
                  T.con2 p5v138v5v152 p T.Tuple2 T.aTuple2 (gok2 p5v139v5v141 p)
                    (T.con2 p5v144v5v151 p T.Cons T.aCons (gnb p5v144v5v145 p)
                      (glist2 p5v147v5v151 p)))
                (\ p ->
                  T.con2 p5v159v5v169 p T.Tuple2 T.aTuple2
                    (T.con0 p5v160v5v164 p False aFalse)
                    (T.con0 p5v167v5v168 p T.List T.aList))))))
    (\ p ->
      T.cguard p6v11v6v18 p
        (T.ap2 p6v11v6v18 p (p6v15v6v16 !== p) (gsub p6v11v6v13 p)
          (T.ap1 p6v18v6v18 p (Hat.PreludeBasic.gfromInteger p6v18v6v18 p)
            (T.conInteger p6v18v6v18 p 0)))
        (\ p ->
          T.con2 p6v22v6v33 p T.Tuple2 T.aTuple2
            (T.con0 p6v23v6v26 p True aTrue)
            (T.fromExpList p6v29v6v32 p [gnb p6v30v6v31 p]))
        (\ p ->
          T.cguard p7v11v7v19 p (gotherwise p7v11v7v19 p)
            (\ p -> gdecrease p7v23v7v30 p) (\ p -> T.fatal p)))
  where
  
  gnb pnb p = T.constUse pnb p snb
  
  snb =
    T.constDef p a9v17v9v24nb
      (\ p -> T.ap2 p9v22v9v24 p (p9v23v9v23 !- p) fn fm)
  
  gsub psub p = T.constUse psub p ssub
  
  ssub =
    T.constDef p a10v17v10v28sub
      (\ p ->
        T.ap2 p10v23v10v28 p (p10v26v10v26 !- p) fnbP (gnb p10v27v10v28 p))
  
  gdecrease pdecrease p = T.constUse pdecrease p sdecrease
  
  sdecrease =
    T.constDef p a11v17v11v93decrease
      (\ p ->
        T.cif p11v28v11v93 p
          (T.uapp2 p11v31v11v41 p11v35v11v36 p (+&&) (*&&)
            (T.ap2 p11v31v11v33 p (p11v32v11v32 !< p) fm fb)
            (T.ap2 p11v38v11v41 p (p11v40v11v40 !> p) (gnb p11v38v11v39 p)
              (T.ap1 p11v41v11v41 p
                (Hat.PreludeBasic.gfromInteger p11v41v11v41 p)
                (T.conInteger p11v41v11v41 p 2))))
          (\ p ->
            T.app5 p11v49v11v74 p11v49v11v59 p arepartition hrepartition fnbP fn
              fa fb
              (T.ap2 p11v72v11v74 p (p11v73v11v73 !+ p) fm
                (T.ap1 p11v74v11v74 p
                  (Hat.PreludeBasic.gfromInteger p11v74v11v74 p)
                  (T.conInteger p11v74v11v74 p 1))))
          (\ p ->
            T.con2 p11v83v11v93 p T.Tuple2 T.aTuple2
              (T.con0 p11v84v11v88 p False aFalse)
              (T.con0 p11v91v11v92 p T.List T.aList)))
  

tMain = T.mkModule "Main" "test.hs" Prelude.True

amain = T.mkVariable tMain 10001 10035 3 0 "main" Prelude.False

arepartition = T.mkVariable tMain 40001 110093 3 5 "repartition" Prelude.False

a9v17v9v24nb = T.mkVariable tMain 90017 90024 3 0 "nb" Prelude.True

a10v17v10v28sub = T.mkVariable tMain 100017 100028 3 0 "sub" Prelude.True

a11v17v11v93decrease =
  T.mkVariable tMain 110017 110093 3 0 "decrease" Prelude.True

a5v26v5v27ok = T.mkVariable tMain 50026 50027 3 0 "ok" Prelude.True

a5v30v5v33list = T.mkVariable tMain 50030 50033 3 0 "list" Prelude.True

a5v100v5v102ok2 = T.mkVariable tMain 50100 50102 3 0 "ok2" Prelude.True

a5v105v5v109list2 = T.mkVariable tMain 50105 50109 3 0 "list2" Prelude.True

p1v1v1v35 = T.mkSrcPos tMain 10001 10035

p1v8v1v35 = T.mkSrcPos tMain 10008 10035

p1v8v1v12 = T.mkSrcPos tMain 10008 10012

p1v15v1v35 = T.mkSrcPos tMain 10015 10035

p1v15v1v25 = T.mkSrcPos tMain 10015 10025

p1v27v1v27 = T.mkSrcPos tMain 10027 10027

p1v29v1v29 = T.mkSrcPos tMain 10029 10029

p1v31v1v31 = T.mkSrcPos tMain 10031 10031

p1v33v1v33 = T.mkSrcPos tMain 10033 10033

p1v35v1v35 = T.mkSrcPos tMain 10035 10035

p4v1v11v93 = T.mkSrcPos tMain 40001 110093

p9v17v9v24 = T.mkSrcPos tMain 90017 90024

p9v22v9v24 = T.mkSrcPos tMain 90022 90024

p9v23v9v23 = T.mkSrcPos tMain 90023 90023

p10v17v10v28 = T.mkSrcPos tMain 100017 100028

p10v23v10v28 = T.mkSrcPos tMain 100023 100028

p10v26v10v26 = T.mkSrcPos tMain 100026 100026

p10v27v10v28 = T.mkSrcPos tMain 100027 100028

p11v17v11v93 = T.mkSrcPos tMain 110017 110093

p11v28v11v93 = T.mkSrcPos tMain 110028 110093

p11v31v11v41 = T.mkSrcPos tMain 110031 110041

p11v35v11v36 = T.mkSrcPos tMain 110035 110036

p11v31v11v33 = T.mkSrcPos tMain 110031 110033

p11v32v11v32 = T.mkSrcPos tMain 110032 110032

p11v38v11v41 = T.mkSrcPos tMain 110038 110041

p11v40v11v40 = T.mkSrcPos tMain 110040 110040

p11v38v11v39 = T.mkSrcPos tMain 110038 110039

p11v41v11v41 = T.mkSrcPos tMain 110041 110041

p11v49v11v74 = T.mkSrcPos tMain 110049 110074

p11v49v11v59 = T.mkSrcPos tMain 110049 110059

p11v72v11v74 = T.mkSrcPos tMain 110072 110074

p11v73v11v73 = T.mkSrcPos tMain 110073 110073

p11v74v11v74 = T.mkSrcPos tMain 110074 110074

p11v83v11v93 = T.mkSrcPos tMain 110083 110093

p11v84v11v88 = T.mkSrcPos tMain 110084 110088

p11v91v11v92 = T.mkSrcPos tMain 110091 110092

p5v11v5v17 = T.mkSrcPos tMain 50011 50017

p5v15v5v15 = T.mkSrcPos tMain 50015 50015

p5v11v5v13 = T.mkSrcPos tMain 50011 50013

p5v17v5v17 = T.mkSrcPos tMain 50017 50017

p5v26v5v27 = T.mkSrcPos tMain 50026 50027

p5v30v5v33 = T.mkSrcPos tMain 50030 50033

p5v38v5v60 = T.mkSrcPos tMain 50038 50060

p5v38v5v48 = T.mkSrcPos tMain 50038 50048

p5v50v5v52 = T.mkSrcPos tMain 50050 50052

p5v65v5v169 = T.mkSrcPos tMain 50065 50169

p5v68v5v69 = T.mkSrcPos tMain 50068 50069

p5v76v5v88 = T.mkSrcPos tMain 50076 50088

p5v77v5v78 = T.mkSrcPos tMain 50077 50078

p5v81v5v87 = T.mkSrcPos tMain 50081 50087

p5v81v5v82 = T.mkSrcPos tMain 50081 50082

p5v84v5v87 = T.mkSrcPos tMain 50084 50087

p5v100v5v102 = T.mkSrcPos tMain 50100 50102

p5v105v5v109 = T.mkSrcPos tMain 50105 50109

p5v114v5v121 = T.mkSrcPos tMain 50114 50121

p5v126v5v169 = T.mkSrcPos tMain 50126 50169

p5v129v5v131 = T.mkSrcPos tMain 50129 50131

p5v138v5v152 = T.mkSrcPos tMain 50138 50152

p5v139v5v141 = T.mkSrcPos tMain 50139 50141

p5v144v5v151 = T.mkSrcPos tMain 50144 50151

p5v144v5v145 = T.mkSrcPos tMain 50144 50145

p5v147v5v151 = T.mkSrcPos tMain 50147 50151

p5v159v5v169 = T.mkSrcPos tMain 50159 50169

p5v160v5v164 = T.mkSrcPos tMain 50160 50164

p5v167v5v168 = T.mkSrcPos tMain 50167 50168

p6v11v6v18 = T.mkSrcPos tMain 60011 60018

p6v15v6v16 = T.mkSrcPos tMain 60015 60016

p6v11v6v13 = T.mkSrcPos tMain 60011 60013

p6v18v6v18 = T.mkSrcPos tMain 60018 60018

p6v22v6v33 = T.mkSrcPos tMain 60022 60033

p6v23v6v26 = T.mkSrcPos tMain 60023 60026

p6v29v6v32 = T.mkSrcPos tMain 60029 60032

p6v30v6v31 = T.mkSrcPos tMain 60030 60031

p7v11v7v19 = T.mkSrcPos tMain 70011 70019

p7v23v7v30 = T.mkSrcPos tMain 70023 70030

main = T.traceIO "test" (Main.gmain T.mkNoSrcPos T.mkRoot)
