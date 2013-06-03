module Hat.Constraints
  (Cmd(),Cstr(Cstr,bitems,bcommand,bwher),aCstr,gitems,gcommand,gwher,hitems
    ,hcommand,hwher,aitems,acommand,awher,Identificator(Label,Content
      ,Identificator),aLabel,aContent,aIdentificator,Identificators()) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

type Cmd = String

data Identificator =
  Label (T.R String) | Content (T.R String)
  | Identificator (T.R String) (T.R String)

instance T.WrapVal (Identificator)
  where
  
  wrapVal pwrapVal (kwrapVal@(Label (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aLabel z1wrapVal)
  wrapVal pwrapVal (kwrapVal@(Content (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aContent z1wrapVal)
  wrapVal pwrapVal
    (kwrapVal@(Identificator (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aIdentificator z1wrapVal z2wrapVal)
  

instance Show (Identificator)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a17v92v17v95showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Label fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "Label "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec fy1 (T.R (Content fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "Content "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec fy1 (T.R (Identificator fy2 fy3) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "Identificator "))
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
    
  

instance Read (Identificator)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a17v98v17v101readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 9)))
            (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.pa0 Label T.cn1 T.mkNoSrcPos p aLabel))
                  (T.fromLitString T.mkNoSrcPos p "Label") p))
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)))) p))
        (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 9)))
              (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                (T.uwrapForward p
                  (Hat.PreludeBasic.hthenLex
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                      (T.pa0 Content T.cn1 T.mkNoSrcPos p aContent))
                    (T.fromLitString T.mkNoSrcPos p "Content") p))
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)))) p))
          (T.uwrapForward p
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
                        (T.pa0 Identificator T.cn2 T.mkNoSrcPos p
                          aIdentificator))
                      (T.fromLitString T.mkNoSrcPos p "Identificator") p))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p 10))))
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)))) p)))
    
  

instance Eq (Identificator)
  where
  
  (!==) (%==) p =
    T.ufun2 (+#^=#!&=#^=#!*==) (%==) p (*==)
    where
    
    (*==) (T.R (Label fy1) _) (T.R (Label fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) (T.R (Content fy1) _) (T.R (Content fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) (T.R (Identificator fy1 fy2) _) (T.R (Identificator fy3 fy4) _) p =
      T.uwrapForward p
        (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy3)
            Hat.Prelude.*&&
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy2 fy4)) p)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

type Identificators = T.List Identificator

data Cstr =
  Cstr
    {bitems :: T.R Identificators,bcommand :: T.R Cmd
      ,bwher :: T.R Identificator}

instance T.WrapVal (Cstr)
  where
  
  wrapVal pwrapVal
    (kwrapVal@(Cstr (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aCstr z1wrapVal z2wrapVal z3wrapVal)
  

gitems pitems p = T.ufun1 aitems pitems p hitems

hitems (T.R z1items _) p = T.projection T.mkNoSrcPos p (bitems z1items)

gcommand pcommand p = T.ufun1 acommand pcommand p hcommand

hcommand (T.R z1command _) p = T.projection T.mkNoSrcPos p (bcommand z1command)

gwher pwher p = T.ufun1 awher pwher p hwher

hwher (T.R z1wher _) p = T.projection T.mkNoSrcPos p (bwher z1wher)

instance Show (Cstr)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a27v21v27v24showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Cstr fy2 fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
          (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Cstr{"))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.Prelude.gshowString T.mkNoSrcPos p)
                    (T.fromLitString T.mkNoSrcPos p "items"))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                    (T.conChar T.mkNoSrcPos p '=')))
                (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 0)) fy2))
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ',')))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.Prelude.gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "command"))
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                      (T.conChar T.mkNoSrcPos p '=')))
                  (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p 0)) fy3))
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                  (T.conChar T.mkNoSrcPos p ',')))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.Prelude.gshowString T.mkNoSrcPos p)
                    (T.fromLitString T.mkNoSrcPos p "wher"))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                    (T.conChar T.mkNoSrcPos p '=')))
                (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 0)) fy4)))))
        (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
          (T.conChar T.mkNoSrcPos p '}'))
    hshowsPrec _ _ p = T.fatal p
    
  

instance Read (Cstr)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a27v27v27v30readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uwrapForward p
        (Hat.PreludeBasic.hthenLex
          (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uwrapForward p
                  (Hat.PreludeBasic.hthenLex
                    (T.uwrapForward p
                      (Hat.PreludeBasic.hthenLex
                        (T.uap2 T.mkNoSrcPos p
                          (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                          (T.uwrapForward p
                            (Hat.PreludeBasic.hthenLex
                              (T.uwrapForward p
                                (Hat.PreludeBasic.hthenLex
                                  (T.uwrapForward p
                                    (Hat.PreludeBasic.hthenLex
                                      (T.uap2 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gthenAp T.mkNoSrcPos
                                          p)
                                        (T.uwrapForward p
                                          (Hat.PreludeBasic.hthenLex
                                            (T.uwrapForward p
                                              (Hat.PreludeBasic.hthenLex
                                                (T.uwrapForward p
                                                  (Hat.PreludeBasic.hthenLex
                                                    (T.uwrapForward p
                                                      (Hat.PreludeBasic.hthenLex
                                                        (T.uap1 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.gyield
                                                            T.mkNoSrcPos p)
                                                          (T.pa0 Cstr T.cn3
                                                            T.mkNoSrcPos p
                                                            aCstr))
                                                        (T.fromLitString
                                                          T.mkNoSrcPos p "Cstr")
                                                        p))
                                                    (T.fromLitString
                                                      T.mkNoSrcPos p "{") p))
                                                (T.fromLitString T.mkNoSrcPos p
                                                  "items") p))
                                            (T.fromLitString T.mkNoSrcPos p "=")
                                            p))
                                        (T.uap1 T.mkNoSrcPos p
                                          (Hat.Prelude.greadsPrec T.mkNoSrcPos
                                            p)
                                          (T.uap1 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.gfromInteger
                                              T.mkNoSrcPos p)
                                            (T.conInteger T.mkNoSrcPos p 0))))
                                      (T.fromLitString T.mkNoSrcPos p ",") p))
                                  (T.fromLitString T.mkNoSrcPos p "command") p))
                              (T.fromLitString T.mkNoSrcPos p "=") p))
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p 0))))
                        (T.fromLitString T.mkNoSrcPos p ",") p))
                    (T.fromLitString T.mkNoSrcPos p "wher") p))
                (T.fromLitString T.mkNoSrcPos p "=") p))
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 0))))
          (T.fromLitString T.mkNoSrcPos p "}") p)
    
  

tConstraints =
  T.mkModule "Constraints" "../../src/Util/Constraints.hs" Prelude.True

aLabel = T.mkConstructor tConstraints 170022 170026 3 1 "Label"

aContent = T.mkConstructor tConstraints 170037 170043 3 1 "Content"

aIdentificator = T.mkConstructor tConstraints 170054 170066 3 2 "Identificator"

aCstr =
  T.mkConstructorWFields tConstraints 230009 230012 3 3 "Cstr"
    (aitems : (acommand : (awher : [])))

aitems = T.mkVariable tConstraints 240017 240021 3 1 "items" Prelude.False

acommand = T.mkVariable tConstraints 250017 250023 3 1 "command" Prelude.False

awher = T.mkVariable tConstraints 260017 260020 3 1 "wher" Prelude.False

a17v92v17v95showsPrec =
  T.mkVariable tConstraints 170092 170095 3 2 "showsPrec" Prelude.False

a17v98v17v101readsPrec =
  T.mkVariable tConstraints 170098 170101 3 1 "readsPrec" Prelude.False

(+#^=#!&=#^=#!*==) =
  T.mkVariable tConstraints 170104 170105 3 2 "==" Prelude.False

a27v21v27v24showsPrec =
  T.mkVariable tConstraints 270021 270024 3 2 "showsPrec" Prelude.False

a27v27v27v30readsPrec =
  T.mkVariable tConstraints 270027 270030 3 1 "readsPrec" Prelude.False

p17v22v17v26 = T.mkSrcPos tConstraints 170022 170026

p17v37v17v43 = T.mkSrcPos tConstraints 170037 170043

p17v54v17v66 = T.mkSrcPos tConstraints 170054 170066

p17v92v17v95 = T.mkSrcPos tConstraints 170092 170095

p17v98v17v101 = T.mkSrcPos tConstraints 170098 170101

p17v104v17v105 = T.mkSrcPos tConstraints 170104 170105

p23v9v23v12 = T.mkSrcPos tConstraints 230009 230012

p24v17v24v21 = T.mkSrcPos tConstraints 240017 240021

p25v17v25v23 = T.mkSrcPos tConstraints 250017 250023

p26v17v26v20 = T.mkSrcPos tConstraints 260017 260020

p27v21v27v24 = T.mkSrcPos tConstraints 270021 270024

p27v27v27v30 = T.mkSrcPos tConstraints 270027 270030
