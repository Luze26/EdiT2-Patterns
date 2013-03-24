stage=/home/zang/Dev/Haskell/Stage
rt=/home/zang/Dev/Haskell/Stage/RT

ghc --make $rt/RT.hs
echo -e "\n\n*** Results ***\n"
$rt/RT $stage/out/RT.t2 Information {participants = [\"Pierre\",\"Marc\",\"Lucie\",\"Lea\",\"Jean\",\"Soph\",\"Alice\"], passages = [\"klkl\",\"p1\",\"3333\"], nbParticipantsPerGroup = 3}
cat $stage/out/RT.t2
