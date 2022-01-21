{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Prelude
import Data.List as L

solve :: [Char] -> [(Int, [Char], [Char])]
solve regexInput = simplifiedRegexList $ regexList
    where 
      regexList = lines regexInput 
      simplifiedRegexList = sortBy sortSimplifiedRegexPairs . map (simplifyAndMatch) 
      sortSimplifiedRegexPairs (x,_,_) (y,_,_) 
          | x < y = LT
          | x > y = GT
          | otherwise = EQ

simplifyAndMatch :: [Char] -> (Int, [Char], [Char])
simplifyAndMatch regex = (evalMatches,regex,simplifiedRegex)
    where (paraStack, squareStack) = parseRegex regex
          simplifiedRegex 
               | (lenParaStack == 0) && (lenSquareStack == 0) = ""
               | head regex == '(' =  if 
                   | lenSquareStack > 0 -> "(" ++ finalParaRegex ++ finalSquareRegex ++ ")"
                   | otherwise -> "(" ++ finalParaRegex ++ ")"
               | head regex == '[' = if
                   | lenParaStack > 0 -> "[" ++ comBineSqaureParaRegex ++ "]"
                   | otherwise -> "[" ++ squareStack ++ "]"
               | otherwise = ""
          evalMatches 
              | head regex == '(' = if 
                  | lenParaStack > 0 -> if 
                      | lenSquareStack > 0 -> lenSquareStack                 --------- (a[bc]) matches ab and ac and nothing else
                      | otherwise -> 1                                       --------- (a(bc)) is the same as (abc) and matches abc and nothing else
                  | otherwise -> if 
                      | lenSquareStack > 0 -> lenSquareStack                 --------- ([abc]) same as [abc]
                      | otherwise -> 0              
              | head regex == '[' = if               
                  | lenSquareStack  > 0 -> if 
                      | lenParaStack > 0 -> length groupedLettersFromParaStack + length comBineSingleLettersFromParaAndSquare   ---------- [a(bc)] matches a and bc and nothing else
                      | otherwise -> lenSquareStack                       
                  | otherwise -> if 
                      | lenParaStack > 0 -> lenParaStack                     ---------- [(a)(bc)(gh)]
                      | otherwise -> 0 
              | otherwise = 0
          lenParaStack = length paraStack
          lenSquareStack = length squareStack
          finalParaRegex = filter (`elem` ['a'..'z']) . concat $ paraStack
          finalSquareRegex = "[" ++ squareStack ++ "]"
          splitSingleAndGroupedLettersFromPara = partition (\x -> length x == 1) paraStack
          singleLettersFromParaStack = fst splitSingleAndGroupedLettersFromPara 
          groupedLettersFromParaStack = snd splitSingleAndGroupedLettersFromPara
          comBineSingleLettersFromParaAndSquare = nub $ squareStack ++ concat singleLettersFromParaStack  -- remove duplicates of single letters from () & [] expressions
          comBineSqaureParaRegex = comBineSingleLettersFromParaAndSquare ++  concat groupedLettersFromParaStack

parseRegex :: [Char] -> ([[Char]], [Char])
parseRegex regex =  if checkForBalancedBrackets == 0 then (recfunc [] [] [] regex) else ([""],"")
    where recfunc parenStack squareStack accumStack [] =  (finalParaStack,finalSquareStack)
              where finalParaStack = nub $ map (\x -> if length x == 3 then (drop 1 . init) x else x) parenStack
                    finalSquareStack = nub squareStack
          recfunc parenStack squareStack accumStack ls@(head:tail)
              | length ls == 1 = if
                  | head == ')' -> recfunc (parenStack ++ [accumStack ++ ")"]) squareStack accumStack tail
                  | head == ']' -> recfunc parenStack (drop 1 accumStack ++ squareStack) accumStack tail
              | head `elem` "([" = recfunc parenStack squareStack (accumStack ++ [head]) tail
              | head == ')' = if
                  | last accumStack == '(' -> recfunc parenStack squareStack (init accumStack) tail 
                  | otherwise -> recfunc newParaStack squareStack newAccumStack tail
              | head == ']' = if 
                  | last accumStack == '[' -> recfunc parenStack squareStack (init accumStack) tail 
                  | otherwise -> recfunc parenStack newSquareStack newAccumStack tail
              | otherwise = recfunc parenStack squareStack (accumStack ++ [head]) tail
                  where chopClosedExpr
                            | head == ')' = (takeWhile (/= '(') . reverse) accumStack
                            | head == ']' = (takeWhile (/= '[') . reverse) accumStack
                        lengthOfClosedExpr = length chopClosedExpr
                        newAccumStack =  take (length (accumStack) - (lengthOfClosedExpr + 1)) accumStack
                        newParaStack =  parenStack ++ ["(" ++ (reverse chopClosedExpr) ++ ")"]
                        newSquareStack = squareStack ++ (reverse chopClosedExpr)
          checkForBalancedBrackets =  length (filter (`elem` "()[]") regex) `mod` 2

main :: IO ()
main = do
  input <-
    getArgs >>= \case
      [] -> do
        pn <- getProgName
        putStrLn $ "Usage: " <> pn <> " <the.regexes>"
        exitFailure
      (fn : _) -> readFile fn
  print $ solve input

{--- Testing
(base) 192:Haskell dev$ stack exec -- ghc main.hs -o artificial-regex
[1 of 1] Compiling Main             ( main.hs, main.o )
Linking artificial-regex ...
(base) 192:Haskell dev$ ./artificial-regex 
Usage: artificial-regex <the.regexes>
(base) 192:Haskell dev$ ./artificial-regex the.regexes 
[(1,"((([a]f)gagg))","(fgagg[a])"),(1,"(bfa(a(d[g])e))","(daebfa[g])"),(1,"(fbbd(gb(cga)bc[g])a[(ag)])","(cgagbbcagfbbda[g])"),(2,"(cdad[bd]cf)","(cdadcf[bd])"),(2,"((g(e(((ge[b[c]]c)))))dfgfda(g)[b(e)bb(efe)]e(f))","(gecegefefdfgfdae[cb])"),(3,"[g(f(dg))]","[gf(dg)]"),(3,"(ff((ca))[((e))((c)((c))b)b([((((e)d)))g]b)a]fb)","(caecbdfffb[gba])"),(4,"[f((db)b)c]","[fcb(db)]"),(4,"(([[e]f[bc]]ega)bd)","(egabd[ebcf])"),(4,"(bc[f[eb]bc]cdc(([e])g[(((c)))cb]))","(cgbccdc[ebfc])"),(4,"(g[g[b]eg]acf[c]acdad[eb[bgg][[c]((ca))c]])","(cagacfacdad[bgec])"),(4,"((([(d)]g)f[e]be)ffbe[ag](([([g])a]b)f[cea]g((g))g)c)","(dgfbebfggffbec[eagc])"),(4,"(d(eegfcf)f(ce[[d]][bc]f(c([[b]])f)b)c(dae[c(b)d]a[a]))","(eegfcfcfcefbbdaeadfc[dbca])"),(4,"(cf(([(ee)]g)((b)))[(dda)][(bfc)fe[b((g))][e]][egfbggb])","(eegbddabfccf[befg])"),(4,"(cf(([(ee)]g)((b)))[(dda)][(bfc)fe[b((g))][e]][egfbggb])","(eegbddabfccf[befg])"),(4,"((c[(a)e][a]a([[d]][g((eg))]g))(ec(cg((a)))fafb)bb(cd(e)bbb)cd[a]b)","(aeggcacgecfafbecdbbbbbcdb[eadg])"),(5,"[egfg[dd]a]","[egfad]"),(5,"[c[e][cfc(ee)]bc]","[cbef(ee)]"),(5,"((cbgddfb)c[ccfe][g]cgdd[a])","(cbgddfbccgdd[cfega])"),(5,"(g(g)ad([(((d)))e]c[a])(d)(ef(f)fa[cg][(b[[d]]g)e[[(e)]]])c)","(gdcfbgeeffagadc[eacgd])"),(5,"(eab[[e(e)]cc[cc]f]b[([b](aadg)(fa(cbd))a(c(a)e))g[eebe]e]g)","(eaadgcbdfaaceeabbg[ecfbg])"),(5,"([ff[d]]e[((e)ff)aeac(gbe)[da]][d[[[[([[(a)]]c)]]](b(g))](e)])","(effgbeacgb[dfaec])"),(5,"(e([b(([[d]])d)(fc)][c(b(c)[d])bd](e[(c)])cgd)faeedbe[e(c)gcg])","(dfccbecgdefaeedbe[dbceg])"),(5,"(fag[e([a][(a)]b)(e)[e([a])]ge]([(([ede]))b[[g]]db](bde)(b(c))b))","(abebdecfag[aegdb])"),(5,"(da((ga)(g)a)(ee[g[e[f]]c(dc)]e)f((d)[ga]da([(cb)(f)c]))eaa(be)ea)","(gagadceeedcbfdabedafeaaea[fegca])"),(5,"([d[aab][(f)]](ag)c[da(g)[[f]][g(d(b))]a(be)]((e[[fb[d(g)]]]g))a[f]gd)","(faggbdbeegcagd[abdfg])"),(5,"(bb(db[(([((([(e)])))[g[f]]]))([d])[d]]adff)g(g[e][ad]ag((c[g])e)a)b[e])","(edbadffcgagabbgb[fgdea])"),(5,"([b[[[[c]]a]g]((g[(ae)]g)(eb)g)(d((d))e)(ga)]gg[(a([g])c)a(e(d))((d))adc])","(aeggebgddegaace[cagbd])"),(5,"(bc(ac[d[(b)]]e[(e)[f([c](ca))]((a)a)]g)d[fgc[c]f](ae((e)(d(b)))gf)[ag]d((b)egg))","(becaaacegdaegfeggbcdd[dcfga])"),(6,"[g(e)b(((g)d)bb[ea]gda)]","[gbead(bbgda)]"),(6,"[[fe[ae]dde][(f)g[cgc]]dd]","[daefcg]"),(6,"((b)d[fgd(g)[[d[a]](g)bf][gcg]b]f)","(bgdf[adbfgc])"),(6,"[f((g((a))[[(ba)]])([f[[[(gb)]e]]]))a]","[faeg(ba)(gb)]"),(6,"[ebgc[efb[((b[[f]]))c(e(((b))))c]]f[(dd)b(f)b]e]","[ebgcf(dd)]"),(6,"(bf[(f)[a((b))][dd]([f][b([(a)])]a)df]d([g[e]]d)[b])","(fbadbfd[adfbeg])"),(6,"((bcf[(a[eb])a(g)]bg[[g]a[b]])agf((bg(d))(d[cf]c)ee)bbb)","(agbcfbgdbgdceeagfbbb[ebagcf])"),(6,"((cfa[db[(c)][(a)]]([e]bc)d)b[fg[(gc)bf]]dfc[a(ff)e]ebdgga)","(cabccfadgcffbdfcebdgga[dbefga])"),(6,"([((bc))]g[[[[d]][(g)]]age]g[(([((e))]d)(((a)g))(g))bgac]b(a))","(bcgedaggb[dagebc])"),(6,"(c(e[[ab]ceg]gcc)d((((f[[g]])g)[[e]][(g)])g)[c[[a]][(fc)fa[c]f]])","(egccfgfccd[abcegf])"),(6,"(beb[(c(c))]d[e[g][g]g][ge[c(([b][(([d]))])c)[g(ce)aa]d]eg](c)ce)","(ccebebdce[gebdac])"),(6,"(cf[e]ebg((d)[abc]fad[g])[(a[[c]][eg])[[[[c]]]]de]([[[e](g)b]]cd))","(dfadagcdcfebg[eabcgd])"),(6,"((f)(fe)([g[a]]bd[ee[ea(([g]c[ef]))]]ab[e([[g]d]g[ac][(f)])[e]]f)a)","(ffecgbdabfa[agefdc])"),(6,"(ccg(cf[f[e]]c)geg[(adge)[[d(c)]cf(([(b)]))]cgcb]dae(ebb(b)b[(bg)]f)c)","(cfcadgecbbgebbbfccggegdaec[efdcgb])"),(6,"([(g[(e)])]ec[f(a)]bda(baf)fe[c[[[[[(g[bc])]][ea]]](f(([(af)](bb))))d]acd]g)","(egabafafbbfecbdafeg[fbcead])"),(6,"(f((dc)[[[e]](e)]aa)bb(d)[[[c(e)][g]]b(d)f](dca(ga)dg[[[gc]]d[g((a))]])cfeb)","(dceaadgaadcadgfbbcfeb[ecgbfd])"),(6,"((d)fb(g[gfce]ff)f([c]([(g)]fa)[e((([b])f))][(b)])[[d]cg(ce(ea))eeg]e([gdd]c))","(dgffgfafbeacecfbfe[gfcebd])"),(6,"(([[(dd)]c]gf)fdfgcgaa[fb]g[a(c([d]))be][bb(b)b[bde]((gf)[[((d))([a])]])c]g[fb(fa)c(c)])","(ddgfcbdfafdfgcgaagg[cfbdae])"),(6,"(fg[g](b([ae])(f))([[[e](e)]][[a][[e]]])eecbd(f[cbd]d[ae](([([[((c(e)))]])]f))d)([[d]][e]fb)((((([[(gcc)]]))))f))","(fbecfddfbgccfgeecbd[gaecbd])"),(7,"[ba[([([f])g](g)d)(f)][(be)d]cd]","[bacdfg(be)]"),(7,"[d[e[c][[b]]([(e)]g[a])]([f[c]b]c)fgedcc]","[dfgecba]"),(7,"[e[c[e]e(((g)(e))a)]ef[ga[(df)f]g][([a]fd)f]]","[efcga(df)(fd)]"),(7,"[[c[e]d]ce(fea[(c([fc]))ad(fb)][[[[[c]]]]]d)cea]","[ceadf(fb)(fead)]"),(7,"((af)[[(a)d]edce([[d]b]gc)e]d([f][[g(d(f))]a])ce)","(afagcfddce[dbecfga])"),(7,"((af)[[(a)d]edce([[d]b]gc)e]d([f][[g(d(f))]a])ce)","(afagcfddce[dbecfga])"),(7,"[gf(bde)a[b([([([g])])][((g)[([a])])((d(a))b)])de]eega]","[gfaebd(bde)]"),(7,"(ffad[dcf](cgf[f[[[d][[ae]b]]]g])ee(eb[[(a)e]cb]cd(c[b])a)c)","(cgfacebcdaffadeec[dcfaebg])"),(7,"[c[[(f)]fe[f(e[f])([[a][d]])e][eg]][fe[e](([([dg]b)])f)](e)eg]","[cegfadb]"),(7,"(([[[fa]b]c([((d))])]([bgdf]))ea(de((g)[([(f)]c)[e]])(e)(cg)g))","(dgfcecgdegea[fabcgde])"),(7,"([ab[a[[[[aa]]]][g(b)]]]bc([[([ab])]c]bcbc[eg(b)][[d]e[e]])d[f[d(e)(a)[d]][[b]]b])","(bbcbceabcd[agbcedf])"),(7,"((g)gc[[be][f(e)(([d]))]g[d]ec((a))]dc([d]d)d[[a]](gd[g]bb)(g(a((fg)[(d)][d]f)a(f[c]b)e)g)db)","(geadgdbbfgffbaaegggcdcddb[bedfgca])"),(7,"((cg(aa)d[([f((bb))])f][(g[g])e]c)cgggb[(f)fa[(g[f])aa]]b(dbf[(([g]))gb](g)f)c([(([[c]]))][df]f)bdb)","(aabbgcgdcfdbffcgggbbcbdb[fgeabcd])"),(7,"(fb[f(c[e]c)dg([[[[((([([d])])))e]]]]a)]fa[ed[ff(d)([(([[fd]](f)))][a])][[(b)f][(f)](c)]c([ab]a)]g[da[bf(a(gg))]]f)","(ccadfbcggfbfagf[edfgabc])"),(7,"(g[f([eb]ad)[cac(c)]f([[(b)]])g[eeeg]]ggdebbe[d(b([c[[[[d]]]]])[(a)])[[ga]dag]]([e[e([(e)])]fg]a[gb[cf]]ab([([d])]g(g)))df)","(adcbaegaabgggdebbedf[ebcagfd])"),(7,"([defd([[[d]]]b)c[(ba)]]aag([(g[c])]b)f(aad(e)ga(a[a]))(c(((g))b)b(g)g(g)c)[(de)][eb[cee]e]ggf[[f]dgbg[(f)[(c)(([b]))]][[c]]])","(bbageaaadgacbgcdefcaagfggf[defcabg])"),(7,"(ca[[((e))bb]e](eed)b[[eba]fb]f[e(abge)(g)gb]g[([(d)](g))f[cdd][e][fec[c]]c[ba((da))]]ab(d)(ggc([b]c)b[a[(e)](([[[(e)](da)]])(d(e)[c]))](d)))","(eeedabgegddacggcbcabfgab[beafgcd])"),(7,"(([a[e]e]ef(fa))[bb([d])(c)ba(ab)e]b([c(g)f]g)b(c(gc[[([[b]])a]])(f[[ff]]a)(e([c]e)))cb(fdaea)[[([[b]])bd]ga[[a]bg]a]((((c[[g]])))b(gc(a[ae]))g)a)","(faefcabggcefdaeaabgbbcba[eadbcfg])"),(8,"[b(f[daabf](g[c][a([[[e]]c])])(c)b(c(f)))eff]","[befdacg(fb)]"),(8,"[bd(dcaeg)[[gc]b]g[(g)c[b]([a]c)[dgb[[[ea][(d)]]b]]]fd]","[bdgfcae(dcaeg)]"),(8,"[dc[[a][e[c]d][(e)[ba]d]e](b[[f]ad]d[[f[[c]][ga]]aab]gfc)]","[dcaebfg(bdgfc)]"),(8,"[e[ce[[d]][f]]aedcgbfbd(a)dc[g((gf))[[e]d([cb[c](a)]c)]ga]]","[eadcgbf(gf)]"),(8,"[(c[e]geb([da]g((([d]e)c)e))([bd](d(e))[ed]c)f)[(g)b]cfc[ef]]","[cfedabg(cgebf)]"),(8,"[ab[(e)[a[e(b)]]ec[[[e]](c[ag][b])[be]]]bdaf[ea[(f)[g(g)]]b][f(bc)be(b)]]","[abdfegc(bc)]"),(8,"[ce[a[b[[[da]]]a]c]dc[ag]fa[f[g]][f[([[be]])][cb]][((a)de)[dfb(d)]e(f)]b(([e(g)]([[e]d[ge]]))[ef])b(a)]","[cedfabg(de)]"),(9,"[cg((c)[([(a)])([f])b]a)dfaeae(bdb[gdg])(gfb)]","[cgdfaeb(bdb)(gfb)]"),(9,"[[cg]((b((b))))g(((g)eb)[bfba]c)[ge[[c](gf)]dgb]eccc]","[gecbfad(eb)(gf)]"),(9,"[[cg]((b((b))))g(((g)eb)[bfba]c)[ge[[c](gf)]dgb]eccc]","[gecbfad(eb)(gf)]"),(9,"[dec[cbfff(be)](dd(c)a)[d[[b]]ee([b]a)f]eg[g([(g)])(c)]]","[decgbfa(be)(dda)]"),(9,"[dec[cbfff(be)](dd(c)a)[d[[b]]ee([b]a)f]eg[g([(g)])(c)]]","[decgbfa(be)(dda)]"),(9,"[c(dad[g][[[e](g)]fe])ee[begd[(a(bg)(f))ad]e][cabb[f[c]b][ega]g]e]","[cegfadb(dad)(bg)]"),(9,"[[eg]f([[[([g])b]a]b(c)]gdaff[g((c))])(g(a[(f)([(g)])]b)[c((c))])d]","[fdegbac(gdaff)(ab)]"),(9,"[g[g(f(b(b))b)([[[[(g)]]]])[(f)b]g][a(d)d]c[(af)b]gg[a]fc[decfd]ac]","[gcfabde(fb)(af)]"),(9,"[ee[[a((f))d]f]gb([d]f(f))(b[e]cef(a[c])gg)[[f]gb]gce[(c(([d]))f)c]ef]","[egbcfad(bcefgg)(cf)]"),(9,"[eb[cegg(bd)b]([ccg][b])cd(b(g[[f]](g)c))c[dd[gb[b]]bgc][c((f))(e(a))]feedg]","[ebcdfga(bd)(gc)]"),(9,"[[geaf][dfgg[a](d[[b]])(e)][e[((c))f][d]]gd(f[[bf]]fc)(db)[[g[[c]]a][ag]]cc]","[gdceafb(ffc)(db)]"),(9,"[be[de[([g])(b)]ad]e(a[[[(g)(d)]c]bd](cab((ae)[e])))[g(b)[c(((((d)))))[(abc)[c]]]eab]aceg]","[beacgd(ae)(cab)(abc)]"),(10,"[(ff(bcaf)[[ad]]c)c[d]fggfgf(dd)[cbf]ae]","[cfgaedb(bcaf)(ffc)(dd)]"),(10,"[fdbbddae([b]cf[ed]b)ge(c[(de)([a])([b(ge)])][fc])]","[fdbaegc(cfb)(de)(ge)]"),(10,"[[g[b]dd(dec)b]baagbdec((e)(e)[b]a((f))b[f[f]])[(fgg)]bbdf]","[bagdecf(dec)(ab)(fgg)]"),(10,"[d[gcabd]a([[([a]a)c[d[f]]][ffd](ge)b](aga))(gg(f)fgf(cf(a)))]","[dagcbf(ge)(aga)(cf)(ggfgf)]"),(10,"[(([ac])b)c(c[a])[[[a]de]([d]c)(a)ga](g[[b(cc)d]]ff)a(fbdf(ca)a)ccecg]","[caegdb(cc)(gff)(ca)(fbdfa)]"),(10,"[ab[[b][ag[[(df)b]f]e]][[d[[([bb]e)]]]]b[ggg(c)](dgdb(((e(ag)))a)(([e]b)d))[bb]bda]","[abdfgec(df)(ag)(dgdb)]"),(10,"[[gb(([f[f]])f[f])[[((b)g)]g]((b))[[cd](g)(a)]c]fa(d[[c]][b]d)d[ced[be]]((ag[(e)a])fc)bf(((e)))a]","[fadbgce(dd)(ag)(fc)]"),(10,"[([fdd](cea)(d[[e](b)[[g][[(c)f]b]]a][[(f)c][c]]))gce[[(c)[f[b]]]eagcf]dgf(c[e[[a(d)]][g]]ab(ef)f)]","[gcedfba(cea)(ef)(cabf)]"),(10,"[[(c)(fea)([a]g)(fcd[[a][cb]])[g([c])]]d[(b(e))]bcf(g[gec]([d](a)c)ff)cg[[[g]g][[ga]]g[c](a)]((e)([e])e)(f)]","[dbcfgae(fea)(fcd)(gff)]"),(10,"[a(a[(g)]g([[[[(([[b(b)](g)]))]]]]((d(d)))[a(c)])g[g([d]((c)))])(d[[[([f[e]][eg])]]g[[[f]]]]egf)([(e[([c]b)e])][(d)d]dd([a])b)[cd((d))c]]","[abdgefc(agg)(degf)(ddb)]"),(11,"[(df)b(f((g)a)((f))c)abea([((c)c)f]b[[be]](de(d))a)ce[[b]]]","[baecfgd(df)(fc)(de)(ba)]"),(12,"[a[cee[cbg(f(cf)f)]ecc]g(baae[g](c))d(d)fgcf[d(a)c](dcd(a)(([([f])])(f)(a))c[gb[b([[b[g]]]g(aef))]])abg((f[b])d)]","[agdfcbe(cf)(ff)(baae)(aef)(dcdc)]"),(13,"[c[(ba)eg(fff)f]g((ce)c)(([[e]])g)e((fb)gce)fbdc([c](add)gdfa)f]","[cgefbd(ba)(fff)(ce)(fb)(gce)(add)(gdfa)]"),(13,"[ca[[(f)[(a)]]a([f][(ga)f[g]])(cc)]ceeb[g](g[(c)c]d(g)e(([c]e)(d)bf)[g])[d(ea)aed][[g]c[gb[f]]]([[[[(g)]]bd]g[a]]d[g(d)]a[d[g[c]]]g)a[ab[g]]]","[caebfgd(ga)(cc)(bf)(gde)(ea)(dag)]"),(14,"[fa(gg(fd))eee(c[c][[f]](bb[[dc]])(ad)a)[((([[b]]))b)e(fb)]gf(dcg)]","[faegcdb(fd)(gg)(bb)(ad)(ca)(fb)(dcg)]")]
-}
