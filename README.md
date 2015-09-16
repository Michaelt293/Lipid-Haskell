# Lipid-Haskell
Lipid analysis from mass spectrometric data. Haskell's type system is used to represent lipid classes, lipid ions and mass spectrometry data. Additional features will be added with time.

##Nomenclature

The shorthand notation as described by Liebisch et. al. [1] is used with minor modifications. These modifications include:

- The inclusion of n-nomenclature. Using technologies such as OzID [2], it is possible to determine double bond position relative to the methyl terminus even when the chain lengths of lipid radyls are unknown. In the internally defined data types, it is possible to indicate double bond position using either the n or delta nomenclatures. When radyl lengths are unknown, double bond position is indicated using the n-nomeclature. When radyl lengths are known, the shorthand notation uses the delta nomenclature as described by Liebisch et. al.

- Liebisch et. al. indicate that lysophospholipids can be written using the notation for normal glycerophosholipid classes in addition to the notation used specifically for lysophospholipids (i.e., PC 16:0/0:0 vs LPC 16:0/0:0). I believe that it is preferrable to only have one representation and since it is not possible to have a lipid such as TG 0:0/16:0/18:1, I suggest this idea is extended to glycerophosholipids. Therefore, the shorthand notation of lysophospholipids uses only the L-prefix form (i.e., LPC 16:0/0:0). 

- For lysophospholipids where the sn-position of the radyl is undefined, Liebisch et. al. allow the following notations: LPC 16:0 and PC 16:0_0:0. Instead of either of these notations, the previous lipid is represented here as LPC 16:0_0:0. This removes dublicate representations and provides greater consistency within the adapted shorthand notation.

The adapted shorthand notation uses the polymorphic function showShorthand (i.e. showShorthand <lipid>). In addition, lipids may also be represented using the n-nomenclature by using the function showNnomenclature (i.e. showNnomenclature <lipid>). Currently the showNnomenclature function lists the position of all double bonds. This will be changed to only show the position of the double bond closest to the methyl terminus (provided double bonds are methylene-interupted within polyunsaturated radyls).

##References

[1] Liebisch G.; Vizcaino J.A.; Kofeler H.; Trotzmuller M.; Griffiths W.J.; Schmitz G.; Spener F.; Wakelam M.J.O. Shorthand notation for lipid structures derived from mass spectrometry. The Journal of Lipid Research. 2013, 54, 1523-1530.

[2] Thomas, M.C.; Mitchell, T.W.; Blanksby, S.J. Ozonolysis of phospholipid double bonds during electrospray ionization: a new tool for structure determination. Journal of the American Chemical Society. 2006, 128, 58-59.
