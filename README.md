# Lipid-Haskell
Lipid analysis from mass spectrometric data. Haskell's type system is used to represent lipid classes, lipid ions and mass spectrometry data. Additional features will be added with time.

##Nomenclature

The shorthand notation as described by Liebisch et. al. [1] is used with minor modifications. These modifications include:

- The inclusion of n-nomenclature. Using technologies such as OzID [2], it is possible to determine double bond position relative to the methyl terminus even when the chain lengths of lipid radyls are unknown. In the internally defined data types, it is possible to indicate double bond position using either the n or delta nomenclatures. When radyl lengths are unknown, double bond position is indicated using the n-nomeclature. When radyl lengths are known, the shorthand notation uses the delta nomenclature as described by Liebisch et. al.

- Liebisch et. al. indicate that lysophospholipids can be written using the notation for normal glycerophosholipid classes in addition to the notation used specifically for lysophospholipids (i.e., PC 16:0/0:0 vs LPC 16:0/0:0). I believe that it is preferrable to only have one representation and since it is not possible to have a lipid such as TG 0:0/16:0/18:1, I suggest this idea is extended to glycerophosholipids. Therefore, the shorthand notation of lysophospholipids uses only the L-prefix form (i.e., LPC 16:0/0:0). 

- For lysophospholipids where the sn-position of the radyl is undefined, Liebisch et. al. allow the following notations: LPC 16:0 and PC 16:0_0:0. Instead of either of these notations, the previous lipid is represented here as LPC 16:0_0:0. This removes dublicate representations and provides greater consistency within the adapted shorthand notation.

The adapted shorthand notation uses the polymorphic function showShorthand (i.e. showShorthand <lipid>). In addition, lipids may also be represented using the n-nomenclature by using the function showNnomenclature (i.e. showNnomenclature <lipid>). Currently the showNnomenclature function lists the position of all double bonds. This will be changed to only show the position of the double bond closest to the methyl terminus (provided double bonds are methylene-interupted within polyunsaturated radyls).

## Future plans

### Include sphingolipids and sterols

Currently, fatty acids, glycerolipids, glycerophospholipids and lysoglycerophospholipids are represented as Haskell data types. This should be extended to also include sphingolipids and sterols.

### Mass calculations

In the experimental module Formulae, functions for calculating monoisotopic, average and integer masses are defined for simple fatty acids (note: masses are calculated from molecular formulae). These need to be extended to all lipid data types.

### SMILES strings

Functions will be defined for converting lipid data structures to SMILES strings.

### Parser for reading shorthand notation

When working with the interpreter, it is adventagous to be able to make lipid data types by parsing the shorthand notation (as strings). 

### Modeling biochemical pathways

Functions can be defined to represent enzymatic reactions. As a proof of principle, the biosynthesis of n-3 polyunsaturated fatty acids is modeled below by using functions representing desaturase, elongation and beta oxidation reactions-

In this example, ala is a variable (value) representing α-linolenic acid (ALA). When working with in GHCI, entering ala reveals the internal representation of ALA.
*Pathways> ala
FA (SimpleCarbonChain {carbonNumber = 18, doubleBonds = [DoubleBond {dbPosition = Just (Delta 9), geometry = Just Cis},DoubleBond {dbPosition = Just (Delta 12), geometry = Just Cis},DoubleBond {dbPosition = Just (Delta 15), geometry = Just Cis}]})

Using the showShorthand function, we can get the shorthand representation of ALA-
*Pathways> showShorthand $ ala
"FA 18:3(9Z,12Z,15Z)"

Using the elongation function, we can perform a chain elongation reaction-
*Pathways> showShorthand $ elongation ala
"FA 20:3(11Z,14Z,17Z)"

Using function composition, we can chain together functions to model sequential biochemical modifications-
*Pathways> showShorthand $ elongation . delta6Desaturase $ ala
"FA 20:4(8Z,11Z,14Z,17Z)"

This can be done to represent an entire pathway-
*Pathways> let product = betaOxidation . delta6Desaturase . elongation . elongation . delta5Desaturase . elongation . delta6Desaturase $ ala

Here, the product is DHA-
*Pathways> showShorthand product
"FA 22:6(4Z,7Z,10Z,13Z,16Z,19Z)"

Now having synthesised a new lipid, we can get its molecular formula-
*Pathways> getFormula product
MolecularFormula [(C,22),(O,2),(H,32)]

And from its formula we can calculate its monoisotopic mass-
*Pathways> monoisotopicMass . getFormula $ product
328.24023027050004

## Concluding remarks

The example above is a powerful example of what can be done by harnessing Haskell's data type system and using functional programming. Interestingly, this is only just scratching the surface of what is possible in Haskell as advanced Haskell programming techniques have not yet been applied here. Therefore, I suspect this should be an interesting area of research which has the potential to improve computational lipidomics.

##References

[1] Liebisch G.; Vizcaino J.A.; Kofeler H.; Trotzmuller M.; Griffiths W.J.; Schmitz G.; Spener F.; Wakelam M.J.O. Shorthand notation for lipid structures derived from mass spectrometry. The Journal of Lipid Research. 2013, 54, 1523-1530.

[2] Thomas, M.C.; Mitchell, T.W.; Blanksby, S.J. Ozonolysis of phospholipid double bonds during electrospray ionization: a new tool for structure determination. Journal of the American Chemical Society. 2006, 128, 58-59.
