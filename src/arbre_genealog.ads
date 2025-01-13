with Arbre_Bin;
with Personne;

package Arbre_Genealog is

   package Arbre_Personnes is new Arbre_Bin (
      Personne.T_Personne, 
      Integer, 
      Personne.To_String, 
      Integer'Image
   );
   use Arbre_Personnes;

   type T_Arbre is new Arbre_Personnes.T_Arbre;

   procedure Create_Family_Tree(Root : in out T_Arbre; Root_ID : Integer; Root_Value : Personne.T_Personne);

   procedure Display_Family_Tree(Tree : in T_Arbre);

end Arbre_Genealog;
