with Arbre_Bin;
with Personne;

package Arbre_Genealog is

   package Arbre_Personnes is new Arbre_Bin (
      Personne.T_Personne,
      Personne.To_String
   );
   use Arbre_Personnes;

   type T_Arbre_Personnes is new T_Arbre;

   procedure Create_Family_Tree(Root : in out T_Arbre_Personnes; Root_Value : Personne.T_Personne);

   procedure Display_Family_Tree(Tree : in T_Arbre_Personnes);

   function Get_Father (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   function Get_Mother (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   function Get_Node_By_Id (Tree : in T_Arbre_Personnes; Id_Child : String) return T_Arbre_Personnes;

   function Count_Ancestors(Tree: in T_Arbre_Personnes; Id_Node : in String) return Integer;

end Arbre_Genealog;
