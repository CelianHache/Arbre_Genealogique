with Arbre_Bin;
with Personne;

package Arbre_Genealog is

   package Arbre_Personnes is new Arbre_Bin (
      Personne.T_Personne,
      Personne.To_String
   );
   use Arbre_Personnes;

   type T_Arbre_Personnes is new T_Arbre;
   type Ancestor_Array is array (Positive range <>) of Personne.T_Personne;  -- Tableau de taille dynamique

   procedure Create_Family_Tree(Root : in out T_Arbre_Personnes; Root_Value : Personne.T_Personne);

   procedure Display_Family_Tree(Tree : in T_Arbre_Personnes);

   function Get_Father (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   function Get_Mother (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   function Get_Node_By_Id (Tree : in T_Arbre_Personnes; Id_Child : String) return T_Arbre_Personnes;

   function Get_Ancestors_Generation (Tree : in T_Arbre_Personnes; Generation: Integer) return Ancestor_Array;

   function Get_Sorted_Ancestor_Array(List : in Ancestor_Array) return Ancestor_Array;

   function Equals(Array1 : in Ancestor_Array; Array2 : in Ancestor_Array) return boolean;
   
   function Count_Ancestors(Tree: in T_Arbre_Personnes; Id_Node : in String) return Integer;

   function Nodes_With_Two_Parents(Tree : in T_Arbre_Personnes) return Ancestor_Array;

   function Nodes_With_Only_One_Parent(Tree : in T_Arbre_Personnes) return Ancestor_Array;

   function Has_Two_Parents(Tree : in T_Arbre_Personnes) return boolean;

   function Has_Only_One_Parent(Tree : in T_Arbre_Personnes) return boolean;

   function Get_Tree_Depth(Tree: in T_Arbre_Personnes) return Integer;


end Arbre_Genealog;
