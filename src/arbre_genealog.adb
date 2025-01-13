with Ada.Text_IO; use Ada.Text_IO;
with Personne; use Personne;

package body Arbre_Genealog is

   procedure Create_Family_Tree(Root : in out T_Arbre; Root_ID : Integer; Root_Value : T_Personne) is
   begin
      Initialise(Root, Root_ID, Root_Value);
   end Create_Family_Tree;

   procedure Display_Family_Tree(Tree : in T_Arbre) is
   begin
      Display(Tree);
   end Display_Family_Tree;

end Arbre_Genealog;