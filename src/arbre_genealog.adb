with Ada.Text_IO; use Ada.Text_IO;
with Arbre_Genealog;
with Personne; use Personne;

package body Arbre_Genealog is

   function Identite_Chaine(Cle : String) return String is
   begin
      return Cle;
   end Identite_Chaine;

   procedure Create_Family_Tree(Root : in out T_Arbre_Personnes; Root_Value : T_Personne) is
   begin
      Initialise(Root, "0", Root_Value, "Père", "Mère");
   end Create_Family_Tree;

   procedure Display_Family_Tree(Tree : in T_Arbre_Personnes) is
   begin
      Display(Tree);
   end Display_Family_Tree;

   function Get_Father (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes is
   begin 
      return Get_Left (Tree);
   end Get_Father;

   function Get_Mother (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes is
   begin
      return Get_Right (Tree);
   end Get_Mother;

   --  procedure Add_Father (Tree : in out T_Arbre_Personnes; Value : in T_Personne) is 
   --     Father: T_Arbre_Personnes;
   --  begin
   --     Father := Get_Father(Tree);
   --     Initialise(Father, Get_Id(Tree) & "1", Value, Get_Left_Title(Tree), Get_Right_Title(Tree));
   --  end Add_Father;

   --  procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value : in T_Personne) is 
   --     Mother: T_Arbre_Personnes;
   --  begin 
   --     Mother := Get_Mother(Tree);
   --     Initialise(Mother, Get_Id(Tree) & "2", Value, Get_Left_Title(Tree), Get_Right_Title(Tree));
   --  end Add_Mother;

end Arbre_Genealog;