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
      Father: T_Arbre_Personnes; 
   begin 
      Father := Get_Left(Tree); 
      return Father;
   end Get_Father;

   function Get_Mother (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes is
      Mother: T_Arbre_Personnes; 
   begin
      Mother := Get_Right(Tree); 
      return Mother;
   end Get_Mother;

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value : in T_Personne) is 
   begin
      Add_Left(Tree, Get_Id(Tree) & "1", Value);
   end Add_Father;
   

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value : in T_Personne) is 
   begin 
      Add_Right(Tree, Get_Id(Tree) & "2", Value);
   end Add_Mother;

   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value : in T_Personne; Id_Child : String) is 
      Child : T_Arbre_Personnes;
   begin
      Child := Get_Node_By_Id(Tree, Id_Child);
      Add_Left(Child, Id_Child & "1", Value);
   end Add_Father;
   

   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value : in T_Personne; Id_Child : String) is 
      Child : T_Arbre_Personnes;
   begin 
      Child := Get_Node_By_Id(Tree, Id_Child);
      Add_Right(Child, Id_Child & "2", Value);
   end Add_Mother;


   function Get_Node_By_Id (Tree : in T_Arbre_Personnes; Id_Child : String) return T_Arbre_Personnes is
      Temp_Tree: T_Arbre_Personnes;
   begin
      Temp_Tree := Tree;
      for i in Id_Child'Range loop
         if Id_Child(i) = '1' then
            Temp_Tree := Get_Father(Temp_Tree);
         elsif Id_Child(i) = '2' then
            Temp_Tree := Get_Mother(Temp_Tree);
         end if;
      end loop;
      return Temp_Tree;
   end Get_Node_By_Id;

function Count_Ancestors(Tree: in T_Arbre_Personnes; Id_Node : in String) return Integer is
   Root, Left, Right : T_Arbre_Personnes;
   Ancestors_Left, Ancestors_Right : Integer;
begin

   Root := Get_Node_By_Id(Tree, Id_Node);

   if Is_Null(Root) then
      return 0;
   end if;

   Left := Get_Left(Root);
   Right := Get_Right(Root);

   Ancestors_Left := 0;
   Ancestors_Right := 0;

   if not Is_Null(Left) then
      Ancestors_Left := Count_Ancestors(Tree, Get_Id(Left));
   end if;

   if not Is_Null(Right) then
      Ancestors_Right := Count_Ancestors(Tree, Get_Id(Right));
   end if;

   -- Retourner la somme des ancêtres, en incluant le nœud courant
   return 1 + Ancestors_Left + Ancestors_Right;
end Count_Ancestors;

end Arbre_Genealog;