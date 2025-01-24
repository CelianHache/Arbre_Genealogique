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


procedure Remove_Family_Member(Tree : in out T_Arbre_Personnes; Id_Node : in String) is
   Root, Child : T_Arbre_Personnes;
   Value : T_Personne;
begin
   -- Supprimer la référence du noeud par l'enfant si différent de la racine
   if (Id_Node'Length >= 2) then
      Root := Get_Node_By_Id(Tree, Id_Node);
      Child := Get_Child(Tree, Id_Node);
      if Is_Father(Id_Node) then
         Remove_Father(Child);
      else
         Remove_Mother(Child);
      end if;
   else 
      -- Libérer les champs dynamiques dans la valeur `T_Personne` du nœud
      Remove(Tree);
   end if;

end Remove_Family_Member;

procedure Remove_Father(Child: in out T_Arbre_Personnes) is
   Father_Personne : T_Personne;
begin
   Remove_Left(Child);
end Remove_Father;

procedure Remove_Mother(Child: in out T_Arbre_Personnes) is
begin
   Remove_Right(Child);
end Remove_Mother;

function Get_Child(Tree : in T_Arbre_Personnes; Id_Node : in String) return T_Arbre_Personnes is
   Child: T_Arbre_Personnes;
   Id_Child : String(1..Id_Node'Last-1); 
begin 
   Id_Child := Id_Node(Id_Node'First..Id_Node'Last - 1);
   Child := Get_Node_By_Id(Tree, Id_Child); 
   return Child;
end Get_Child;

function Is_Father(Id_Node : in String) return Boolean is
   Last_Char: Character;
begin
   Last_Char := Id_Node(Id_Node'Last);

   if Last_Char = '1' then
      return True;
   end if;

   return False;
end Is_Father;

end Arbre_Genealog;