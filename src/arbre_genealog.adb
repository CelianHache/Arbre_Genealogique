with Ada.Text_IO; use Ada.Text_IO;
with Arbre_Genealog;
with Personne; use Personne;
with Ada.Calendar; use Ada.Calendar;

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

   function Get_Ancestors_Generation (Tree : in T_Arbre_Personnes; Generation : Integer) return Ancestor_Array is
      Ancestors : Ancestor_Array(1 .. 2**(Generation));           -- Taille initiale
      Mother_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale
      Father_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale
      Count : Integer := 0;
   begin
      if Generation = 0 then 
         if (not Is_Null (Tree)) then 
            Ancestors(1) := Get_Value(Tree);
         end if;
         return Ancestors;
      else 
         Mother_Ancestors := Get_Ancestors_Generation (Get_Mother(Tree), Generation -1);
         Father_Ancestors := Get_Ancestors_Generation (Get_Father(Tree), Generation -1);
         for i in Mother_Ancestors'Range loop 
            Ancestors(i) := Mother_Ancestors(i);
            Count := Count + 1;
         end loop;
         for i in Father_Ancestors'Range loop
            Ancestors(Count + i) := Father_Ancestors(i);
         end loop;
         return Ancestors;
      end if;
   end Get_Ancestors_Generation;

   function Get_Sorted_Ancestor_Array(List : in Ancestor_Array) return Ancestor_Array is
      Temp : T_Personne;
      New_Array : Ancestor_Array(List'Range);
   begin
      New_Array := List;
      for I in New_Array'First .. New_Array'Last - 1 loop
         for J in I + 1 .. New_Array'Last loop
            -- Comparer d'abord les noms, puis les prénoms, en déréférant les pointeurs
            if (Get_Name(New_Array(I)).all > Get_Name(New_Array(J)).all) or
               (Get_Name(New_Array(I)).all = Get_Name(New_Array(J)).all and then Get_First_Name(New_Array(I)).all > Get_First_Name(New_Array(J)).all) then
               -- Échanger les éléments si la condition est vraie
               Temp := New_Array(I);
               New_Array(I) := New_Array(J);
               New_Array(J) := Temp;
               end if;
         end loop;
      end loop;
      return New_Array;
   end Get_Sorted_Ancestor_Array;

   function Equals(Array1 : in Ancestor_Array; Array2 : in Ancestor_Array) return boolean is
      Sorted_Array_1 : Ancestor_Array(Array1'Range);
      Sorted_Array_2 : Ancestor_Array(Array2'Range);
   begin
      Sorted_Array_1 := Get_Sorted_Ancestor_Array(Array1);
      Sorted_Array_2 := Get_Sorted_Ancestor_Array(Array2);
      for i in Sorted_Array_1'Range loop
         if Sorted_Array_1(i) /= Sorted_Array_2(i) 
            and Get_Name(Sorted_Array_1(i)).all /= "" 
            and Get_Name (Sorted_Array_2(i)).all /= ""
            and Get_First_Name(Sorted_Array_1(i)).all /= "" 
            and Get_First_Name (Sorted_Array_2(i)).all /= ""
         then
            return false;
         end if;
      end loop;
      return true;
   end Equals;

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

   function Get_Tree_Depth(Tree: in T_Arbre_Personnes) return Integer is
   begin
      if Is_Null (Tree) then
         return 0;
      end if;
      return 1 + Integer'Max(Get_Tree_Depth(Get_Father(Tree)), Get_Tree_Depth(Get_Mother(Tree)));
   end Get_Tree_Depth;

   function Has_Two_Parents(Tree : in T_Arbre_Personnes) return boolean is
      Father : T_Arbre_Personnes;
      Mother : T_Arbre_Personnes;
   begin
      Father := Get_Father (Tree);
      Mother := Get_Mother (Tree);
      return not Is_Null (Father) and not Is_Null (Mother);
   end Has_Two_Parents;

   function Has_Only_One_Parent(Tree : in T_Arbre_Personnes) return boolean is
      Father : T_Arbre_Personnes;
      Mother : T_Arbre_Personnes;
   begin
      Father := Get_Father (Tree);
      Mother := Get_Mother (Tree);
      return (not Is_Null (Father) and Is_Null (Mother)) or (Is_Null (Father) and not Is_Null (Mother));
   end Has_Only_One_Parent;

   function Is_Orphan(Tree : in T_Arbre_Personnes) return boolean is
      Father : T_Arbre_Personnes;
      Mother : T_Arbre_Personnes;
   begin
      Father := Get_Father (Tree);
      Mother := Get_Mother (Tree);
      return Is_Null (Father) and Is_Null (Mother);
   end Is_Orphan;

   function Nodes_With_Two_Parents(Tree : in T_Arbre_Personnes) return Ancestor_Array is
      function Nodes_With_Two_Parents_Generation(Tree : in T_Arbre_Personnes; Depth : Integer) return Ancestor_Array is
         Ancestors : Ancestor_Array(1 .. 2**(Depth-1));           -- Taille initiale
         Mother_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father : T_Arbre_Personnes;
         Mother : T_Arbre_Personnes;
         Count : Integer := 0;
         Padding : Integer := 0;
      begin 
         if not Is_Null (Tree) then
            if Has_Two_Parents (Tree) then
               Ancestors(1) := Get_Value(Tree);
               Padding := 1;
            end if;
            Mother := Get_Mother(Tree);
            Father := Get_Father(Tree);
            if not Is_Null (Mother) then 
               Mother_Ancestors := Nodes_With_Two_Parents_Generation(Mother, Depth-1);
               for i in Mother_Ancestors'Range loop 
                  Ancestors(i + Padding) := Mother_Ancestors(i);
                  Count := Count + 1;
               end loop;
            end if;
            if not Is_Null (Father) then
               Father_Ancestors := Nodes_With_Two_Parents_Generation(Father, Depth-1);
               for i in Father_Ancestors'Range loop
                  Ancestors(Count + i) := Father_Ancestors(i);
               end loop;
            end if;
         end if;
         return Ancestors;
      end Nodes_With_Two_Parents_Generation;
   begin
      return Nodes_With_Two_Parents_Generation (Tree, Get_Tree_Depth(Tree));
   end Nodes_With_Two_Parents;

   function Nodes_With_Only_One_Parent(Tree : in T_Arbre_Personnes) return Ancestor_Array is
      function Nodes_With_Only_One_Parent_Generation(Tree : in T_Arbre_Personnes; Depth : Integer) return Ancestor_Array is
         Ancestors : Ancestor_Array(1 .. 2**(Depth-1));           -- Taille initiale
         Mother_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father : T_Arbre_Personnes;
         Mother : T_Arbre_Personnes;
      begin 
         if not Is_Null (Tree) then
            if Has_Only_One_Parent (Tree) then
               Ancestors(1) := Get_Value(Tree);
            end if;
            Mother := Get_Mother(Tree);
            Father := Get_Father(Tree);
            if not Is_Null (Mother) then 
               Mother_Ancestors := Nodes_With_Only_One_Parent_Generation(Mother, Depth-1);
               for i in Mother_Ancestors'Range loop 
                  Ancestors(i + 1) := Mother_Ancestors(i);
               end loop;
            else
               Father_Ancestors := Nodes_With_Only_One_Parent_Generation(Father, Depth-1);
               for i in Father_Ancestors'Range loop
                  Ancestors(i + 1) := Father_Ancestors(i);
               end loop;
            end if;
         end if;
         return Ancestors;
      end Nodes_With_Only_One_Parent_Generation;
   begin
      return Nodes_With_Only_One_Parent_Generation (Tree, Get_Tree_Depth(Tree));
   end Nodes_With_Only_One_Parent;

   function Nodes_Without_Parent(Tree : in T_Arbre_Personnes) return Ancestor_Array is
      function Nodes_Without_Parent_Generation(Tree : in T_Arbre_Personnes; Depth : Integer) return Ancestor_Array is
         Ancestors : Ancestor_Array(1 .. 2**(Depth-1));           -- Taille initiale
         Mother_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father_Ancestors : Ancestor_Array(1 .. 2**(Depth-2));  -- Taille initiale
         Father : T_Arbre_Personnes;
         Mother : T_Arbre_Personnes;
         Count : Integer := 0;
      begin 
         if not Is_Null (Tree) then
            if Is_Orphan (Tree) then
               Ancestors(1) := Get_Value(Tree);
            else
               Mother := Get_Mother(Tree);
               Father := Get_Father(Tree);
               if not Is_Null (Mother) then 
                  Mother_Ancestors := Nodes_Without_Parent_Generation(Mother, Depth-1);
                  for i in Mother_Ancestors'Range loop 
                     Ancestors(i) := Mother_Ancestors(i);
                     Count := Count + 1;
                  end loop;
               end if;
               if not Is_Null (Father) then
                  Father_Ancestors := Nodes_Without_Parent_Generation(Father, Depth-1);
                  for i in Father_Ancestors'Range loop
                     Ancestors(Count + i) := Father_Ancestors(i);
                  end loop;
               end if;
            end if;
         end if;
         return Ancestors;
      end Nodes_Without_Parent_Generation;
   begin
      return Nodes_Without_Parent_Generation (Tree, Get_Tree_Depth(Tree));
   end Nodes_Without_Parent;

end Arbre_Genealog;