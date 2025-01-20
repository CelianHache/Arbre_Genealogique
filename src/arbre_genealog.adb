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
      Ancestors : Ancestor_Array(1 .. 2**(Generation));           -- Taille initiale, peut être ajustée
      Mother_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale, peut être ajustée
      Father_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale, peut être ajustée
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
            --  Put_Line("Comparing " & Get_First_Name(New_Array(I)).all & " with " & Get_First_Name(New_Array(J)).all);
            -- Comparer d'abord les noms, puis les prénoms, en déréférant les pointeurs
            if (Get_Name(New_Array(I)).all > Get_Name(New_Array(J)).all) or
               (Get_Name(New_Array(I)).all = Get_Name(New_Array(J)).all and then Get_First_Name(New_Array(I)).all > Get_First_Name(New_Array(J)).all) then
               -- Échanger les éléments si la condition est vraie
               Temp := New_Array(I);
               New_Array(I) := New_Array(J);
               New_Array(J) := Temp;
               --  Put_Line("Swapped " & Get_First_Name(New_Array(I)).all & " with " & Get_First_Name(New_Array(J)).all);
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

end Arbre_Genealog;