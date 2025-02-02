with Ada.Text_IO; use Ada.Text_IO;
with Arbre_Genealog;
with GNAT.Sockets;
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


   procedure Display_Family_Tree_From_Node(Tree : in T_Arbre_Personnes; Id_Node: String) is
      Tree_From_Node : T_Arbre_Personnes;
   begin 
      Tree_From_Node := Get_Node_By_Id(Tree, Id_Node);
      Display_Family_Tree (Tree_From_Node);
   end Display_Family_Tree_From_Node;

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
      Base_Id: String := Get_Id(Tree);
      Path: String := Id_Child(Base_Id'Length + 1 .. Id_Child'Last);
   begin
   -- R0 - Get a node based on its ID
      -- R1 - Retrieve the direct ancestor
      if Id_Child(Id_Child'First) /= '0' then
         raise Invalid_Node_Id;
      end if;
      Temp_Tree := Tree;
      for i in Path'Range loop
         -- R2 -Check the digits of the searched id
         if Path(i) = '1' then
            -- R3 - Replace current tree by the father's tree
            Temp_Tree := Get_Father(Temp_Tree);
         elsif Path(i) = '2' then
            -- R3 - Replace current tree by the mother's tree
            Temp_Tree := Get_Mother(Temp_Tree);
         else 
            -- R3 - Filled in id contains characters impossible in our use case
            raise Invalid_Node_Id;
         end if;
      end loop;
      -- R1 - Return the tree
      return Temp_Tree;
   end Get_Node_By_Id;

   function Get_Ancestors_Generation (Tree : in T_Arbre_Personnes; Generation : Integer) return Ancestor_Array is
      Ancestors : Ancestor_Array(1 .. 2**(Generation));           -- Taille initiale
      Mother_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale
      Father_Ancestors : Ancestor_Array(1 .. 2**(Generation-1));  -- Taille initiale
      Count : Integer := 0;
   begin
   -- R0 - Retrieve ancestors of a specified generation
      -- R1 - Check the generation 
      if Generation = 0 then 
         -- R2 - Return the value of the current node
         if (not Is_Null (Tree)) then 
            Ancestors(1) := Get_Value(Tree);
         end if;
         return Ancestors;
      else 
         -- R2 - Get ancestors from previous generations
         Mother_Ancestors := Get_Ancestors_Generation (Get_Mother(Tree), Generation -1);
         Father_Ancestors := Get_Ancestors_Generation (Get_Father(Tree), Generation -1);
         -- R2 - Copy previous ancestors with the current ones
         for i in Mother_Ancestors'Range loop 
            Ancestors(i) := Mother_Ancestors(i);
            Count := Count + 1;
         end loop;
         for i in Father_Ancestors'Range loop
            Ancestors(Count + i) := Father_Ancestors(i);
         end loop;
         -- R2 - Return ancestors
         return Ancestors;
      end if;
   end Get_Ancestors_Generation;

   function Get_Sorted_Ancestor_Array(List : in Ancestor_Array) return Ancestor_Array is
      Temp : T_Personne;
      New_Array : Ancestor_Array(List'Range);
   begin
   -- R0 - Sort the ancestors array
      -- R1 - Copy the list
      New_Array := List;
      -- R1 - Sorted the copied list
      for I in New_Array'First .. New_Array'Last - 1 loop
         for J in I + 1 .. New_Array'Last loop
            -- R2 - Compare names and first names
            if (Get_Name(New_Array(I)).all > Get_Name(New_Array(J)).all) or
               (Get_Name(New_Array(I)).all = Get_Name(New_Array(J)).all and then Get_First_Name(New_Array(I)).all > Get_First_Name(New_Array(J)).all) then
               -- R3 - Echange values
               Temp := New_Array(I);
               New_Array(I) := New_Array(J);
               New_Array(J) := Temp;
               end if;
         end loop;
      end loop;
      -- R1 - Return array
      return New_Array;
   end Get_Sorted_Ancestor_Array;

   function Equals(Array1 : in Ancestor_Array; Array2 : in Ancestor_Array) return boolean is
      Sorted_Array_1 : Ancestor_Array(Array1'Range);
      Sorted_Array_2 : Ancestor_Array(Array2'Range);
   begin
   -- R0 - Eval if the contents of two lists are equals
      -- R1 - Sort the lists
      Sorted_Array_1 := Get_Sorted_Ancestor_Array(Array1);
      Sorted_Array_2 := Get_Sorted_Ancestor_Array(Array2);
      -- R1 - Compare the lists
      for i in Sorted_Array_1'Range loop
         -- R2 - Check the equality
         if Sorted_Array_1(i) /= Sorted_Array_2(i) 
            and Get_Name(Sorted_Array_1(i)).all /= "" 
            and Get_Name (Sorted_Array_2(i)).all /= ""
            and Get_First_Name(Sorted_Array_1(i)).all /= "" 
            and Get_First_Name (Sorted_Array_2(i)).all /= ""
         then
         -- R2 - Returns false when the lists are different
            return false;
         end if;
      end loop;
      -- R1 - Return true if the lists are equals
      return true;
   end Equals;

   function Count_Ancestors(Tree: in T_Arbre_Personnes; Id_Node : in String) return Integer is
      Root, Left, Right : T_Arbre_Personnes;
      Ancestors_Left, Ancestors_Right : Integer;
   begin
   -- R0 - Count the ancestors
      -- R1 - Retrieve root
      Root := Get_Node_By_Id(Tree, Id_Node);
      -- R1 - Return 0 if root is null
      if Is_Null(Root) then
         return 0;
      end if;
      -- R1 - Retrieve left parent
      Left := Get_Left(Root);
      -- R1 - Retrieve right parent
      Right := Get_Right(Root);
      -- R1 - Count number of ancestors for the parents
         -- R2 - Initialise the count 
      Ancestors_Left := 0;
      Ancestors_Right := 0;
         -- R2 - Count the ancestors for the left parent
      if not Is_Null(Left) then
         Ancestors_Left := Count_Ancestors(Tree, Get_Id(Left));
      end if;
         -- R2 - Count the ancestors for the right parent
      if not Is_Null(Right) then
         Ancestors_Right := Count_Ancestors(Tree, Get_Id(Right));
      end if;

      -- R1 - Return the sum of the ancestors
      return 1 + Ancestors_Left + Ancestors_Right;
   end Count_Ancestors;

   procedure Remove_Family_Member(Tree : in out T_Arbre_Personnes; Id_Node : in String) is
      Root, Child : T_Arbre_Personnes;
      Value : T_Personne;
   begin
   -- R0 - Remove a family member and all its ancestors
      if (Id_Node'Length >= 2) then
         -- R1 - Retrieve the node
         Root := Get_Node_By_Id(Tree, Id_Node);
         -- R1 - Retrieve the child of the node
         Child := Get_Child(Tree, Id_Node);
         -- R1 - Remove the parent
         if Is_Father(Id_Node) then
            -- R2 - Remove the father 
            Remove_Father(Child);
         else
            -- R2 - Remove the mother
            Remove_Mother(Child);
         end if;
      else 
         -- R1 - Free the node and all the values in T_Personne
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
      -- R0 - Get the nodes with both parents
         -- R1 - Check for ancestors
         if not Is_Null (Tree) then
            -- R2 - Add the ancestor if it has two parents
            if Has_Two_Parents (Tree) then
               Ancestors(1) := Get_Value(Tree);
               Padding := 1;
            end if;
            -- R2 - Get the ancestors of the mother and the father
            Mother := Get_Mother(Tree);
            Father := Get_Father(Tree);
            if not Is_Null (Mother) then 
               -- R3 - Get the node with both parents in the mother tree 
               Mother_Ancestors := Nodes_With_Two_Parents_Generation(Mother, Depth-1);
               for i in Mother_Ancestors'Range loop 
                  Ancestors(i + Padding) := Mother_Ancestors(i);
                  Count := Count + 1;
               end loop;
            end if;
            if not Is_Null (Father) then
               -- R3 - Get the node with both parents in the father tree
               Father_Ancestors := Nodes_With_Two_Parents_Generation(Father, Depth-1);
               for i in Father_Ancestors'Range loop
                  Ancestors(Count + i) := Father_Ancestors(i);
               end loop;
            end if;
         end if;
         -- R1 - Return the ancestors
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
      -- R0 - Get the nodes with only one parent
         -- R1 - Check for ancestors
         if not Is_Null (Tree) then
            -- R2 - Add the ancestor if it has only one parent
            if Has_Only_One_Parent (Tree) then
               Ancestors(1) := Get_Value(Tree);
            end if;
            -- R2 - Get the ancestors of the mother and the father
            Mother := Get_Mother(Tree);
            Father := Get_Father(Tree);
            if not Is_Null (Mother) then 
               -- R3 - Get the node with only one parent in the mother tree 
               Mother_Ancestors := Nodes_With_Only_One_Parent_Generation(Mother, Depth-1);
               for i in Mother_Ancestors'Range loop 
                  Ancestors(i + 1) := Mother_Ancestors(i);
               end loop;
            else
               -- R3 - Get the node with only one parent in the father tree 
               Father_Ancestors := Nodes_With_Only_One_Parent_Generation(Father, Depth-1);
               for i in Father_Ancestors'Range loop
                  Ancestors(i + 1) := Father_Ancestors(i);
               end loop;
            end if;
         end if;
         -- R1 - Return the ancestors
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
      -- R0 - Get the orphans
         -- R1 - Check for ancestors
         if not Is_Null (Tree) then
            -- R2 - Add the ancestor if it is an orphan
            if Is_Orphan (Tree) then
               Ancestors(1) := Get_Value(Tree);
            else
            -- R2 - Get the ancestors of the mother and the father
               Mother := Get_Mother(Tree);
               Father := Get_Father(Tree);
               -- R3 - Get the orphan nodes in the mother tree 
               if not Is_Null (Mother) then 
                  Mother_Ancestors := Nodes_Without_Parent_Generation(Mother, Depth-1);
                  for i in Mother_Ancestors'Range loop 
                     Ancestors(i) := Mother_Ancestors(i);
                     Count := Count + 1;
                  end loop;
               end if;
               -- R3 - Get the orphan nodes in the father tree 
               if not Is_Null (Father) then
                  Father_Ancestors := Nodes_Without_Parent_Generation(Father, Depth-1);
                  for i in Father_Ancestors'Range loop
                     Ancestors(Count + i) := Father_Ancestors(i);
                  end loop;
               end if;
            end if;
         end if;
         -- R1 - Return the ancestors
         return Ancestors;
      end Nodes_Without_Parent_Generation;
   begin
      return Nodes_Without_Parent_Generation (Tree, Get_Tree_Depth(Tree));
   end Nodes_Without_Parent;

end Arbre_Genealog;