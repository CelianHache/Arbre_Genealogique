with Ada.Text_IO; use Ada.Text_IO;

package body Arbre_Bin is

   procedure Initialise(Tree : in out T_Arbre; Id : in Id_Type;  Value : in Element_Type) is
   begin
      Tree := new T_Noeud'(Id => Id, Value => Value, Right => null, Left => null);
   end Initialise;

   procedure Add_Left (Tree : in out T_Arbre; Id : in Id_Type; Value : in Element_Type) is 
   begin 
      Arbre_Bin.Initialise(Tree.Left, Id, Value);
   end Add_Left;

   procedure Add_Right (Tree : in out T_Arbre; Id : in Id_Type; Value : in Element_Type) is 
   begin 
      Arbre_Bin.Initialise(Tree.Right, Id, Value);
   end Add_Right;

   procedure Remove_Left (Tree : in out T_Arbre) is
   begin
      Tree.Left := null;
   end Remove_Left;

   procedure Remove_Right (Tree : in out T_Arbre) is 
   begin 
      Tree.Right := null;
   end Remove_Right;

   function Get_Left (Tree : in T_Arbre) return T_Arbre is
   begin 
      return Tree.Left;
   end Get_Left;

   function Get_Right (Tree : in T_Arbre) return T_Arbre is
   begin
      return Tree.Right;
   end Get_Right;

   function Get_Value (Tree : in T_Arbre) return Element_Type is
   begin
      return Tree.Value;
   end Get_Value;

   function Get_Id (Tree : in T_Arbre) return Id_Type is 
   begin 
      return Tree.Id;   
   end Get_Id;

   function Is_Null (Tree : in T_Arbre) return Boolean is
   begin 
      return Tree = null;
   end Is_Null; 

   -- Afficher un nœud
   procedure Display(Tree : in T_Arbre) is
      procedure Display_Space(Tree : in T_Arbre; Space: Integer; name: String) is
      begin
         Put_Line("");
            for I in 0 .. Space loop
               Put("  ");
            end loop;
            if not Is_Null (Tree) then
               Put("-- " & name & " : " & To_String (Tree.Id) & " > " & To_String (Tree.Value));
               Display_Space (Tree.Left, Space + 1, "left");
               Display_Space (Tree.Right, Space + 1, "right");
            else 
               Put("-- " & name & " : Empty");
         end if;
      end Display_Space;
   begin
      Display_Space (Tree, 0, "root");
      Put_Line("");
      Put_Line("");

   end Display;

end Arbre_Bin;