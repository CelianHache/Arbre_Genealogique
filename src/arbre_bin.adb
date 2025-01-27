with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Deallocation;

package body Arbre_Bin is

   procedure Free_String is new Ada.Unchecked_Deallocation(String, String_Access);
   procedure Free is new Ada.Unchecked_Deallocation(T_Noeud, T_Arbre);

   procedure Initialise(Tree : in out T_Arbre; Id : in String;  Value : in Element_Type; Left_Title : in String; Right_Title : in String) is
   begin
      Tree := new T_Noeud'(
         Id => new String'(Id), 
         Value => Value, 
         Right => null, 
         Left => null, 
         Right_title => new String'(Right_title),
         Left_title => new String'(Left_title));
   end Initialise;

   procedure Add_Left (Tree : in out T_Arbre; Id : in String; Value : in Element_Type) is 
   begin 
      Initialise(Tree.Left, Id, Value, Tree.Left_Title.all, Tree.Right_title.all);
   end Add_Left;

   procedure Add_Right (Tree : in out T_Arbre; Id : in String; Value : in Element_Type) is 
   begin 
      Initialise(Tree.Right, Id, Value,Tree.Left_Title.all, Tree.Right_title.all);
   end Add_Right;

   procedure Remove_Left (Tree : in out T_Arbre) is
   begin
   -- R0 - Remove left node of a tree
      if not Is_Null(Tree.Left) then
         Remove (Tree.Left);
      end if;
   end Remove_Left;

   procedure Remove_Right (Tree : in out T_Arbre) is 
   begin 
   -- R0 - Remove right node of a tree
      if not Is_Null(Tree.Right) then
         Remove (Tree.Right);
      end if;
   end Remove_Right;

   procedure Remove (Tree : in out T_Arbre) is 
   begin 
   -- R0 - Remove a tree
      -- R1 - Remove recursively right node of the tree
      Remove_Right (Tree);
      -- R1 - Remove recursively left node of the tree
      Remove_Left (Tree);

      -- R1 - Free memory of each variable
      Free_String(Tree.Id);
      Free_String(Tree.Right_title);
      Free_String(Tree.Left_title);
      Free_Element(Tree.Value);

      -- R1 - Free memory for the node itself
      Free(Tree);
   end Remove;

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

   function Get_Id (Tree : in T_Arbre) return String is 
   begin 
      if not Is_Null (Tree) then
         return Tree.Id.all;
      else 
         return "0";
      end if;    
   end Get_Id;

   function Get_Left_Title (Tree : in T_Arbre) return String is 
   begin 
      return Tree.Left_title.all;   
   end Get_Left_Title;


   function Get_Right_Title (Tree : in T_Arbre) return String is 
   begin 
      return Tree.Right_title.all;   
   end Get_Right_Title;

   function Is_Null (Tree : in T_Arbre) return Boolean is
   begin 
      return Tree = null;
   end Is_Null; 

   procedure Display(Tree : in T_Arbre) is
      procedure Display_Space(Tree : in T_Arbre; Space: Integer; name: String) is
      begin
         -- R0 - Display with indentation the differents generations
         Put_Line("");
            -- R1 - Put some indentation in tune with generation level
            for I in 0 .. Space loop
               Put("  ");
            end loop;
            -- R1 - Put the node and his children
            if not Is_Null (Tree) then
               Put("-- " & name & " : " & Tree.Id.all & " > " & To_String (Tree.Value));
               -- R2 - Display left side of the tree
               Display_Space (Tree.Left, Space + 1, Tree.Left_title.all);
               -- R2 - Display right side of the tree
               Display_Space (Tree.Right, Space + 1, Tree.Right_title.all);
            else 
               Put("-- " & name & " : Empty");
         end if;
      end Display_Space;
   begin
   -- R0 - Display tree on console 
      -- R1 - Display tree from the root 
      Display_Space (Tree, 0, "root");
      Put_Line("");
      Put_Line("");

   end Display;

end Arbre_Bin;