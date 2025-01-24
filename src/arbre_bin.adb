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
      if not Is_Null(Tree.Left) then
         -- Libération récursive du sous-arbre gauche
         Remove_Left(Tree.Left);
         Remove_Right(Tree.Left);

         -- Libérer la mémoire associée aux champs internes
         Free_String(Tree.Left.Id);
         Free_String(Tree.Left.Right_title);
         Free_String(Tree.Left.Left_title);
         Free_Element (Tree.Left.Value);

         -- Libérer la mémoire du sous-arbre gauche lui-même
         Free(Tree.Left);
      end if;
   end Remove_Left;

   procedure Remove_Right (Tree : in out T_Arbre) is 
   begin 
      if not Is_Null(Tree.Right) then
         -- Libération récursive du sous-arbre droit
         Remove_Left(Tree.Right);
         Remove_Right(Tree.Right);

         -- Libérer la mémoire associée aux champs internes
         Free_String(Tree.Right.Id);
         Free_String(Tree.Right.Right_title);
         Free_String(Tree.Right.Left_title);
         Free_Element (Tree.Right.Value);
         
         -- Libérer la mémoire du sous-arbre droit lui-même
         Free(Tree.Right);
      end if;
   end Remove_Right;

   procedure Remove (Tree : in out T_Arbre) is 
   begin 
      -- Libération récursive des sous-arbres
      Remove_Right (Tree);
      Remove_Left (Tree);

      -- Libérer la mémoire associée aux champs internes
      Free_String(Tree.Id);
      Free_String(Tree.Right_title);
      Free_String(Tree.Left_title);
      Free_Element (Tree.Value);

      -- Libérer la mémoire du sous-arbre lui-même
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
      return Tree.Id.all;   
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

   -- Afficher un nœud
   procedure Display(Tree : in T_Arbre) is
      procedure Display_Space(Tree : in T_Arbre; Space: Integer; name: String) is
      begin
         Put_Line("");
            for I in 0 .. Space loop
               Put("  ");
            end loop;
            if not Is_Null (Tree) then
               Put("-- " & name & " : " & Tree.Id.all & " > " & To_String (Tree.Value));
               Display_Space (Tree.Left, Space + 1, Tree.Left_title.all);
               Display_Space (Tree.Right, Space + 1, Tree.Right_title.all);
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