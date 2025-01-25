with Ada.Text_IO; 
with Ada.Integer_Text_IO; 
with Ada.Calendar; use Ada.Calendar;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; 

procedure Main is
   procedure Show_Menu is
   begin
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("=== Menu ===");
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("1. Create a minimal tree with a single root node");
      Ada.Text_IO.Put_Line("2. Add a parent to a given node");
      Ada.Text_IO.Put_Line("3. Get the number of ancestors of a given node");
      Ada.Text_IO.Put_Line("4. Get all the ancestors from a specified generation");
      Ada.Text_IO.Put_Line("5. Display tree from a given node");
      Ada.Text_IO.Put_Line("6. Delete a node and its ancestors");
      Ada.Text_IO.Put_Line("7. Get all individuals with a single parent");
      Ada.Text_IO.Put_Line("8. Get all individuals with two parents");
      Ada.Text_IO.Put_Line("9. Get all individuals without parent");
      Ada.Text_IO.Put_Line("10. Exit");
      Ada.Text_IO.Put_Line("");
   end Show_Menu;

   function Get_User_Input(Prompt : String) return Integer is
      Input : Integer;
   begin
      Ada.Text_IO.Put(Prompt);
      Ada.Integer_Text_IO.Get(Input);
      Ada.Text_IO.Skip_Line;
      return Input;
   exception
      when others =>
         Ada.Text_IO.Put_Line("Entrée invalide. Veuillez réessayer.");
         return Get_User_Input(Prompt);
   end Get_User_Input;

   procedure Get_String_Input(Prompt : String; Input: in out Unbounded_String) is
   begin
      Ada.Text_IO.Put(Prompt);
      Input := Ada.Strings.Unbounded.Text_IO.Get_Line;
   exception
      when others =>
         Ada.Text_IO.Put_Line("Entrée invalide. Veuillez réessayer.");
         Ada.Text_IO.Skip_Line;
         Get_String_Input(Prompt, Input); -- Redemande en cas d'erreur
   end Get_String_Input;

   procedure Create_Personne(Personne: out T_Personne) is
      Name: Unbounded_String;
      First_Name: Unbounded_String;
      Gender: Unbounded_String;
      Sex: Unbounded_String;
   begin 
      Get_String_Input ("Enter name : ", Name);
      Get_String_Input ("Enter first name : ", First_Name);
      Get_String_Input ("Enter gender (M/F) : ", Gender);
      while Gender /= "M" and Gender /= "F" loop
         Ada.Text_IO.Skip_Line;
         Get_String_Input ("Error !!! Enter gender (M/F) : ", Gender) ;
      end loop;
      if Gender = "M" then
         Sex := Ada.Strings.Unbounded.To_Unbounded_String("Homme");
      else
         Sex := Ada.Strings.unbounded.To_Unbounded_String("Femme");
      end if;
      Initialise (Personne, To_String(First_Name), To_String(Name), To_String(Sex), new Time'(Time_Of(2000, 1, 2)), "Paris");
   end Create_Personne;

   procedure Create_Minimal_Tree(Tree: out T_Arbre_Personnes) is
      Personne: T_Personne;
   begin 
      Create_Personne (Personne);
      Create_Family_Tree (Tree, Personne);
      Ada.Text_IO.Put_Line("Minimal tree created");
      Display_Family_Tree (Tree);
   end Create_Minimal_Tree; 

   procedure Add_Parent(Tree: in out T_Arbre_Personnes) is
      Parent: T_Personne;
      Parent_Type: Unbounded_String;
      Id: Unbounded_String;
   begin
      Get_String_Input ("Enter the ID of the node you want to give a parent : ", Id);
      Get_String_Input ("Which parent (Mother/Father) : ", Parent_Type);
      Ada.Text_IO.Skip_Line;
      while Parent_Type /= "Mother" and Parent_Type /= "Father" loop
         Ada.Text_IO.Skip_Line;
         Get_String_Input ("Error !!! Which parent (Mother/Father) : ", Parent_Type);
      end loop;
      Create_Personne (Parent);
      if Parent_Type = "Mother" then
         Add_Mother (Tree , Parent, To_String(Id));
      else
         Add_Father (Tree , Parent, To_String(Id));
      end if;
      Display_Family_Tree (Tree);
   end Add_Parent;

   procedure Get_Number_Ancestors(Tree : T_Arbre_Personnes) is
      Id: Unbounded_String;
      Number_of_ancestors : Integer;
   begin
      Get_String_Input ("Enter the ID of the node : ", Id);
      Number_of_ancestors := Count_Ancestors (Tree, To_String(Id));
      Display_Family_Tree_From_Node (Tree, To_String(Id));
      Ada.Text_IO.Put_Line("Number of ancestors : " & Integer'Image(Number_of_ancestors));
   end Get_Number_Ancestors;

   procedure Get_All_Ancestors(Tree : T_Arbre_Personnes; Generation : Integer) is
      Ancestors : Ancestor_Array(1..2**Generation);
   begin
      Ancestors := Get_Ancestors_Generation (Tree, Generation);
      for I in Ancestors'Range loop
         Display (Ancestors(I));
      end loop;

   end Get_All_Ancestors;


   Main_Tree : T_Arbre_Personnes;
   Generation : Integer;

begin
   loop 
      Show_Menu;
      declare
         Input : Integer := Get_User_Input ("Enter selection : ");
      begin
         case Input is
            when 1 =>
               Create_Minimal_Tree (Main_Tree);
            when 2 => 
               Add_Parent(Main_Tree);
            when 3 =>
               Get_Number_Ancestors(Main_Tree);
            when 4 => 
               Generation := Get_User_Input ("Enter the number of the desired generation : ");
               Get_All_Ancestors (Main_Tree, Generation);
            when 10 => 
               Ada.Text_IO.Put_Line("Execution stopped !");
               exit;
            when others => 
               Ada.Text_IO.Put_Line("Invalid input, please retry !");
         end case;
      end;

   end loop;
end Main;
