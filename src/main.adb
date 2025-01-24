with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Main is
   procedure Show_Menu is
   begin
      Put_Line("");
      Put_Line("=== Menu ===");
      Put_Line("");
      Put_Line("1. Create a minimal tree with a single root node");
      Put_Line("2. Add a parent to a given node");
      Put_Line("3. Get the number of ancestors of a given node");
      Put_Line("4. Get all the ancestors from a specified generation");
      Put_Line("5. Display tree from a given node");
      Put_Line("6. Delete a node and its ancestors");
      Put_Line("7. Get all individuals with a single parent");
      Put_Line("8. Get all individuals with two parents");
      Put_Line("9. Get all individuals without parent");
      Put_Line("10. Exit");
      Put_Line("");
   end Show_Menu;

   function Get_User_Input(Prompt : String) return Integer is
      Input : Integer;
   begin
      Put(Prompt);
      Get(Input);
      Skip_Line;
      return Input;
   exception
      when others =>
         Put_Line("Entrée invalide. Veuillez réessayer.");
         return Get_User_Input(Prompt);
   end Get_User_Input;

   procedure Get_String_Input(Prompt : String; Input: in out String) is
      Last: Natural;
   begin
      Put(Prompt);
      Get_Line(Input, Last); -- Lecture de l'entrée utilisateur   
      if Last < 100 then
         Input(Last + 1 .. Input'Last) := (others => ' ');
      end if;
   exception
      when others =>
         Put_Line("Entrée invalide. Veuillez réessayer.");
         Skip_Line;
         Get_String_Input(Prompt, Input); -- Redemande en cas d'erreur
   end Get_String_Input;

   procedure Create_Personne(Personne: out T_Personne) is
      Name: String(1..15);
      First_Name: String(1..15);
      Gender: String(1..1);
      Sex: String(1..5);
   begin 
      Get_String_Input ("Enter name : ", Name);
      Get_String_Input ("Enter first name : ", First_Name);
      Get_String_Input ("Enter gender (M/F) : ", Gender);
      while Gender /= "M" and Gender /= "F" loop
         Skip_Line;
         Get_String_Input ("Error !!! Enter gender (M/F) : ", Gender) ;
      end loop;
      if Gender = "M" then
         Sex := "Homme";
      else
         Sex := "Femme";
      end if;
      Initialise (Personne, First_Name, Name, Sex, new Time'(Time_Of(2000, 1, 2)), "Paris");
   end Create_Personne;

   procedure Create_Minimal_Tree(Tree: out T_Arbre_Personnes) is
      Personne: T_Personne;
   begin 
      Create_Personne (Personne);
      Create_Family_Tree (Tree, Personne);
      Put_Line("Minimal tree created");
      Display_Family_Tree (Tree);
   end Create_Minimal_Tree; 

   procedure Add_Parent(Tree: in out T_Arbre_Personnes) is
      Parent: T_Personne;
      Parent_Type: String(1..6);
      Id: String(1..15);
   begin
      Get_String_Input ("Enter the ID of the node you want to give a parent : ", Id);
      Get_String_Input ("Which parent (Mother/Father) : ", Parent_Type);
      Skip_Line;
      while Parent_Type /= "Mother" and Parent_Type /= "Father" loop
         Skip_Line;
         Get_String_Input ("Error !!! Which parent (Mother/Father) : ", Parent_Type);
      end loop;
      Create_Personne (Parent);
      if Parent_Type = "Mother" then
         Add_Mother (Tree , Parent, Id);
      else
         Add_Father (Tree , Parent, Id);
      end if;
      Display_Family_Tree (Tree);
   end Add_Parent;


   Main_Tree : T_Arbre_Personnes;

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
            when 10 => 
               Put_Line("Execution stopped !");
               exit;
            when others => 
               Put_Line("Invalid input, please retry !");
         end case;
      end;

   end loop;
end Main;
