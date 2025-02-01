with Ada.Text_IO; 
with Ada.Integer_Text_IO; 
with Ada.Calendar; use Ada.Calendar;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with system.assertions;

procedure Main is
   procedure Show_Menu is
   begin
   -- R0 - Display menu
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
   -- R0 - Get user input
      -- R1 - Display the prompt
      Ada.Text_IO.Put(Prompt);
      -- R1 - Retreive input
      Ada.Integer_Text_IO.Get(Input);
      -- R1 - Clean the buffer
      Ada.Text_IO.Skip_Line;
      -- R1 - Return input
      return Input;
   exception
      when others =>
      -- R1 - Handle errors
         -- R2 - Display error
         Ada.Text_IO.Put_Line("Incorrect input. Please retry.");
         Ada.Text_IO.Skip_Line;
         -- R2 - Restart procedure
         return Get_User_Input(Prompt);
   end Get_User_Input;

   procedure Get_String_Input(Prompt : String; Input: in out Unbounded_String) is
   begin
   -- R0 - Get string input 
      -- R1 - Display prompt
      Ada.Text_IO.Put(Prompt);
      -- R1 - Retreive user input
      Input := Ada.Strings.Unbounded.Text_IO.Get_Line;
   exception
      -- R1 - Handle errors
      when others =>
         -- R2 - Display error
         Ada.Text_IO.Put_Line("Incorrect input. Please retry.");
         -- R2 - Restart procedure
         Get_String_Input(Prompt, Input); 
   end Get_String_Input;

   procedure Create_Personne(Personne: out T_Personne) is
      Name: Unbounded_String;
      First_Name: Unbounded_String;
      Gender: Unbounded_String;
      Sex: String(1..5);
   begin 
   -- R0 - Create Person
      -- R1 - Retreive user input for name
      Get_String_Input ("Enter name : ", Name);
      -- R1 - Retreive user input for first name
      Get_String_Input ("Enter first name : ", First_Name);
      -- R1 - Retreive user input for gender
      Get_String_Input ("Enter gender (M/F) : ", Gender);
      -- R1 - Handle errors in gender input
      while Gender /= "M" and Gender /= "F" loop
         -- R2 - Restart gender input
         Get_String_Input ("Error !!! Enter gender (M/F) : ", Gender) ;
      end loop;
      -- R1 - Assign sex based on gender
      if Gender = "M" then
         Sex := "Homme";
      else
         Sex := "Femme";
      end if;
      -- R1 -Create new Person
      Initialise (Personne, To_String(First_Name), To_String(Name), Sex, new Time'(Time_Of(2000, 1, 2)), "Paris");
   end Create_Personne;

   procedure Create_Minimal_Tree(Tree: out T_Arbre_Personnes) is
      Personne: T_Personne;
   begin 
   -- R0 - Create Minimal Tree
      -- R1 - Create Person
      Create_Personne (Personne);
      -- R1 - Initialise Family Tree
      Create_Family_Tree (Tree, Personne);
      -- R1 - Display creation success
      Ada.Text_IO.Put_Line("Minimal tree created");
      -- R1 - Display created Tree
      Display_Family_Tree (Tree);
   end Create_Minimal_Tree; 

   procedure Add_Parent(Tree: in out T_Arbre_Personnes) is
      Parent: T_Personne;
      Parent_Type: Unbounded_String;
      Id: Unbounded_String;
   begin
   -- R0 - Add a parent
      -- R1 - Retreive the id of the child
      Get_String_Input ("Enter the ID of the node you want to give a parent : ", Id);
      -- R1 - Retreive the desired parent (Mother/Father)
      Get_String_Input ("Which parent (Mother/Father) : ", Parent_Type);
      -- R1 - Handle input errors
      while Parent_Type /= "Mother" and Parent_Type /= "Father" loop
         -- R2 - Restart input
         Get_String_Input ("Error !!! Which parent (Mother/Father) : ", Parent_Type);
      end loop;
      -- R1 - Create Person
      Create_Personne (Parent);
      -- R1 - Add the choosen parent
      if Parent_Type = "Mother" then
         -- R2 - Add mother
         Add_Mother (Tree , Parent, To_String(Id));
      else
         -- R2 - Add mother
         Add_Father (Tree , Parent, To_String(Id));
      end if;
      -- R1 - Display family tree
      Display_Family_Tree (Tree);
   exception
      when system.assertions.assert_failure =>
      -- R1 - Handle errors
         -- R2 - Display error
         Ada.Text_IO.Put_Line("Error when adding a parent. Uninitialized tree or child node not existing. Please retry.");
      when others =>
      -- R1 - Handle errors
         -- R2 - Display error
         Ada.Text_IO.Put_Line("Procedure error. Please retry.");
   end Add_Parent;

   procedure Get_Number_Ancestors(Tree : in out T_Arbre_Personnes) is
      Id: Unbounded_String;
      Number_of_ancestors : Integer;
   begin
   -- R0 - Retreive the number of ancestors of a family member
      -- R1 - Retreive the id of the family member
      Get_String_Input ("Enter the ID of the node : ", Id);
      -- R1 - Count the number of ancestors
      Number_of_ancestors := Count_Ancestors (Tree, To_String(Id));
      -- R1 - Display the family tree of the family member
      Display_Family_Tree_From_Node (Tree, To_String(Id));
      -- R1 - Display the number of ancestors
      Ada.Text_IO.Put_Line("Number of ancestors : " & Integer'Image(Number_of_ancestors));
   end Get_Number_Ancestors;

   procedure Get_All_Ancestors(Tree : in out T_Arbre_Personnes; Generation : in Integer) is
      Ancestors : Ancestor_Array(1..2**Generation);
   begin
   -- R0 - Get all ancestors of a specified generation for a family member
      -- R1 - Retreive the ancestors
      Ancestors := Get_Ancestors_Generation (Tree, Generation);
      -- R1 - Browse the ancestors
      for I in Ancestors'Range loop
         -- R2 - Display each ancestor
         Display (Ancestors(I));
      end loop;

   end Get_All_Ancestors;

   procedure Display_Tree(Tree : in T_Arbre_Personnes) is
      Id: Unbounded_String;
   begin
   -- R0 - Display the tree of a family member 
      -- R1 - Retreive the id of the family member
      Get_String_Input ("Enter the ID of the node : ", Id);
      -- R1 - Display the family tree based on the Id 
      Display_Family_Tree_From_Node (Tree, To_String(Id));
   end Display_Tree;

   procedure Delete_A_Node(Tree : in out T_Arbre_Personnes) is 
      Id : Unbounded_String;
   begin
   -- R0 - Delete a node and all the ancestors
      -- R1 - Retreive the id of the family member
      Get_String_Input ("Enter the ID of the node : ", Id);
      -- R1 - Remove the family member and his ancestors
      Remove_Family_Member (Tree, To_String(Id));
      -- R1 - Display the updated family tree
      Display_Family_Tree (Tree);
   end Delete_A_Node;

   procedure Get_Single_Parent(Tree: in T_Arbre_Personnes) is
      Nodes : Ancestor_Array(1..2**(Get_Tree_Depth (Tree)-1));
   begin
   -- R0 - Get family members with only one parent 
      -- R1 - Retreive the family members with only one parent
      Nodes := Nodes_With_Only_One_Parent (Tree);
      -- R1 - Browse the family members
      for I in Nodes'Range loop
         -- R2 - Display each family member
         Display (Nodes(i));
      end loop;
   end Get_Single_Parent;

   procedure Get_Both_Parents(Tree: in T_Arbre_Personnes) is
      Nodes : Ancestor_Array(1..2**(Get_Tree_Depth (Tree)-1));
   begin
   -- R0 - Get family members with both parents
      -- R1 - Retreive family members with both parents
      Nodes := Nodes_With_Two_Parents (Tree);
      -- R1 - Browse the family members
      for I in Nodes'Range loop
         -- R2 - Display the family members
         Display (Nodes(i));
      end loop;
   end Get_Both_Parents;

   procedure Get_Orphans(Tree: in T_Arbre_Personnes) is
      Nodes : Ancestor_Array(1..2**(Get_Tree_Depth (Tree)-1));
   begin
   -- R0 - Get all orphans in the family
      -- R1 - Retreive the orphans
      Nodes := Nodes_Without_Parent (Tree);
      -- R1 - Browse the orphans
      for I in Nodes'Range loop
         -- R2 - Display each orphan
         Display (Nodes(i));
      end loop;
   end Get_Orphans;


   Main_Tree : T_Arbre_Personnes;
   Generation : Integer;

begin
-- R0 - Handle an interactive menu to permit to user user to manage a family tree
   loop 
   -- R1 - Display the menu
      Show_Menu;
   -- R1 - Get user input
      declare
         Input : Integer := Get_User_Input ("Enter selection : ");
      begin
         case Input is
            when 1 =>
            -- R2 - Create a minimal tree without any parent
               Create_Minimal_Tree (Main_Tree);
            when 2 => 
            -- R2 - Add a father or a mother to a family member
               Add_Parent(Main_Tree);
            when 3 =>
            -- R2 - Retreive the number of ancestors of a family member 
               Get_Number_Ancestors(Main_Tree);
            when 4 => 
            -- R2 - Retreive all ancestors of a family member for desired generation
               Generation := Get_User_Input ("Enter the number of the desired generation : ");
               Get_All_Ancestors (Main_Tree, Generation);
            when 5 => 
            -- R2 - Display the family tree from a specified family member
               Display_Tree(Main_Tree);
            when 6 =>
            -- R2 - Delete a family member and all his ancestors
               Delete_A_Node(Main_Tree);
            when 7 => 
            -- R2 - Retreive family members who only have one parent
               Get_Single_Parent(Main_Tree);
            when 8 => 
            -- R2 - Retreive family members who have two parents
               Get_Both_Parents(Main_Tree);
            when 9 => 
            -- R2 - Retreive all orphans
               Get_Orphans(Main_Tree);
            when 10 => 
            -- R2 - Stop the execution
               Ada.Text_IO.Put_Line("Execution stopped !");
               exit;
            when others => 
            -- R2 - Handle bad input
               Ada.Text_IO.Put_Line("Invalid input, please retry !");
         end case;
      end;

   end loop;
end Main;
