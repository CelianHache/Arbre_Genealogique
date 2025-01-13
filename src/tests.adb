with Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Arbre_Bin;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;

procedure tests is

   procedure test_arbre_minimal is
      Family_Tree : T_Arbre_Personnes;
      Personne : T_Personne;
   begin
      pragma Assert(Is_Null(Family_Tree)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Create_Family_Tree(Family_Tree, Personne);
      pragma Assert(not Is_Null (Family_Tree));
      pragma Assert(Is_Null (Get_Left(Family_Tree)));
      pragma Assert(Is_Null (Get_Right(Family_Tree)));
      Display_Family_Tree (Family_Tree);
   end test_arbre_minimal;

   procedure test_ajout_parent is 
      Family_Tree : T_Arbre_Personnes;
      Personne: T_Personne;
      Parent: T_Personne;
      Mother: T_Arbre_Personnes;
   begin
      pragma Assert(Is_Null(Family_Tree)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Initialise(Parent, "Mamam", "Gouzy", "Femme", Ada.Calendar.Time_Of(1973, 5, 15), "Libourne");
      Create_Family_Tree(Family_Tree, Personne);
      Add_Right (Family_Tree, "02", Parent);
      Mother := Get_Mother(Family_Tree);
      pragma Assert(not Is_Null (Family_Tree));
      pragma Assert(Is_Null (Get_Father(Family_Tree)));
      pragma Assert(not Is_Null (Get_Mother(Family_Tree)));
      pragma Assert(Is_Null (Get_Father(Mother)));
      pragma Assert(Is_Null (Get_Mother(Mother)));
      Display_Family_Tree (Family_Tree);
   end test_ajout_parent;

begin

   test_arbre_minimal;
   test_ajout_parent;

end tests;