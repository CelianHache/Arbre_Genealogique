with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Arbre_Bin;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;

procedure tests is

   procedure test_arbre_minimal is
      Family_Root : T_Arbre_Personnes;
      Personne : T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Create_Family_Tree(Family_Root, Personne);
      pragma Assert(not Is_Null (Family_Root));
      pragma Assert(Is_Null (Get_Left(Family_Root)));
      pragma Assert(Is_Null (Get_Right(Family_Root)));
      Display_Family_Tree (Family_Root);
   end test_arbre_minimal;

   procedure test_ajout_parent is 
      Family_Root : T_Arbre_Personnes;
      Antoine: T_Personne;
      Mother: T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Antoine, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Initialise(Mother, "Mamam", "Gouzy", "Femme", Ada.Calendar.Time_Of(1973, 5, 15), "Libourne");
      Create_Family_Tree(Family_Root, Antoine);
      pragma Assert(not Is_Null (Family_Root));
      Add_Mother (Family_Root, Mother);
      pragma Assert(Is_Null (Get_Father(Family_Root)));
      pragma Assert(not Is_Null (Get_Mother(Family_Root)));
      Add_Father (Family_Root, Mother, "02");
      pragma Assert(not Is_Null (Get_Node_By_Id(Family_Root, "021")));
      Display_Family_Tree (Family_Root);
   end test_ajout_parent;

begin

   test_arbre_minimal;
   test_ajout_parent;

end tests;