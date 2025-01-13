with Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;

procedure tests is

   procedure test_arbre_minimal is
      Family_Tree : Arbre_Genealog.T_Arbre;
      Personne : T_Personne;
   begin
      pragma Assert(Is_Null(Family_Tree)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Create_Family_Tree(Family_Tree, 1, Personne);
      pragma Assert(not Is_Null (Family_Tree));
      pragma Assert(Is_Null (Get_Left(Family_Tree)));
      pragma Assert(Is_Null (Get_Right(Family_Tree)));
   end test_arbre_minimal;

begin

   test_arbre_minimal;

end tests;


