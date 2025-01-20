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

   procedure test_ancestors_generation is       
      Family_Root : T_Arbre_Personnes;
      P1: T_Personne;
      P2: T_Personne;
      P3: T_Personne;
      P4: T_Personne;
      P5: T_Personne;
      P6: T_Personne;
      P7: T_Personne;
      P8: T_Personne;
      P9: T_Personne;
      P10: T_Personne;
      P11: T_Personne;
      P12: T_Personne;
      P13: T_Personne;
      P14: T_Personne;
      P15: T_Personne;
      P16: T_Personne;
      P17: T_Personne;
      P18: T_Personne;
      P19: T_Personne;
      P20: T_Personne;
      Ancestors_List_0 : Ancestor_Array(1 .. 1); 
      Expected_Ancestors_List_0 : Ancestor_Array(1 .. 1); 
      Ancestors_List_1 : Ancestor_Array(1 .. 2); 
      Expected_Ancestors_List_1 : Ancestor_Array(1 .. 2); 
      Ancestors_List_2 : Ancestor_Array(1 .. 4); 
      Expected_Ancestors_List_2 : Ancestor_Array(1 .. 4); 
      Ancestors_List_3 : Ancestor_Array(1 .. 8); 
      Expected_Ancestors_List_3 : Ancestor_Array(1 .. 8); 
   begin
      pragma Assert(Is_Null(Family_Root));    
      Initialise(P1, "Antoine", "Gouzy", "Homme", Time_Of(1980, 4, 14), "Libourne");
      Initialise(P2, "Marie", "Gouzy", "Femme", Time_Of(1985, 6, 21), "Libourne");
      Initialise(P3, "Jean", "Gouzy", "Homme", Time_Of(2005, 2, 15), "Libourne");
      Initialise(P4, "Sophie", "Gouzy", "Femme", Time_Of(2007, 8, 25), "Libourne");
      Initialise(P5, "Luc", "Gouzy", "Homme", Time_Of(2009, 1, 30), "Libourne");
      Initialise(P6, "Claire", "Gouzy", "Femme", Time_Of(2012, 3, 10), "Libourne");
      Initialise(P7, "David", "Gouzy", "Homme", Time_Of(2015, 5, 22), "Libourne");
      Initialise(P8, "Alice", "Gouzy", "Femme", Time_Of(1987, 11, 5), "Libourne");
      Initialise(P9, "Philippe", "Gouzy", "Homme", Time_Of(1990, 3, 30), "Libourne");
      Initialise(P10, "Julien", "Gouzy", "Homme", Time_Of(1993, 7, 14), "Libourne");
      Initialise(P11, "Martine", "Gouzy", "Femme", Time_Of(1996, 12, 17), "Libourne");
      Initialise(P12, "Nicolas", "Gouzy", "Homme", Time_Of(1999, 4, 21), "Libourne");
      Initialise(P13, "Isabelle", "Gouzy", "Femme", Time_Of(2002, 9, 6), "Libourne");
      Initialise(P14, "Thomas", "Gouzy", "Homme", Time_Of(2004, 10, 9), "Libourne");
      Initialise(P15, "Emilie", "Gouzy", "Femme", Time_Of(2006, 2, 28), "Libourne");
      Initialise(P16, "Vincent", "Gouzy", "Homme", Time_Of(2008, 5, 17), "Libourne");
      Initialise(P17, "Camille", "Gouzy", "Femme", Time_Of(2010, 8, 23), "Libourne");
      Initialise(P18, "Louis", "Gouzy", "Homme", Time_Of(2013, 1, 10), "Libourne");
      Initialise(P19, "Elise", "Gouzy", "Femme", Time_Of(2016, 6, 15), "Libourne");
      Initialise(P20, "Pierre", "Gouzy", "Homme", Time_Of(2018, 4, 2), "Libourne");

      Create_Family_Tree(Family_Root, P1);
      Add_Mother (Family_Root, P2);
      Add_Father (Family_Root, P3);
      Add_Father (Family_Root, P4,     "01");
      Add_Mother (Family_Root, P5,     "01");
      Add_Father (Family_Root, P6,     "02");
      Add_Mother (Family_Root, P7,     "02");
      Add_Father (Family_Root, P8,     "011");
      Add_Mother (Family_Root, P9,     "011");
      Add_Father (Family_Root, P10,    "012");
      Add_Mother (Family_Root, P11,    "012");
      Add_Father (Family_Root, P12,    "0111");
      Add_Mother (Family_Root, P13,    "0111");
      Add_Father (Family_Root, P14,    "0112");
      Add_Mother (Family_Root, P15,    "0112");
      Add_Father (Family_Root, P16,    "01111");
      Add_Mother (Family_Root, P17,    "01111");
      Add_Father (Family_Root, P18,    "01112");
      Add_Mother (Family_Root, P19,    "01112");
      Add_Father (Family_Root, P20,    "0121");
      Ancestors_List_0 := Get_Ancestors_Generation (Family_Root, 0);
      Expected_Ancestors_List_0(1) := P1;
      pragma Assert(Expected_Ancestors_List_0 = Ancestors_List_0); 
      Ancestors_List_1 := Get_Ancestors_Generation (Family_Root, 1);
      Expected_Ancestors_List_1(1) := P2;
      Expected_Ancestors_List_1(2) := P3;
      pragma Assert(Expected_Ancestors_List_1 = Ancestors_List_1); 
      Ancestors_List_2 := Get_Ancestors_Generation (Family_Root, 2);
      Expected_Ancestors_List_2(1) := P4;
      Expected_Ancestors_List_2(2) := P5;
      Expected_Ancestors_List_2(3) := P6;
      Expected_Ancestors_List_2(4) := P7;
      pragma Assert(Equals(Expected_Ancestors_List_2, Ancestors_List_2)); 
      Ancestors_List_3 := Get_Ancestors_Generation (Family_Root, 3); 
      Expected_Ancestors_List_3(1) := P8;
      Expected_Ancestors_List_3(2) := P9;
      Expected_Ancestors_List_3(3) := P10;
      Expected_Ancestors_List_3(4) := P11;
      pragma Assert(Equals(Expected_Ancestors_List_3, Ancestors_List_3));  
      Put_Line("Test_ancestors_generation OK!");    
   end test_ancestors_generation;

begin

   test_arbre_minimal;
   test_ajout_parent;
   test_ancestors_generation;

end tests;